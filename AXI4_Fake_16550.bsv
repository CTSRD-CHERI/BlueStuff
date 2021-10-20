/*-
 * Copyright (c) 2021 Alexandre Joannou
 * All rights reserved.
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory (Department of Computer Science and
 * Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
 * DARPA SSITH research programme.
 *
 * This material is based upon work supported by the DoD Information Analysis
 * Center Program Management Office (DoD IAC PMO), sponsored by the Defense
 * Technical Information Center (DTIC) under Contract No. FA807518D0004.  Any
 * opinions, findings and conclusions or recommendations expressed in this
 * material are those of the author(s) and do not necessarily reflect the views
 * of the Air Force Installation Contracting Agency (AFICA).
 *
 * @BERI_LICENSE_HEADER_START@
 *
 * Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
 * license agreements.  See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.  BERI licenses this
 * file to you under the BERI Hardware-Software License, Version 1.0 (the
 * "License"); you may not use this file except in compliance with the
 * License.  You may obtain a copy of the License at:
 *
 *   http://www.beri-open-systems.org/legal/license-1-0.txt
 *
 * Unless required by applicable law or agreed to in writing, Work distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations under the License.
 *
 * @BERI_LICENSE_HEADER_END@
 */

package AXI4_Fake_16550;

import AXI4Lite :: *;
import AXI4Stream :: *;
import Connectable :: *;
import SourceSink :: *;

// This package aims to provides a simple component exposing a subset of the
// 16550 register map on 2 AXI4Lite slave interfaces. Each can be memory
// mapped in AXI4Lite masters' address spaces to allow for simple communication.

// The mkAXI4_Fake_16550 module presents a (not complete) 16550 interface
// through an AXI4 lite slave, and exposes two AXI4 stream interfaces for the
// rx and tx channels as well as an interrupt line.
// Note that the registers are byte wide registers, and have been remapped to
// be 4 bytes appart to be accessible easily on a 32-bit wide AXI4Lite bus.
module mkAXI4_Fake_16550 (
  Tuple4 #( AXI4Lite_Slave #( addr_, data_
                            , awuser_, wuser_, buser_, aruser_, ruser_)
          , AXI4Stream_Master #(txId, txData, txDest, txUser)
          , AXI4Stream_Slave #(rxId, rxData, rxDest, rxUser)
          , ReadOnly #(Bool) ))
  provisos ( Add#(_a, txData, data_)
           , Add#(_b, rxData, data_)
           , Add#(_c, 8, data_)
           , Add#(_d, 4, addr_) );

  // transaction buffers
  AXI4Stream_Shim #(txId, txData, txDest, txUser) txShim <- mkAXI4StreamShimFF;
  AXI4Stream_Shim #(rxId, rxData, rxDest, rxUser) rxShim <- mkAXI4StreamShimFF;
  let axiShim <- mkAXI4LiteShimFF;

  // internal state / wires
  Reg #(Bit #(8))   regIER <- mkReg (8'b00000000);
  Reg #(Bit #(8))   regSCR <- mkRegU;
  Reg #(Bit #(8))   regLCR <- mkReg (8'b00000000);
  Reg #(Bit #(8))   regDLR_LSB <- mkRegU;
  Reg #(Bit #(8))   regDLR_MSB <- mkRegU;
  Wire #(Bit #(8)) wireIIR <- mkDWire (8'b00000000);
  PulseWire       pulseIrq <- mkPulseWire;

  // rx handling
  Wire #(Bit #(rxData)) rxData <- mkDWire (?);
  (* fire_when_enabled *)
  rule rx_read_data; rxData <= rxShim.master.peek.tdata; endrule
  PulseWire rxDropData <- mkPulseWire;
  (* fire_when_enabled *)
  rule rx_drop_data; rxShim.master.drop; endrule

  // tx handling
  Wire #(Bit #(txData)) wireTxData <- mkWire;
  (* fire_when_enabled *)
  rule tx_write_data;
    txShim.slave.put (AXI4Stream_Flit { tdata: wireTxData
                                      , tstrb: ~0
                                      , tkeep: ~0
                                      , tlast: True
                                      , tid: ?
                                      , tdest: ?
                                      , tuser: ? });
  endrule

  // irq handling
  (* fire_when_enabled, no_implicit_conditions *)
  rule updt_irq (unpack (regIER[0]) && rxShim.master.canPeek);
    pulseIrq.send;
    wireIIR <= 8'b00000101;
  endrule

  // read requests handling
  rule read_req;
    let r <- get (axiShim.master.ar);
    let rsp = AXI4Lite_RFlit { rdata: 0
                             , rresp: OKAY
                             , ruser: ? };
    case (r.araddr[4:2])
      3'h0: begin
        if (regLCR[0] == 0) begin // RBR: Receiver Buffer Register
          rsp.rdata = zeroExtend (rxData);
          if (rxShim.master.canPeek) rxDropData.send;
        end else // DLR (LSB): Divisor Latch Register (LSB)
          rsp.rdata = zeroExtend (regDLR_LSB);
      end
      3'h1: begin
        if (regLCR[0] == 0) // IER: Interrupt Enable Register
          rsp.rdata = zeroExtend (regIER);
        else // DLR (MSB): Divisor Latch Register (MSB)
          rsp.rdata = zeroExtend (regDLR_MSB);
      end
      3'h2: // IIR: Interrupt Identification Register
        rsp.rdata = zeroExtend (wireIIR);
      3'h3: // LCR: LINE CONTROL REGISTER
        rsp.rdata = zeroExtend (regLCR);
      3'h5: // LSR: Line Statur Register
        rsp.rdata = zeroExtend ({ 2'b00
                                , pack (txShim.slave.canPut)
                                , 4'b0000
                                , pack (rxShim.master.canPeek) });
      3'h7: // SCR: Scratch Register
        rsp.rdata = zeroExtend (regSCR);
      default:
        //rsp.rresp = SLVERR;
        noAction;
    endcase
    axiShim.master.r.put (rsp);
  endrule

  // write requests handling
  rule write_req;
    let aw <- get (axiShim.master.aw);
    let w <- get (axiShim.master.w);
    let rsp = AXI4Lite_BFlit { bresp: OKAY
                             , buser: ? };
    if (w.wstrb[0] == 1'b1) case (aw.awaddr[4:2])
      3'h0: begin
        if (regLCR[0] == 0) // THR: Transmitter Holding Register
          wireTxData <= truncate (w.wdata);
        else // DLR (LSB): Divisor Latch Register (LSB)
          regDLR_LSB <= truncate (w.wdata);
      end
      3'h1: begin
        if (regLCR[0] == 0) // IER: Interrupt Enable Register
          regIER <= truncate (w.wdata);
        else // DLR (MSB): Divisor Latch Register (MSB)
          regDLR_MSB <= truncate (w.wdata);
      end
      3'h3: // LCR: LINE CONTROL REGISTER
        regLCR <= truncate (w.wdata);
      3'h7: // SCR: Scratch Register
        regSCR <= truncate (w.wdata);
      default:
        //rsp.bresp = SLVERR;
        noAction;
    endcase
    axiShim.master.b.put (rsp);
  endrule

  // export the AXI4 lite 16550 interfaces as well as the tx and rx streams
  return tuple4 ( axiShim.slave
                , txShim.master
                , rxShim.slave
                , pulseWireToReadOnly (pulseIrq) );
endmodule

// The mkAXI4_Fake_16550_Pair module instantiates two mkAXI4_Fake_16550
// modules and connects their rx and tx streams, and returns the two remaining
// AXI4 lite slave interfaces together with their irq line
module mkAXI4_Fake_16550_Pair (
  Tuple2 #( Tuple2 #( AXI4Lite_Slave #( addr0_, data0_
                                      , awuser0_, wuser0_, buser0_
                                      , aruser0_, ruser0_)
                    , ReadOnly #(Bool) )
          , Tuple2 #( AXI4Lite_Slave #( addr1_, data1_
                                      , awuser1_, wuser1_, buser1_
                                      , aruser1_, ruser1_)
                    , ReadOnly #(Bool) )))
  provisos ( Add #(_a, 4, addr0_)
           , Add #(_b, 4, addr1_)
           , Add #(_c, 8, data_)
           , Add #(0, data0_, data1_)
           , NumAlias #(data0_, data_) );
  Tuple4 #( AXI4Lite_Slave #( addr0_, data0_
                            , awuser0_, wuser0_, buser0_, aruser0_, ruser0_)
          , AXI4Stream_Master #(0, data_, 0, 0)
          , AXI4Stream_Slave #(0, data_, 0, 0)
          , ReadOnly #(Bool) )
    half0 <- mkAXI4_Fake_16550;
  match {.slv0, .tx0, .rx0, .irq0} = half0;
  Tuple4 #( AXI4Lite_Slave #( addr1_, data1_
                            , awuser1_, wuser1_, buser1_, aruser1_, ruser1_)
          , AXI4Stream_Master #(0, data_, 0, 0)
          , AXI4Stream_Slave #(0, data_, 0, 0)
          , ReadOnly #(Bool) )
    half1 <- mkAXI4_Fake_16550;
  match {.slv1, .tx1, .rx1, .irq1} = half1;
  mkConnection (tx0, rx1);
  mkConnection (tx1, rx0);
  return tuple2 (tuple2 (slv0, irq0), tuple2 (slv1, irq1));
endmodule

endpackage
