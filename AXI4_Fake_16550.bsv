/*-
 * Copyright (c) 2021-2022 Alexandre Joannou
 * Copyright (c) 2022 Jonathan Woodruff
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

import CounterHelpers :: *;

import FIFOF :: *;

// This package aims to provides a simple component exposing a subset of the
// 16550 register map on 2 AXI4Lite slave interfaces. Each can be memory
// mapped in AXI4Lite masters' address spaces to allow for simple communication.

// The mkAXI4_Fake_16550 module presents a (not complete) 16550 interface
// through an AXI4 lite slave, and exposes two AXI4 stream interfaces for the
// rx and tx channels as well as an interrupt line.
// Note that the registers are byte wide registers, and have been remapped to
// be 4 bytes appart to be accessible easily on a 32-bit wide AXI4Lite bus.
module mkAXI4_Fake_16550 #(Integer txDepth, Integer rxDepth)
  (Tuple4 #( AXI4Lite_Slave #( addr_, data_
                             , awuser_, wuser_, buser_, aruser_, ruser_)
           , AXI4Stream_Master #(txId, txData, txDest, txUser)
           , AXI4Stream_Slave #(rxId, rxData, rxDest, rxUser)
           , ReadOnly #(Bool) ))
  provisos ( // rx irq timeout mechanism from 16550 spec
             NumAlias #(4000, t_rxTimeout)
           , NumAlias #(TLog #(t_rxTimeout), t_rxTimeoutWidth)
             // non standard rx irq rate limiter
           , NumAlias #(50000, t_rxIrqRate)
           , NumAlias #(TLog #(t_rxIrqRate), t_rxIrqRateWidth)
             // rx depth bitwidth. Note: received rxDepth must fit
           , NumAlias #(16, t_rxLvlWidth)
             // Constraints
           , Add #(_a, txData, data_)
           , Add #(_b, rxData, data_)
           , Add #(_c, 8, data_)
           , Add #(_d, 4, addr_) );

  // AXI interface
  let axiShim <- mkAXI4LiteShimFF;

  // transaction buffers
  AXI4Stream_Shim #(txId, txData, txDest, txUser)
    txShim <- mkAXI4StreamShim (mkSizedFIFOF (txDepth));
  AXI4Stream_Shim #(rxId, rxData, rxDest, rxUser)
    rxShim <- mkAXI4StreamShim (mkSizedFIFOF (rxDepth));

  // internal state / wires
  Reg #(Bit #(8)) regIER <- mkReg (8'b00000000);
  Reg #(Bit #(8)) regSCR <- mkRegU;
  Reg #(Bit #(8)) regLCR <- mkReg (8'b00000000);
  Reg #(Bit #(8)) regDLR_LSB <- mkRegU;
  Reg #(Bit #(8)) regDLR_MSB <- mkRegU;
  Reg #(Bit #(1)) regFIFOMode <- mkReg (1'b1);
  PulseWire       pulseIrq <- mkPulseWire;
  PulseWire       pulseTxIrq <- mkPulseWire;
  PulseWire       pulseRxIrq <- mkPulseWire;
  Reg #(Bool)     regTxIrqPending <- mkReg (False);

  // receive buffer fill level tracking
  /////////////////////////////////////
  TimerIfc #(t_rxTimeoutWidth) irqReceiveTimer <- mkTimer;
  // sanity check requested rxDepth
  if (valueOf (t_rxLvlWidth) >= log2 (rxDepth))
    error ( "Requeted rxDepth (" + integerToString (rxDepth)
          + ") should fit in " + integerToString (valueOf (t_rxLvlWidth))
          + "bits" );
  LevelTriggeredCounterIfc #(t_rxLvlWidth) rxLvl <- mkLevelTriggeredCounter (1);
  let rxTimeout = fromInteger (valueOf (t_rxTimeout));
  let rxShimMaster = interface AXI4Stream_Master;
    method canPeek = rxShim.master.canPeek;
    method peek = rxShim.master.peek;
    method drop = action
      rxShim.master.drop;
      rxLvl.decrement;
    endaction;
  endinterface;
  let rxShimSlave = interface AXI4Stream_Slave;
    method canPut = rxShim.slave.canPut;
    method put (x) = action
      rxShim.slave.put (x);
      rxLvl.increment;
      irqReceiveTimer.start (rxTimeout);
    endaction;
  endinterface;

  // rx handling
  //////////////
  Wire #(Bit #(rxData)) rxData <- mkDWire (?);
  (* fire_when_enabled *)
  rule rx_read_data; rxData <= rxShimMaster.peek.tdata; endrule
  PulseWire rxDropData <- mkPulseWire;
  PulseWire rxShimClear <- mkPulseWire;
  (* fire_when_enabled *)
  rule rx_clear (rxShimClear); rxShim.clear; endrule
  (* fire_when_enabled *)
  rule rx_drop_data (rxDropData); rxShimMaster.drop; endrule

  // tx handling
  //////////////
  Wire #(Bit #(txData)) wireTxData <- mkWire;
  PulseWire txShimClear <- mkPulseWire;
  (* fire_when_enabled *)
  rule tx_clear (txShimClear); txShim.clear; endrule
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

  // receive data ready irq handling
  //////////////////////////////////
  // rx irq trigger signals
  Bool irqReceiveTimeout = irqReceiveTimer.done && rxShimMaster.canPeek;
  Bool rxTrigger =
    unpack (regIER[0]) && (rxLvl.levelReached || irqReceiveTimeout);
  /**/ let rxRate = fromInteger (valueOf (t_rxIrqRate));
  /**/ let rxTriggerRising <- mkRisingEdgeDetector (rxTrigger);
  /**/ TimerIfc #(t_rxIrqRateWidth) rxIrqRateTimer <- mkTimer;
  /**/ (* fire_when_enabled, no_implicit_conditions *)
  /**/ rule restart_irq_rate_timer (rxTriggerRising);
  /**/   rxIrqRateTimer.start (rxRate);
  /**/ endrule
  (* fire_when_enabled, no_implicit_conditions *)
  rule receive_data_ready_irq (rxTrigger && /**/ rxIrqRateTimer.done);
    pulseRxIrq.send;
  endrule

  // transmitter holding register empty irq handling
  //////////////////////////////////////////////////
  // Note: we consider both the tx can put status AND the associated interrupt
  // enable bit to actually trigger the irq and prepare the irq pending on a
  // rising edge detection
  Bool txReadyIrq = txShim.slave.canPut && unpack (regIER[1]);
  let txReadyIrqRising <- mkRisingEdgeDetector (txReadyIrq);
  (* no_implicit_conditions *)
  rule set_thr_pending (txReadyIrqRising);
    regTxIrqPending <= True;
  endrule
  (* fire_when_enabled, no_implicit_conditions *)
  rule thr_empty_irq (regTxIrqPending && txReadyIrq);
    pulseTxIrq.send;
  endrule

  // exported irq line
  ////////////////////
  // pulse the exported irq line if any interrupt source is active
  (* fire_when_enabled, no_implicit_conditions *)
  rule pulse_irq_line (pulseRxIrq || pulseTxIrq);
    pulseIrq.send;
  endrule

  // read requests handling
  /////////////////////////
  rule read_req;
    let r <- get (axiShim.master.ar);
    let rsp = AXI4Lite_RFlit { rdata: 0
                             , rresp: OKAY
                             , ruser: ? };
    case (r.araddr[4:2])
      3'h0: begin
        if (regLCR[7] == 0) begin // RBR: Receiver Buffer Register
          rsp.rdata = zeroExtend (rxData);
          if (rxShimMaster.canPeek) rxDropData.send;
        end else // DLR (LSB): Divisor Latch Register (LSB)
          rsp.rdata = zeroExtend (regDLR_LSB);
        // reset rx irq timeout
        irqReceiveTimer.start (rxTimeout);
      end
      3'h1: begin
        if (regLCR[7] == 0) // IER: Interrupt Enable Register
          rsp.rdata = zeroExtend (regIER);
        else // DLR (MSB): Divisor Latch Register (MSB)
          rsp.rdata = zeroExtend (regDLR_MSB);
      end
      3'h2: begin // IIR: Interrupt Identification Register
        Bit #(8) val = { regFIFOMode == 1'b1 ? 2'b11 : 2'b00 // FIFOs enabled
                       , 2'b00  // DMA end
                       , 3'b000 // Interrupt Identification Code
                       , 1'b0   // Interrupt Status (0: interrupt pending)
                       };
        // From highest to lowest priority ...
        if (pulseRxIrq)
          val[3:1] = 3'b010;
        else if (pulseTxIrq) begin
          val[3:1] = 3'b001;
          regTxIrqPending <= False;
        end else
          val[0] = 1'b1; // no interrupt pending
        rsp.rdata = zeroExtend (val);
      end
      3'h3: // LCR: LINE CONTROL REGISTER
        rsp.rdata = zeroExtend (regLCR);
      3'h5: // LSR: Line Status Register
        rsp.rdata = zeroExtend ({ 1'b0
                                , pack (txShim.slave.canPut)
                                , pack (txShim.slave.canPut)
                                , 4'b0000
                                , pack (rxShimMaster.canPeek) });
      3'h7: // SCR: Scratch Register
        rsp.rdata = zeroExtend (regSCR);
      default:
        //rsp.rresp = SLVERR;
        noAction;
    endcase
    axiShim.master.r.put (rsp);
  endrule

  // write requests handling
  //////////////////////////
  rule write_req;
    let aw <- get (axiShim.master.aw);
    let w <- get (axiShim.master.w);
    let rsp = AXI4Lite_BFlit { bresp: OKAY
                             , buser: ? };
    if (w.wstrb[0] == 1'b1) case (aw.awaddr[4:2])
      3'h0: begin
        if (regLCR[7] == 0) begin // THR: Transmitter Holding Register
          wireTxData <= truncate (w.wdata);
          regTxIrqPending <= False;
        end else // DLR (LSB): Divisor Latch Register (LSB)
          regDLR_LSB <= truncate (w.wdata);
      end
      3'h1: begin
        if (regLCR[7] == 0) // IER: Interrupt Enable Register
          regIER <= truncate (w.wdata);
        else // DLR (MSB): Divisor Latch Register (MSB)
          regDLR_MSB <= truncate (w.wdata);
      end
      3'h2: begin // FCR: FIFO Control Register
        Bool clearRx = False;
        Bool clearTx = False;
        regFIFOMode <= w.wdata[0];
        if (w.wdata[0] != regFIFOMode) begin
          clearRx = True;
          clearTx = True;
        end
        if (w.wdata[0] == 1'b1) begin
          if (w.wdata[1] == 1'b1) clearRx = True;
          if (w.wdata[2] == 1'b1) clearTx = True;
          case (w.wdata[7:6])
            2'b00: rxLvl.setThreshold (1);
            2'b01: rxLvl.setThreshold (4);
            2'b10: rxLvl.setThreshold (8);
            2'b11: rxLvl.setThreshold (fromInteger (rxDepth));
          endcase
        end
        if (clearRx) begin
          rxShimClear.send;
          rxLvl.setLevel (0);
        end
        if (clearTx) txShimClear.send;
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
                , rxShimSlave
                , pulseWireToReadOnly (pulseIrq) );
endmodule

// The mkAXI4_Fake_16550_Pair module instantiates two mkAXI4_Fake_16550
// modules and connects their rx and tx streams, and returns the two remaining
// AXI4 lite slave interfaces together with their irq line
module mkAXI4_Fake_16550_Pair #(Integer txDepth, Integer rxDepth)
  (Tuple2 #( Tuple2 #( AXI4Lite_Slave #( addr0_, data0_
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
    half0 <- mkAXI4_Fake_16550 (txDepth, rxDepth);
  match {.slv0, .tx0, .rx0, .irq0} = half0;
  Tuple4 #( AXI4Lite_Slave #( addr1_, data1_
                            , awuser1_, wuser1_, buser1_, aruser1_, ruser1_)
          , AXI4Stream_Master #(0, data_, 0, 0)
          , AXI4Stream_Slave #(0, data_, 0, 0)
          , ReadOnly #(Bool) )
    half1 <- mkAXI4_Fake_16550 (txDepth, rxDepth);
  match {.slv1, .tx1, .rx1, .irq1} = half1;
  mkConnection (tx0, rx1);
  mkConnection (tx1, rx0);
  return tuple2 (tuple2 (slv0, irq0), tuple2 (slv1, irq1));
endmodule

endpackage
