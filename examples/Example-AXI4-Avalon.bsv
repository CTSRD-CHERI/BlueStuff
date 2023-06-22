/*-
 * Copyright (c) 2018-2023 Alexandre Joannou
 * All rights reserved.
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory (Department of Computer Science and
 * Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
 * DARPA SSITH research programme.
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

import BlueBasics :: *;
import BlueAXI4 :: *;
import BlueAvalon :: *;
import AXI4_Avalon :: *;

import Connectable :: *;

typedef 1 NMASTERS;
typedef 1 NSLAVES;

typedef TExp#(19) SlaveWidth;

typedef 0 MID_sz;
typedef TAdd#(MID_sz, TLog#(NMASTERS)) SID_sz;
typedef TAdd#(1, TLog#(TMul#(NSLAVES, SlaveWidth))) ADDR_sz;
typedef  32 DATA_sz;
typedef   0 AWUSER_sz;
typedef   0 WUSER_sz;
typedef   0 BUSER_sz;
typedef   0 ARUSER_sz;
typedef   0 RUSER_sz;

`define PARAMS ADDR_sz, DATA_sz, AWUSER_sz, WUSER_sz, BUSER_sz, ARUSER_sz, RUSER_sz
`define MPARAMS MID_sz, `PARAMS
`define SPARAMS SID_sz, `PARAMS
`define MASTER_T AXI4_Master#(`MPARAMS)
`define SLAVE_T  AXI4_Slave#(`SPARAMS)
`define MASTER_SYNTH_T AXI4_Master_Sig#(`MPARAMS)
`define SLAVE_SYNTH_T  AXI4_Slave_Sig#(`SPARAMS)

`define DELAY 5

Integer nb_flit = 3;
Integer nb_rsp = 2;

module mkAXI4Master (`MASTER_T);

  // AXI master shim
  AXI4_Shim#(`MPARAMS) shim <- mkAXI4Shim;
  // Req addr
  Reg#(Bit#(ADDR_sz)) nextWriteAddr <- mkReg(0);
  // book keep
  Reg#(Bool) awSent <- mkReg (False);
  Reg#(Bool) reqSent <- mkReg (False);
  Reg#(Bit#(32)) rspCnt <- mkReg (0);
  Reg#(Bit#(32)) cnt <- mkReg (0);

  // arbitrary work for each channel
  rule putAXI4_AWFlit (!awSent);
    AXI4_AWFlit#(MID_sz, ADDR_sz, AWUSER_sz) f = ?;
    f.awaddr  = nextWriteAddr;
    f.awburst = INCR;
    f.awlen   = fromInteger (nb_flit - 1);
    nextWriteAddr <= nextWriteAddr + 1;
    shim.slave.aw.put(f);
    awSent <= True;
    $display("%0t - MASTER - sending ", $time, fshow(f));
  endrule
  rule putAXI4_WFlit (!reqSent);
    AXI4_WFlit#(DATA_sz, WUSER_sz) f = AXI4_WFlit{
      wdata: zeroExtend (cnt), wstrb: ?, wlast: cnt == fromInteger (nb_flit - 1), wuser: ?
    };
    shim.slave.w.put(f);
    if (cnt == fromInteger (nb_flit - 1)) begin
      cnt <= 0;
      reqSent <= True;
    end else cnt <= cnt + 1;
    $display("%0t - MASTER - sending ", $time, fshow(f));
  endrule
  rule getAXI4_BFlit (awSent && reqSent);
    let rsp <- get(shim.slave.b);
    $display("%0t - MASTER - received ", $time, fshow(rsp));
    reqSent <= False;
    awSent <= False;
    if (rspCnt == fromInteger (nb_rsp - 1)) $finish(0);
    else rspCnt <= rspCnt + 1;
  endrule

  // return AXI interface
  return shim.master;

endmodule

module mkPipelinedAvalonMMAgent (PipelinedAvalonMMAgent #(ADDR_sz, DATA_sz));
  NumProxy #(2) depthProxy = ?;
  let {srcReq, snkRsp, ifc} <- toPipelinedAvalonMMAgent (depthProxy);

  // artificial dealy in the agent
  let delay <- mkReg (`DELAY);
  rule dec_delay (delay > 0); delay <= delay - 1; endrule

  rule handleReq (delay == 0);
    let req = srcReq.peek;
    AvalonMMResponse #(DATA_sz) rsp = case (req.operation) matches
      tagged Read: AvalonMMResponse {
        response: 2'h00
      , operation: tagged Read zeroExtend (req.address)
      };
      tagged Write .*: AvalonMMResponse { response: 2'h00
                                        , operation: tagged Write };
    endcase;
    $display ( "<%0t> AGENT> received: ", $time, fshow (req)
             , "\n       sending: ", fshow (rsp) );
    srcReq.drop;
    snkRsp.put (rsp);
    delay <= `DELAY;
  endrule

  return ifc;
endmodule

module top (Empty);
  let axm <- mkAXI4Master;
  let avh <- mkAXI4Manager_to_PipelinedAvalonMMHost (axm);
  let ava <- mkPipelinedAvalonMMAgent;
  (* fire_when_enabled, no_implicit_conditions *)
  rule debug;
    $display ("-------------------- <t = %0t> --------------------", $time);
    $display (fshow (avh));
    $display (fshow (ava));
  endrule
  mkConnection(avh, ava);
endmodule

`undef PARAMS
`undef MPARAMS
`undef SPARAMS
`undef MASTER_T
`undef SLAVE_T
`undef DELAY
