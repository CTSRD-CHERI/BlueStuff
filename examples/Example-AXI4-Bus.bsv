/*-
 * Copyright (c) 2018-2019 Alexandre Joannou
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

import AXI4 :: *;

import Routable :: *;
import SourceSink :: *;
import ListExtra :: *;

import Connectable :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;
import List :: *;
import Vector :: *;

typedef 1 NMASTERS;
typedef 1 NSLAVES;

typedef 4096 SlaveWidth;

typedef 0 MID_sz;
typedef TAdd#(MID_sz, TLog#(NMASTERS)) SID_sz;
typedef TAdd#(1, TLog#(TMul#(NSLAVES, SlaveWidth))) ADDR_sz;
typedef 128 DATA_sz;
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

module axiMaster (`MASTER_T);

  // AXI master shim
  AXI4_Shim#(`MPARAMS) shim <- mkAXI4Shim;
  // Req addr
  Reg#(Bit#(ADDR_sz)) nextWriteAddr <- mkReg(0);

  // counter
  Reg#(Bit#(DATA_sz)) cnt <- mkReg(0);
  rule counteUp; cnt <= cnt + 1; endrule

  // arbitrary work for each channel
  Bool sendWrite = cnt[3:0] == 0;
  rule putAXI4_AWFlit (sendWrite);
    AXI4_AWFlit#(MID_sz, ADDR_sz, AWUSER_sz) f = ?;
    f.awaddr = nextWriteAddr;
    f.awlen  = 0;
    nextWriteAddr <= nextWriteAddr + fromInteger(valueOf(SlaveWidth)) + 1;
    shim.slave.aw.put(f);
    $display("%0t - MASTER - sending ", $time, fshow(f));
  endrule
  rule putAXI4_WFlit (sendWrite);
    AXI4_WFlit#(DATA_sz, WUSER_sz) f = AXI4_WFlit{
      wdata: cnt, wstrb: ?, wlast: True, wuser: ?
    };
    shim.slave.w.put(f);
    $display("%0t - MASTER - sending ", $time, fshow(f));
  endrule
  rule getAXI4_BFlit;
    let rsp <- get(shim.slave.b);
    $display("%0t - MASTER - received ", $time, fshow(rsp));
  endrule
  rule putAXI4_ARFlit; shim.slave.ar.put(?); endrule
  rule dropAXI4_RFlit; shim.slave.r.drop; endrule

  // return AXI interface
  return shim.master;

endmodule

module axiSlave (`SLAVE_T);

  // AXI slave shim
  AXI4_Shim#(`SPARAMS) shim <- mkAXI4Shim;

  // arbitrary work for each channel
  let writeResp <- mkFIFOF;
  rule getAXI4_AWFlit;
    let req <- get(shim.master.aw);
    writeResp.enq(AXI4_BFlit{
      bid: req.awid, bresp: OKAY, buser: ?
    });
    $display("%0t - SLAVE - received ", $time, fshow(req));
  endrule
  rule getAXI4_WFlit;
    let req <- get(shim.master.w);
    $display("%0t - SLAVE - received ", $time, fshow(req));
  endrule
  rule putAXI4_BFlit;
    writeResp.deq;
    shim.master.b.put(writeResp.first);
    $display("%0t - SLAVE - sending ", $time, fshow(writeResp.first));
  endrule
  let readResp <- mkFIFOF;
  rule getAXI4_ARFlit;
    let req <- get(shim.master.ar);
    readResp.enq(AXI4_RFlit{
      rid: req.arid, rdata: ?, rresp: SLVERR, rlast: True, ruser: ?
    });
  endrule
  rule putAXI4_RFlit;
    readResp.deq;
    shim.master.r.put(readResp.first);
  endrule

  // return AXI interface
  return shim.slave;

endmodule

module top (Empty);
  Vector#(NMASTERS, `MASTER_T) ms;
  Vector#(NSLAVES, `SLAVE_T)   ss;
  for (Integer i = 0; i < valueOf(NMASTERS); i = i + 1)
    ms[i] <- axiMaster;
  MappingTable#(NSLAVES, ADDR_sz) maptab = newVector;
  for (Integer i = 0; i < valueOf(NSLAVES); i = i + 1) begin
    maptab[i] = Range{base: fromInteger(i*valueOf(SlaveWidth)), size: fromInteger(valueOf(SlaveWidth))};
    ss[i] <- axiSlave;
  end
  mkAXI4Bus(maptab, ms, ss);
endmodule

`undef PARAMS
`undef MPARAMS
`undef SPARAMS
`undef MASTER_T
`undef SLAVE_T
