/*-
 * Copyright (c) 2018 Alexandre Joannou
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

import AXI :: *;

import Routable :: *;
import SourceSink :: *;
import ListExtra :: *;

import Connectable :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;
import List :: *;
import Vector :: *;

typedef 2 NMASTERS;
typedef 3 NSLAVES;

typedef 4096 SlaveWidth;

typedef TAdd#(1, TLog#(TMul#(NSLAVES, SlaveWidth))) ADDR_sz;
typedef 128 DATA_sz;

`define MASTER_T AXILiteMaster#(ADDR_sz, DATA_sz)
`define SLAVE_T  AXILiteSlave#(ADDR_sz, DATA_sz)

module axiMaster (`MASTER_T);

  // AXI master shim
  AXILiteShim#(ADDR_sz, DATA_sz) shim <- mkAXILiteShim;
  // Req addr
  Reg#(Bit#(ADDR_sz)) nextWriteAddr <- mkReg(0);

  // counter
  Reg#(Bit#(DATA_sz)) cnt <- mkReg(0);
  rule counteUp; cnt <= cnt + 1; endrule

  // arbitrary work for each channel
  Bool sendWrite = cnt[3:0] == 0;
  rule putAWFlit (sendWrite);
    AWLiteFlit#(ADDR_sz) f = ?;
    f.awaddr = nextWriteAddr;
    nextWriteAddr <= nextWriteAddr + fromInteger(valueOf(SlaveWidth)) + 1;
    shim.slave.aw.put(f);
    $display("%0t - MASTER - sending ", $time, fshow(f));
  endrule
  rule putWFlit (sendWrite);
    WLiteFlit#(DATA_sz) f = WLiteFlit{wdata: cnt, wstrb: ?};
    shim.slave.w.put(f);
    $display("%0t - MASTER - sending ", $time, fshow(f));
  endrule
  rule getBFlit;
    let rsp <- shim.slave.b.get;
    $display("%0t - MASTER - received ", $time, fshow(rsp));
  endrule
  rule putARFlit; shim.slave.ar.put(?); endrule
  rule getRFlit; let _ <- shim.slave.r.get; endrule

  // return AXI interface
  return shim.master;

endmodule

module axiSlave (`SLAVE_T);

  // AXI slave shim
  AXILiteShim#(ADDR_sz, DATA_sz) shim <- mkAXILiteShim;

  // arbitrary work for each channel
  FIFOF#(Bit#(0)) writeResp <- mkFIFOF;
  rule getAWFlit;
    let req <- shim.master.aw.get;
    $display("%0t - SLAVE - received ", $time, fshow(req));
  endrule
  rule getWFlit;
    let req <- shim.master.w.get;
    $display("%0t - SLAVE - received ", $time, fshow(req));
    writeResp.enq(?);
  endrule
  rule putBFlit;
    writeResp.deq;
    BLiteFlit f = ?;
    shim.master.b.put(f);
    $display("%0t - SLAVE - sending ", $time, fshow(f));
  endrule
  rule getARFlit; let _ <- shim.master.ar.get; endrule
  rule putRFlit; shim.master.r.put(?); endrule

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
  mkAXILiteBus(maptab, ms, ss);
endmodule

`undef MASTER_T
`undef SLAVE_T
