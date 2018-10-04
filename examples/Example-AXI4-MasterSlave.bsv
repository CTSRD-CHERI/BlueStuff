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

import AXI4 :: *;

import SourceSink :: *;

import Connectable :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;

typedef 0 ID_sz;
typedef 32 ADDR_sz;
typedef 128 DATA_sz;
typedef 0 USER_sz;

(* synthesize, clock_prefix="aclk", reset_prefix="aresetn" *)
module axiMaster (AXIMaster#(ID_sz, ADDR_sz, DATA_sz, USER_sz));

  // AXI master shim
  AXIShim#(ID_sz, ADDR_sz, DATA_sz, USER_sz) shim <- mkAXIShim;

  // counter
  Reg#(Bit#(DATA_sz)) cnt <- mkReg(0);
  rule counteUp; cnt <= cnt + 1; endrule

  // arbitrary work for each channel
  Bool sendWrite = cnt[3:0] == 0;
  rule putAWFlit (sendWrite);
    AWFlit#(ID_sz, ADDR_sz, USER_sz) f = ?;
    shim.slave.aw.put(f);
    $display("%0t - MASTER - sending ", $time, fshow(f));
  endrule
  rule putWFlit (sendWrite);
    WFlit#(DATA_sz, USER_sz) f = WFlit{
      wdata: cnt, wstrb: ?, wlast: True, wuser: ?
    };
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

(* synthesize, clock_prefix="aclk", reset_prefix="aresetn" *)
module axiSlave (AXISlave#(ID_sz, ADDR_sz, DATA_sz, USER_sz));

  // AXI slave shim
  AXIShim#(ID_sz, ADDR_sz, DATA_sz, USER_sz) shim <- mkAXIShim;

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
    BFlit#(ID_sz, USER_sz) f = ?;
    shim.master.b.put(f);
    $display("%0t - SLAVE - sending ", $time, fshow(f));
  endrule
  rule getARFlit; let _ <- shim.master.ar.get; endrule
  rule putRFlit; shim.master.r.put(?); endrule

  // return AXI interface
  return shim.slave;

endmodule

module top (Empty);
  AXIMaster#(ID_sz, ADDR_sz, DATA_sz, USER_sz) master <- axiMaster;
  AXISlave#(ID_sz, ADDR_sz, DATA_sz, USER_sz)  slave  <- axiSlave;
  mkConnection(master, slave);
endmodule