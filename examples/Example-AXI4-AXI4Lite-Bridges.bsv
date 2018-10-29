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
import SourceSink :: *;

import FIFOF :: *;
import Connectable :: *;

typedef   0 ID_sz;
typedef  32 ADDR_sz;
typedef 128 DATA_sz;
typedef   0 AWUSER_sz;
typedef   0 WUSER_sz;
typedef   0 BUSER_sz;
typedef   0 ARUSER_sz;
typedef   0 RUSER_sz;

`define PARAMS ID_sz, ADDR_sz, DATA_sz, AWUSER_sz, WUSER_sz, BUSER_sz, ARUSER_sz, RUSER_sz
`define PARAMS_LITE ADDR_sz, DATA_sz, AWUSER_sz, WUSER_sz, BUSER_sz, ARUSER_sz, RUSER_sz
`define MASTER_T      AXIMaster#(`PARAMS)
`define MASTER_LITE_T AXILiteMaster#(`PARAMS_LITE)
`define SLAVE_T       AXISlave#(`PARAMS)
`define SLAVE_LITE_T  AXILiteSlave#(`PARAMS_LITE)

module axiMaster (`MASTER_T);

  // AXI master shim
  AXIShim#(`PARAMS) shim <- mkAXIShim;
  // Req addr
  Reg#(Bit#(ADDR_sz)) nextWriteAddr <- mkReg(0);

  // counter
  Reg#(Bit#(DATA_sz)) cnt <- mkReg(0);
  rule counteUp; cnt <= cnt + 1; endrule

  // arbitrary work for each channel
  Bool sendWrite = cnt[3:0] == 0;
  rule putAWFlit (sendWrite);
    AWFlit#(ID_sz, ADDR_sz, AWUSER_sz) f = ?;
    f.awaddr = nextWriteAddr;
    f.awlen  = 0;
    nextWriteAddr <= nextWriteAddr + 1;
    shim.slave.aw.put(f);
    $display("%0t - MASTER - sending ", $time, fshow(f));
  endrule
  rule putWFlit (sendWrite);
    WFlit#(DATA_sz, WUSER_sz) f = WFlit {
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

module axiMasterLite (`MASTER_LITE_T);

  // AXI master shim
  AXILiteShim#(`PARAMS_LITE) shim <- mkAXILiteShim;
  // Req addr
  Reg#(Bit#(ADDR_sz)) nextWriteAddr <- mkReg(0);

  // counter
  Reg#(Bit#(DATA_sz)) cnt <- mkReg(0);
  rule counteUp; cnt <= cnt + 1; endrule

  // arbitrary work for each channel
  Bool sendWrite = cnt[3:0] == 0;
  rule putAWFlit (sendWrite);
    AWLiteFlit#(ADDR_sz, AWUSER_sz) f = ?;
    f.awaddr = nextWriteAddr;
    nextWriteAddr <= nextWriteAddr + 1;
    shim.slave.aw.put(f);
    $display("%0t - MASTER - sending ", $time, fshow(f));
  endrule
  rule putWFlit (sendWrite);
    WLiteFlit#(DATA_sz, WUSER_sz) f = WLiteFlit {
      wdata: cnt, wstrb: ?, wuser: ?
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

module axiSlave (`SLAVE_T);

  // AXI slave shim
  AXIShim#(`PARAMS) shim <- mkAXIShim;

  // arbitrary work for each channel
  let writeResp <- mkFIFOF;
  rule getAWFlit;
    let req <- shim.master.aw.get;
    writeResp.enq(BFlit {
      bid: req.awid, bresp: OKAY, buser: ?
    });
    $display("%0t - SLAVE - received ", $time, fshow(req));
  endrule
  rule getWFlit;
    let req <- shim.master.w.get;
    $display("%0t - SLAVE - received ", $time, fshow(req));
  endrule
  rule putBFlit;
    writeResp.deq;
    shim.master.b.put(writeResp.first);
    $display("%0t - SLAVE - sending ", $time, fshow(writeResp.first));
  endrule
  let readResp <- mkFIFOF;
  rule getARFlit;
    let req <- shim.master.ar.get;
    readResp.enq(RFlit {
      rid: req.arid, rdata: ?, rresp: SLVERR, rlast: True, ruser: ?
    });
  endrule
  rule putRFlit;
    readResp.deq;
    shim.master.r.put(readResp.first);
  endrule

  // return AXI interface
  return shim.slave;

endmodule

module axiSlaveLite (`SLAVE_LITE_T);

  // AXI slave shim
  AXILiteShim#(`PARAMS_LITE) shim <- mkAXILiteShim;

  // arbitrary work for each channel
  let writeResp <- mkFIFOF;
  rule getAWFlit;
    let req <- shim.master.aw.get;
    writeResp.enq(BLiteFlit {
      bresp: OKAY, buser: ?
    });
    $display("%0t - SLAVE - received ", $time, fshow(req));
  endrule
  rule getWFlit;
    let req <- shim.master.w.get;
    $display("%0t - SLAVE - received ", $time, fshow(req));
  endrule
  rule putBFlit;
    writeResp.deq;
    shim.master.b.put(writeResp.first);
    $display("%0t - SLAVE - sending ", $time, fshow(writeResp.first));
  endrule
  let readResp <- mkFIFOF;
  rule getARFlit;
    let req <- shim.master.ar.get;
    readResp.enq(RLiteFlit {
      rdata: ?, rresp: SLVERR, ruser: ?
    });
  endrule
  rule putRFlit;
    readResp.deq;
    shim.master.r.put(readResp.first);
  endrule

  // return AXI interface
  return shim.slave;

endmodule

module top (Empty);
  `MASTER_LITE_T mlite0 <- axiMasterLite;
  `SLAVE_T           s0 <- axiSlave;
  `MASTER_T          m0 <- fromAXILiteMaster(mlite0);
  mkConnection(m0, s0);
  //`MASTER_T         m1 <- axiMaster;
  //`SLAVE_LITE_T slite1 <- axiSlaveLite;
  //`SLAVE_T          s1 <- fromAXILiteSlave(slite1);
  //mkConnection(m1, s1);
  `MASTER_LITE_T mlite2 <- axiMasterLite;
  `MASTER_T          m2 <- fromAXILiteMaster(mlite2);
  `SLAVE_LITE_T  slite2 <- axiSlaveLite;
  `SLAVE_T           s2 <- fromAXILiteSlave(slite2);
  mkConnection(m2, s2);
endmodule

`undef PARAMS
`undef PARAMS_LITE
`undef MASTER_T
`undef MASTER_LITE_T
`undef SLAVE_T
`undef SLAVE_LITE_T
