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

import Connectable :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;

typedef 32 ADDR_sz;
typedef 128 DATA_sz;

(* synthesize, clock_prefix="aclk", reset_prefix="aresetn" *)
module axiLiteMaster (AXILiteMaster#(ADDR_sz, DATA_sz));

  // one FIFOF per channel
  AWLiteMasterShim#(ADDR_sz) awshim <- mkAWLiteMasterShim;
  WLiteMasterShim#(DATA_sz)   wshim <- mkWLiteMasterShim;
  BLiteMasterShim             bshim <- mkBLiteMasterShim;
  ARLiteMasterShim#(ADDR_sz) arshim <- mkARLiteMasterShim;
  RLiteMasterShim#(DATA_sz)   rshim <- mkRLiteMasterShim;

  // counter
  Reg#(Bit#(DATA_sz)) cnt <- mkReg(0);
  rule counteUp; cnt <= cnt + 1; endrule

  // arbitrary work (enq/deq FIFOF) for each channel
  Bool sendWrite = cnt[3:0] == 0;
  rule enqAWFlit (sendWrite);
    AWLiteFlit#(ADDR_sz) f = ?;
    awshim.fifof.enq(f);
    $display("%0t - MASTER - sending ", $time, fshow(f));
  endrule
  rule enqWFlit (sendWrite);
    WLiteFlit#(DATA_sz) f = WLiteFlit{wdata: cnt, wstrb: ?};
    wshim.fifof.enq(f);
    $display("%0t - MASTER - sending ", $time, fshow(f));
  endrule
  rule deqBFlit;
    bshim.fifof.deq;
    $display("%0t - MASTER - received ", $time, fshow(bshim.fifof.first));
  endrule
  rule enqARFlit; arshim.fifof.enq(?); endrule
  rule deqRFlit;  rshim.fifof.deq;     endrule

  // assign channel interfaces to module the interface
  interface aw = awshim.master;
  interface w  = wshim.master;
  interface b  = bshim.master;
  interface ar = arshim.master;
  interface r  = rshim.master;

endmodule

(* synthesize, clock_prefix="aclk", reset_prefix="aresetn" *)
module axiLiteSlave (AXILiteSlave#(ADDR_sz, DATA_sz));

  // one FIFOF per channel
  AWLiteSlaveShim#(ADDR_sz) awshim <- mkAWLiteSlaveShim;
  WLiteSlaveShim#(DATA_sz)   wshim <- mkWLiteSlaveShim;
  BLiteSlaveShim             bshim <- mkBLiteSlaveShim;
  ARLiteSlaveShim#(ADDR_sz) arshim <- mkARLiteSlaveShim;
  RLiteSlaveShim#(DATA_sz)   rshim <- mkRLiteSlaveShim;

  // arbitrary work (enq/deq FIFOF) for each channel
  FIFOF#(Bit#(0)) writeResp <- mkFIFOF;
  rule deqAWFlit;
    awshim.fifof.deq;
    $display("%0t - SLAVE - received ", $time, fshow(awshim.fifof.first));
  endrule
  rule deqWFlit;
    wshim.fifof.deq;
    $display("%0t - SLAVE - received ", $time, fshow(wshim.fifof.first));
    writeResp.enq(?);
  endrule
  rule enqBFlit;
    writeResp.deq;
    BLiteFlit f = ?;
    bshim.fifof.enq(f);
    $display("%0t - SLAVE - sending ", $time, fshow(f));
  endrule
  rule deqARFlit; arshim.fifof.deq;   endrule
  rule enqRFlit;  rshim.fifof.enq(?); endrule

  // assign channel interfaces to module the interface
  interface aw = awshim.slave;
  interface w  = wshim.slave;
  interface b  = bshim.slave;
  interface ar = arshim.slave;
  interface r  = rshim.slave;

endmodule

module exampleSim (Empty);
  AXILiteMaster#(ADDR_sz, DATA_sz) master <- axiLiteMaster;
  AXILiteSlave#(ADDR_sz, DATA_sz)  slave  <- axiLiteSlave;
  mkConnection(master, slave);
endmodule
