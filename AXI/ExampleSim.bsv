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
  FIFOF#(AWLiteFlit#(ADDR_sz)) awff <- mkBypassFIFOF;
  FIFOF#(WLiteFlit#(DATA_sz))  wff  <- mkBypassFIFOF;
  FIFOF#(BLiteFlit)            bff  <- mkBypassFIFOF;
  FIFOF#(ARLiteFlit#(ADDR_sz)) arff <- mkBypassFIFOF;
  FIFOF#(RLiteFlit#(DATA_sz))  rff  <- mkBypassFIFOF;

  // counter
  Reg#(Bit#(DATA_sz)) cnt <- mkReg(0);
  rule counteUp; cnt <= cnt + 1; endrule

  // arbitrary work (enq/deq FIFOF) for each channel
  Bool sendWrite = cnt[3:0] == 0;
  rule enqAWFlit (sendWrite);
    AWLiteFlit#(ADDR_sz) f = ?;
    awff.enq(f);
    $display("%0t - MASTER - sending ", $time, fshow(f));
  endrule
  rule enqWFlit (sendWrite);
    WLiteFlit#(DATA_sz) f = WLiteFlit{wdata: cnt, wstrb: ?};
    wff.enq(f);
    $display("%0t - MASTER - sending ", $time, fshow(f));
  endrule
  rule deqBFlit;
    bff.deq;
    $display("%0t - MASTER - received ", $time, fshow(bff.first));
  endrule
  rule enqARFlit; arff.enq(?); endrule
  rule deqRFlit;  rff.deq;     endrule

  // turn FIFOFs to AXI interfaces
  let awifc <- toAXIAWLiteMaster(awff);
  let wifc  <- toAXIWLiteMaster(wff);
  let bifc  <- toAXIBLiteMaster(bff);
  let arifc <- toAXIARLiteMaster(arff);
  let rifc  <- toAXIRLiteMaster(rff);

  // assign channel interfaces to module the interface
  interface aw = awifc;
  interface w  = wifc;
  interface b  = bifc;
  interface ar = arifc;
  interface r  = rifc;

endmodule

(* synthesize, clock_prefix="aclk", reset_prefix="aresetn" *)
module axiLiteSlave (AXILiteSlave#(ADDR_sz, DATA_sz));

  // one FIFOF per channel
  FIFOF#(AWLiteFlit#(ADDR_sz)) awff <- mkBypassFIFOF;
  FIFOF#(WLiteFlit#(DATA_sz))  wff  <- mkBypassFIFOF;
  FIFOF#(BLiteFlit)            bff  <- mkBypassFIFOF;
  FIFOF#(ARLiteFlit#(ADDR_sz)) arff <- mkBypassFIFOF;
  FIFOF#(RLiteFlit#(DATA_sz))  rff  <- mkBypassFIFOF;

  // arbitrary work (enq/deq FIFOF) for each channel
  FIFOF#(Bit#(0)) writeResp <- mkFIFOF;
  rule deqAWFlit;
    awff.deq;
    $display("%0t - SLAVE - received ", $time, fshow(awff.first));
  endrule
  rule deqWFlit;
    wff.deq;
    $display("%0t - SLAVE - received ", $time, fshow(wff.first));
    writeResp.enq(?);
  endrule
  rule enqBFlit;
    writeResp.deq;
    BLiteFlit f = ?;
    bff.enq(f);
    $display("%0t - SLAVE - sending ", $time, fshow(f));
  endrule
  rule deqARFlit; arff.deq;   endrule
  rule enqRFlit;  rff.enq(?); endrule

  // turn FIFOFs to AXI interfaces
  let awifc <- toAXIAWLiteSlave(awff);
  let wifc  <- toAXIWLiteSlave(wff);
  let bifc  <- toAXIBLiteSlave(bff);
  let arifc <- toAXIARLiteSlave(arff);
  let rifc  <- toAXIRLiteSlave(rff);

  // assign channel interfaces to module the interface
  interface aw = awifc;
  interface w  = wifc;
  interface b  = bifc;
  interface ar = arifc;
  interface r  = rifc;

endmodule

module exampleSim (Empty);
  AXILiteMaster#(ADDR_sz, DATA_sz) master <- axiLiteMaster;
  AXILiteSlave#(ADDR_sz, DATA_sz)  slave  <- axiLiteSlave;
  mkConnection(master, slave);
endmodule
