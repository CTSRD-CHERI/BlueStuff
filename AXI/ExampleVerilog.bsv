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

import FIFOF :: *;

typedef 4 ID_sz;
typedef 32 ADDR_sz;
typedef 128 DATA_sz;
typedef 0 USER_sz;

(* synthesize, clock_prefix="aclk", reset_prefix="aresetn" *)
module axiMaster (AXIMaster#(ID_sz, ADDR_sz, DATA_sz, USER_sz));

  // one FIFOF per channel
  FIFOF#(AWFlit#(ID_sz, ADDR_sz, USER_sz)) awff <- mkFIFOF;
  FIFOF#(WFlit#(DATA_sz, USER_sz))         wff  <- mkFIFOF;
  FIFOF#(BFlit#(ID_sz, USER_sz))           bff  <- mkFIFOF;
  FIFOF#(ARFlit#(ID_sz, ADDR_sz, USER_sz)) arff <- mkFIFOF;
  FIFOF#(RFlit#(ID_sz, DATA_sz, USER_sz))  rff  <- mkFIFOF;

  // arbitrary work (enq/deq FIFOF) for each channel
  rule enqAWFlit; awff.enq(?); endrule
  rule enqWFlit;  wff.enq(?);  endrule
  rule deqBFlit;  bff.deq;     endrule
  rule enqARFlit; arff.enq(?); endrule
  rule deqRFlit;  rff.deq;     endrule

  // turn FIFOFs to AXI interfaces
  let awifc <- toAXIAWMaster(awff);
  let wifc  <- toAXIWMaster(wff);
  let bifc  <- toAXIBMaster(bff);
  let arifc <- toAXIARMaster(arff);
  let rifc  <- toAXIRMaster(rff);

  // assign channel interfaces to module the interface
  interface aw = awifc;
  interface w  = wifc;
  interface b  = bifc;
  interface ar = arifc;
  interface r  = rifc;

endmodule

(* synthesize, clock_prefix="aclk", reset_prefix="aresetn" *)
module axiLiteMaster (AXILiteMaster#(ADDR_sz, DATA_sz));

  // one FIFOF per channel
  FIFOF#(AWLiteFlit#(ADDR_sz)) awff <- mkFIFOF;
  FIFOF#(WLiteFlit#(DATA_sz))  wff  <- mkFIFOF;
  FIFOF#(BLiteFlit)            bff  <- mkFIFOF;
  FIFOF#(ARLiteFlit#(ADDR_sz)) arff <- mkFIFOF;
  FIFOF#(RLiteFlit#(DATA_sz))  rff  <- mkFIFOF;

  // arbitrary work (enq/deq FIFOF) for each channel
  rule enqAWFlit; awff.enq(?); endrule
  rule enqWFlit;  wff.enq(?);  endrule
  rule deqBFlit;  bff.deq;     endrule
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
module axiSlave (AXISlave#(ID_sz, ADDR_sz, DATA_sz, USER_sz));

  // one FIFOF per channel
  FIFOF#(AWFlit#(ID_sz, ADDR_sz, USER_sz)) awff <- mkFIFOF;
  FIFOF#(WFlit#(DATA_sz, USER_sz))         wff  <- mkFIFOF;
  FIFOF#(BFlit#(ID_sz, USER_sz))           bff  <- mkFIFOF;
  FIFOF#(ARFlit#(ID_sz, ADDR_sz, USER_sz)) arff <- mkFIFOF;
  FIFOF#(RFlit#(ID_sz, DATA_sz, USER_sz))  rff  <- mkFIFOF;

  // arbitrary work (enq/deq FIFOF) for each channel
  rule deqAWFlit; awff.deq;   endrule
  rule deqWFlit;  wff.deq;    endrule
  rule enqBFlit;  bff.enq(?); endrule
  rule deqARFlit; arff.deq;   endrule
  rule enqRFlit;  rff.enq(?); endrule

  // turn FIFOFs to AXI interfaces
  let awifc <- toAXIAWSlave(awff);
  let wifc  <- toAXIWSlave(wff);
  let bifc  <- toAXIBSlave(bff);
  let arifc <- toAXIARSlave(arff);
  let rifc  <- toAXIRSlave(rff);

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
  FIFOF#(AWLiteFlit#(ADDR_sz)) awff <- mkFIFOF;
  FIFOF#(WLiteFlit#(DATA_sz))  wff  <- mkFIFOF;
  FIFOF#(BLiteFlit)            bff  <- mkFIFOF;
  FIFOF#(ARLiteFlit#(ADDR_sz)) arff <- mkFIFOF;
  FIFOF#(RLiteFlit#(DATA_sz))  rff  <- mkFIFOF;

  // arbitrary work (enq/deq FIFOF) for each channel
  rule deqAWFlit; awff.deq;   endrule
  rule deqWFlit;  wff.deq;    endrule
  rule enqBFlit;  bff.enq(?); endrule
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
