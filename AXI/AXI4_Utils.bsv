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

// AXI imports
import AXI4_Types :: *;
import AXI4_AW_Utils :: *;
import AXI4_W_Utils :: *;
import AXI4_B_Utils :: *;
import AXI4_AR_Utils :: *;
import AXI4_R_Utils :: *;

// BlueBasics import
import SourceSink :: *;

// Standard
import FIFOF :: *;
import SpecialFIFOs :: *;

///////////////////////////////
// AXI Shim Master <-> Slave //
////////////////////////////////////////////////////////////////////////////////

module mkAXIShim (AXIShim#(id_, addr_, data_, user_));
  let awff <- mkBypassFIFOF;
  let  wff <- mkBypassFIFOF;
  let  bff <- mkBypassFIFOF;
  let arff <- mkBypassFIFOF;
  let  rff <- mkBypassFIFOF;
  interface master = interface AXIMaster;
    interface aw = toSource(awff);
    interface  w = toSource(wff);
    interface  b = toSink(bff);
    interface ar = toSource(arff);
    interface  r = toSink(rff);
  endinterface;
  interface slave = interface AXISlave;
    interface aw = toSink(awff);
    interface  w = toSink(wff);
    interface  b = toSource(bff);
    interface ar = toSink(arff);
    interface  r = toSource(rff);
  endinterface;
endmodule

module mkAXILiteShim (AXILiteShim#(addr_, data_));
  let awff <- mkBypassFIFOF;
  let  wff <- mkBypassFIFOF;
  let  bff <- mkBypassFIFOF;
  let arff <- mkBypassFIFOF;
  let  rff <- mkBypassFIFOF;
  interface master = interface AXILiteMaster;
    interface aw = toSource(awff);
    interface  w = toSource(wff);
    interface  b = toSink(bff);
    interface ar = toSource(arff);
    interface  r = toSink(rff);
  endinterface;
  interface slave = interface AXILiteSlave;
    interface aw = toSink(awff);
    interface  w = toSink(wff);
    interface  b = toSource(bff);
    interface ar = toSink(arff);
    interface  r = toSource(rff);
  endinterface;
endmodule

/////////////////////////////////////////
// to "Synth" version of the interface //
////////////////////////////////////////////////////////////////////////////////

// AXI Master

module toAXIMasterSynth#(AXIMaster#(id_, addr_, data_, user_) master)
  (AXIMasterSynth#(id_, addr_, data_, user_));
  let awifc <- toAXIAWMaster(master.aw);
  let wifc  <- toAXIWMaster(master.w);
  let bifc  <- toAXIBMaster(master.b);
  let arifc <- toAXIARMaster(master.ar);
  let rifc  <- toAXIRMaster(master.r);
  interface aw = awifc;
  interface w  = wifc;
  interface b  = bifc;
  interface ar = arifc;
  interface r  = rifc;
endmodule

module toAXILiteMasterSynth#(AXILiteMaster#(addr_, data_) master)
  (AXILiteMasterSynth#(addr_, data_));
  let awifc <- toAXIAWLiteMaster(master.aw);
  let wifc  <- toAXIWLiteMaster(master.w);
  let bifc  <- toAXIBLiteMaster(master.b);
  let arifc <- toAXIARLiteMaster(master.ar);
  let rifc  <- toAXIRLiteMaster(master.r);
  interface aw = awifc;
  interface w  = wifc;
  interface b  = bifc;
  interface ar = arifc;
  interface r  = rifc;
endmodule

// AXI Slave

module toAXISlaveSynth#(AXISlave#(id_, addr_, data_, user_) master)
  (AXISlaveSynth#(id_, addr_, data_, user_));
  let awifc <- toAXIAWSlave(master.aw);
  let wifc  <- toAXIWSlave(master.w);
  let bifc  <- toAXIBSlave(master.b);
  let arifc <- toAXIARSlave(master.ar);
  let rifc  <- toAXIRSlave(master.r);
  interface aw = awifc;
  interface w  = wifc;
  interface b  = bifc;
  interface ar = arifc;
  interface r  = rifc;
endmodule

module toAXILiteSlaveSynth#(AXILiteSlave#(addr_, data_) master)
  (AXILiteSlaveSynth#(addr_, data_));
  let awifc <- toAXIAWLiteSlave(master.aw);
  let wifc  <- toAXIWLiteSlave(master.w);
  let bifc  <- toAXIBLiteSlave(master.b);
  let arifc <- toAXIARLiteSlave(master.ar);
  let rifc  <- toAXIRLiteSlave(master.r);
  interface aw = awifc;
  interface w  = wifc;
  interface b  = bifc;
  interface ar = arifc;
  interface r  = rifc;
endmodule
