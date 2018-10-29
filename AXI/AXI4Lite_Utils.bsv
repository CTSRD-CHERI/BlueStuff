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
import AXI4Lite_Types :: *;
import AXI4Lite_AW_Utils :: *;
import AXI4Lite_W_Utils :: *;
import AXI4Lite_B_Utils :: *;
import AXI4Lite_AR_Utils :: *;
import AXI4Lite_R_Utils :: *;

// BlueBasics import
import SourceSink :: *;

// Standard
import FIFOF :: *;
import SpecialFIFOs :: *;

///////////////////////////////
// AXI Write channel helpers //
////////////////////////////////////////////////////////////////////////////////

function Source#(AXILiteWriteFlit#(addr_, data_, awu_, wu_)) mergeLiteWrite(
  Source#(AWLiteFlit#(addr_, awu_)) aw,
  Source#(WLiteFlit#(data_, wu_)) w) = interface Source;
    method canGet = aw.canGet && w.canGet;
    method peek   = AXILiteWriteFlit { aw: aw.peek, w: w.peek };
    method get    = actionvalue
      let flit_aw <- aw.get;
      let flit_w  <- w.get;
      return AXILiteWriteFlit { aw: flit_aw, w: flit_w };
    endactionvalue;
  endinterface;

function Sink#(AXILiteWriteFlit#(addr_, data_, awu_, wu_)) splitLiteWrite(
  Sink#(AWLiteFlit#(addr_, awu_)) aw,
  Sink#(WLiteFlit#(data_, wu_)) w) = interface Sink;
    method canPut = aw.canPut && w.canPut;
    method put(x) = action
      aw.put(x.aw);
      w.put(x.w);
    endaction;
  endinterface;

///////////////////////////////
// AXI Shim Master <-> Slave //
////////////////////////////////////////////////////////////////////////////////

module mkAXILiteShim (AXILiteShim#(a, b, c, d, e, f, g));
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
module toAXILiteMasterSynth#(AXILiteMaster#(a, b, c, d, e, f, g) master)
  (AXILiteMasterSynth#(a, b, c, d, e, f, g));
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
module toAXILiteSlaveSynth#(AXILiteSlave#(a, b, c, d, e, f, g) master)
  (AXILiteSlaveSynth#(a, b, c, d, e, f, g));
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
