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
    method canPeek = aw.canPeek && w.canPeek;
    method peek    = AXILiteWriteFlit { aw: aw.peek, w: w.peek };
    method drop    = action aw.drop; w.drop; endaction;
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

////////////////////////////
// Address offset helpers //
////////////////////////////////////////////////////////////////////////////////

function AWLiteFlit#(a, b) offsetAWFlit(AWLiteFlit#(a, b) f, Int#(a) o) =
  AWLiteFlit {
    awaddr: pack(unpack(f.awaddr) + o), awprot: f.awprot, awuser: f.awuser
  };
function ARLiteFlit#(a, b) offsetARFlit(ARLiteFlit#(a, b) f, Int#(a) o) =
  ARLiteFlit {
    araddr: pack(unpack(f.araddr) + o), arprot: f.arprot, aruser: f.aruser
  };
function AXILiteSlave#(a, b, c, d, e, f, g) offsetSlave(
  AXILiteSlave#(a, b, c, d, e, f, g) s, Integer offset) =
  interface AXILiteSlave;
    interface aw = interface Sink;
      method canPut = s.aw.canPut;
      method put(x) = s.aw.put(offsetAWFlit(x, fromInteger(-offset)));
    endinterface;
    interface w  = s.w;
    interface b  = s.b;
    interface ar = interface Sink;
      method canPut = s.ar.canPut;
      method put(x) = s.ar.put(offsetARFlit(x, fromInteger(-offset)));
    endinterface;
    interface r  = s.r;
  endinterface;

//////////////////////////////
// drop user fields helpers //
////////////////////////////////////////////////////////////////////////////////

function AXILiteSlave#(a, b, c, d, e, f, g) dropUserFields(
  AXILiteSlave#(a, b, 0, 0, 0, 0, 0) s) = interface AXILiteSlave;
    interface aw = interface Sink;
      method canPut = s.aw.canPut;
      method put(x) = s.aw.put(AWLiteFlit {
        awaddr: x.awaddr, awprot: x.awprot, awuser: ?
      });
    endinterface;
    interface w = interface Sink;
      method canPut = s.w.canPut;
      method put(x) = s.w.put(WLiteFlit {
        wdata: x.wdata, wstrb: x.wstrb, wuser: ?
      });
    endinterface;
    interface b = interface Source;
      method canPeek = s.b.canPeek;
      method peek    = BLiteFlit { bresp: s.b.peek.bresp, buser: unpack(0) };
      method drop    = s.b.drop;
    endinterface;
    interface ar = interface Sink;
      method canPut = s.ar.canPut;
      method put(x) = s.ar.put(ARLiteFlit {
        araddr: x.araddr, arprot: x.arprot, aruser: ?
      });
    endinterface;
    interface r = interface Source;
      method canPeek = s.r.canPeek;
      method peek;
        let rflit = s.r.peek;
        return RLiteFlit {
          rdata: rflit.rdata, rresp: rflit.rresp, ruser: unpack(0)
        };
      endmethod
      method drop = s.r.drop;
    endinterface;
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
