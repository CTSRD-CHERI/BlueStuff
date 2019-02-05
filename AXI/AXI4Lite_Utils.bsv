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

function Source#(AXI4Lite_WriteFlit#(addr_, data_, awu_, wu_)) mergeLiteWrite(
  Source#(AXI4Lite_AWFlit#(addr_, awu_)) aw,
  Source#(AXI4Lite_WFlit#(data_, wu_)) w) = interface Source;
    method canPeek = aw.canPeek && w.canPeek;
    method peek    = AXI4Lite_WriteFlit { aw: aw.peek, w: w.peek };
    method drop    = action aw.drop; w.drop; endaction;
  endinterface;

function Sink#(AXI4Lite_WriteFlit#(addr_, data_, awu_, wu_)) splitLiteWrite(
  Sink#(AXI4Lite_AWFlit#(addr_, awu_)) aw,
  Sink#(AXI4Lite_WFlit#(data_, wu_)) w) = interface Sink;
    method canPut = aw.canPut && w.canPut;
    method put(x) = action
      aw.put(x.aw);
      w.put(x.w);
    endaction;
  endinterface;

////////////////////////////
// Address offset helpers //
////////////////////////////////////////////////////////////////////////////////

function AXI4Lite_AWFlit#(a, b)
  offsetAWFlit(AXI4Lite_AWFlit#(a, b) f, Int#(a) o) = AXI4Lite_AWFlit {
    awaddr: pack(unpack(f.awaddr) + o), awprot: f.awprot, awuser: f.awuser
  };
function AXI4Lite_ARFlit#(a, b)
  offsetARFlit(AXI4Lite_ARFlit#(a, b) f, Int#(a) o) = AXI4Lite_ARFlit {
    araddr: pack(unpack(f.araddr) + o), arprot: f.arprot, aruser: f.aruser
  };
function AXI4Lite_Slave#(a, b, c, d, e, f, g) offsetSlave(
  AXI4Lite_Slave#(a, b, c, d, e, f, g) s, Integer offset) =
  interface AXI4Lite_Slave;
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

function AXI4Lite_Slave#(a, b, c, d, e, f, g) dropUserFields(
  AXI4Lite_Slave#(a, b, 0, 0, 0, 0, 0) s) = interface AXI4Lite_Slave;
    interface aw = interface Sink;
      method canPut = s.aw.canPut;
      method put(x) = s.aw.put(AXI4Lite_AWFlit {
        awaddr: x.awaddr, awprot: x.awprot, awuser: ?
      });
    endinterface;
    interface w = interface Sink;
      method canPut = s.w.canPut;
      method put(x) = s.w.put(AXI4Lite_WFlit {
        wdata: x.wdata, wstrb: x.wstrb, wuser: ?
      });
    endinterface;
    interface b = interface Source;
      method canPeek = s.b.canPeek;
      method peek    = AXI4Lite_BFlit {
                         bresp: s.b.peek.bresp, buser: unpack(0)
                       };
      method drop    = s.b.drop;
    endinterface;
    interface ar = interface Sink;
      method canPut = s.ar.canPut;
      method put(x) = s.ar.put(AXI4Lite_ARFlit {
        araddr: x.araddr, arprot: x.arprot, aruser: ?
      });
    endinterface;
    interface r = interface Source;
      method canPeek = s.r.canPeek;
      method peek;
        let rflit = s.r.peek;
        return AXI4Lite_RFlit {
          rdata: rflit.rdata, rresp: rflit.rresp, ruser: unpack(0)
        };
      endmethod
      method drop = s.r.drop;
    endinterface;
  endinterface;

///////////////////////////////
// AXI Shim Master <-> Slave //
////////////////////////////////////////////////////////////////////////////////

module mkAXI4LiteShim (AXI4Lite_Shim#(a, b, c, d, e, f, g));
  let awff <- mkBypassFIFOF;
  let  wff <- mkBypassFIFOF;
  let  bff <- mkBypassFIFOF;
  let arff <- mkBypassFIFOF;
  let  rff <- mkBypassFIFOF;
  interface master = interface AXI4Lite_Master;
    interface aw = toSource(awff);
    interface  w = toSource(wff);
    interface  b = toSink(bff);
    interface ar = toSource(arff);
    interface  r = toSink(rff);
  endinterface;
  interface slave = interface AXI4Lite_Slave;
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
module toAXI4Lite_Master_Synth#(AXI4Lite_Master#(a, b, c, d, e, f, g) master)
  (AXI4Lite_Master_Synth#(a, b, c, d, e, f, g));
  let awifc <- toAXI4Lite_AW_Master_Synth(master.aw);
  let wifc  <- toAXI4Lite_W_Master_Synth(master.w);
  let bifc  <- toAXI4Lite_B_Master_Synth(master.b);
  let arifc <- toAXI4Lite_AR_Master_Synth(master.ar);
  let rifc  <- toAXI4Lite_R_Master_Synth(master.r);
  interface aw = awifc;
  interface w  = wifc;
  interface b  = bifc;
  interface ar = arifc;
  interface r  = rifc;
endmodule

// AXI Slave
module toAXI4Lite_Slave_Synth#(AXI4Lite_Slave#(a, b, c, d, e, f, g) master)
  (AXI4Lite_Slave_Synth#(a, b, c, d, e, f, g));
  let awifc <- toAXI4Lite_AW_Slave_Synth(master.aw);
  let wifc  <- toAXI4Lite_W_Slave_Synth(master.w);
  let bifc  <- toAXI4Lite_B_Slave_Synth(master.b);
  let arifc <- toAXI4Lite_AR_Slave_Synth(master.ar);
  let rifc  <- toAXI4Lite_R_Slave_Synth(master.r);
  interface aw = awifc;
  interface w  = wifc;
  interface b  = bifc;
  interface ar = arifc;
  interface r  = rifc;
endmodule
