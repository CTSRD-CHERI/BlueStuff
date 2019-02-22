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

// XXX
// Macro to work around the fact that we cannot pass FIFOF module constructor
// and use it for different channels with different types (need Rank2Types)...?
`define defAXI4LiteShimFIFOF (name, mkFF)\
module mkAXI4LiteShim``name (AXI4Lite_Shim#(a, b, c, d, e, f, g));\
  let awff <- mkFF;\
  let  wff <- mkFF;\
  let  bff <- mkFF;\
  let arff <- mkFF;\
  let  rff <- mkFF;\
  method clear = action\
    awff.clear;\
    wff.clear;\
    bff.clear;\
    arff.clear;\
    rff.clear;\
  endaction;\
  interface master = interface AXI4Lite_Master;\
    interface aw = toSource(awff);\
    interface  w = toSource(wff);\
    interface  b = toSink(bff);\
    interface ar = toSource(arff);\
    interface  r = toSink(rff);\
  endinterface;\
  interface slave = interface AXI4Lite_Slave;\
    interface aw = toSink(awff);\
    interface  w = toSink(wff);\
    interface  b = toSource(bff);\
    interface ar = toSink(arff);\
    interface  r = toSource(rff);\
  endinterface;\
endmodule

`defAXI4LiteShimFIFOF(BypassFIFOF, mkBypassFIFOF)
`defAXI4LiteShimFIFOF(BypassFF1, mkSizedBypassFIFOF(1))
`defAXI4LiteShimFIFOF(FF1, mkFIFOF1)
`defAXI4LiteShimFIFOF(SizedFIFOF4, mkSizedFIFOF(4))

module mkAXI4LiteShim (AXI4Lite_Shim#(a, b, c, d, e, f, g));
  AXI4Lite_Shim#(a, b, c, d, e, f, g) shim <- mkAXI4LiteShimBypassFIFOF;
  return shim;
endmodule


/////////////////////////////////////
// to/from "Synth" interface utils //
////////////////////////////////////////////////////////////////////////////////

// AXI4Lite Master
function AXI4Lite_Master_Synth#(a, b, c, d, e, f, g)
  toAXI4Lite_Master_Synth (AXI4Lite_Master#(a, b, c, d, e, f, g) master) =
  interface AXI4Lite_Master_Synth;
    interface aw = toAXI4Lite_AW_Master_Synth(master.aw);
    interface w  = toAXI4Lite_W_Master_Synth(master.w);
    interface b  = toAXI4Lite_B_Master_Synth(master.b);
    interface ar = toAXI4Lite_AR_Master_Synth(master.ar);
    interface r  = toAXI4Lite_R_Master_Synth(master.r);
  endinterface;

function AXI4Lite_Master#(a, b, c, d, e, f, g)
  fromAXI4Lite_Master_Synth (AXI4Lite_Master_Synth#(a, b, c, d, e, f, g) master) =
  interface AXI4Lite_Master;
    interface aw = fromAXI4Lite_AW_Master_Synth(master.aw);
    interface w  = fromAXI4Lite_W_Master_Synth(master.w);
    interface b  = fromAXI4Lite_B_Master_Synth(master.b);
    interface ar = fromAXI4Lite_AR_Master_Synth(master.ar);
    interface r  = fromAXI4Lite_R_Master_Synth(master.r);
  endinterface;

// AXI4Lite Slave
function AXI4Lite_Slave_Synth#(a, b, c, d, e, f, g)
  toAXI4Lite_Slave_Synth (AXI4Lite_Slave#(a, b, c, d, e, f, g) slave) =
  interface AXI4Lite_Slave_Synth;
    interface aw = toAXI4Lite_AW_Slave_Synth(slave.aw);
    interface w  = toAXI4Lite_W_Slave_Synth(slave.w);
    interface b  = toAXI4Lite_B_Slave_Synth(slave.b);
    interface ar = toAXI4Lite_AR_Slave_Synth(slave.ar);
    interface r  = toAXI4Lite_R_Slave_Synth(slave.r);
  endinterface;

function AXI4Lite_Slave#(a, b, c, d, e, f, g)
  fromAXI4Lite_Slave_Synth (AXI4Lite_Slave_Synth#(a, b, c, d, e, f, g) slave) =
  interface AXI4Lite_Slave;
    interface aw = fromAXI4Lite_AW_Slave_Synth(slave.aw);
    interface w  = fromAXI4Lite_W_Slave_Synth(slave.w);
    interface b  = fromAXI4Lite_B_Slave_Synth(slave.b);
    interface ar = fromAXI4Lite_AR_Slave_Synth(slave.ar);
    interface r  = fromAXI4Lite_R_Slave_Synth(slave.r);
  endinterface;

/////////////////////////////
// to unguarded interfaces //
////////////////////////////////////////////////////////////////////////////////

module toUnguarded_AXI4Lite_Master#(AXI4Lite_Master#(a, b, c, d, e, f, g) m)
  (AXI4Lite_Master#(a, b, c, d, e, f, g));
  let u_aw <- toUnguardedSource(m.aw, ?);
  let u_w  <- toUnguardedSource(m.w, ?);
  let u_b  <- toUnguardedSink(m.b);
  let u_ar <- toUnguardedSource(m.ar, ?);
  let u_r  <- toUnguardedSink(m.r);
  return interface AXI4Lite_Master;
    interface aw = u_aw;
    interface w  = u_w;
    interface b  = u_b;
    interface ar = u_ar;
    interface r  = u_r;
  endinterface;
endmodule

module mkAXI4Lite_Master_Xactor(AXI4Lite_Master_Xactor#(a, b, c, d, e, f, g));
  let shim <- mkAXI4LiteShimSizedFIFOF4;
  let u_master <- toUnguarded_AXI4Lite_Master(shim.master);
  //method clear = shim.clear;
  method clear = noAction;
  interface slave = shim.slave;
  interface masterSynth = toAXI4Lite_Master_Synth(u_master);
endmodule

module toUnguarded_AXI4Lite_Slave#(AXI4Lite_Slave#(a, b, c, d, e, f, g) s)
  (AXI4Lite_Slave#(a, b, c, d, e, f, g));
  let u_aw <- toUnguardedSink(s.aw);
  let u_w  <- toUnguardedSink(s.w);
  let u_b  <- toUnguardedSource(s.b, ?);
  let u_ar <- toUnguardedSink(s.ar);
  let u_r  <- toUnguardedSource(s.r, ?);
  return interface AXI4Lite_Slave;
    interface aw = u_aw;
    interface w  = u_w;
    interface b  = u_b;
    interface ar = u_ar;
    interface r  = u_r;
  endinterface;
endmodule

module mkAXI4Lite_Slave_Xactor (AXI4Lite_Slave_Xactor#(a, b, c, d, e, f, g));
  let shim <- mkAXI4LiteShimSizedFIFOF4;
  let u_slave <- toUnguarded_AXI4Lite_Slave(shim.slave);
  method clear = shim.clear;
  interface master = shim.master;
  interface slaveSynth = toAXI4Lite_Slave_Synth(u_slave);
endmodule
