/*-
 * Copyright (c) 2018-2021 Alexandre Joannou
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

// BlueStuff import
import Routable :: *;
// BlueBasics import
import SourceSink :: *;
import MasterSlave :: *;

// Standard
import FIFOF :: *;
import SpecialFIFOs :: *;
import ConfigReg :: *;

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

function AXI4Lite_Slave#(a, b, c0, d0, e0, f0, g0) zeroUserFields(
  AXI4Lite_Slave#(a, b, c1, d1, e1, f1, g1) s) = interface AXI4Lite_Slave;
    interface aw = interface Sink;
      method canPut = s.aw.canPut;
      method put(x) = s.aw.put(AXI4Lite_AWFlit {
        awaddr: x.awaddr, awprot: x.awprot, awuser: unpack(0)
      });
    endinterface;
    interface w = interface Sink;
      method canPut = s.w.canPut;
      method put(x) = s.w.put(AXI4Lite_WFlit {
        wdata: x.wdata, wstrb: x.wstrb, wuser: unpack(0)
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
        araddr: x.araddr, arprot: x.arprot, aruser: unpack(0)
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
`defAXI4LiteShimFIFOF(FF, mkFIFOF)
`defAXI4LiteShimFIFOF(SizedFIFOF4, mkSizedFIFOF(4))

module mkAXI4LiteShim (AXI4Lite_Shim#(a, b, c, d, e, f, g));
  AXI4Lite_Shim#(a, b, c, d, e, f, g) shim <- mkAXI4LiteShimBypassFIFOF;
  return shim;
endmodule

/////////////////////////
// Debug / Trace utils //
////////////////////////////////////////////////////////////////////////////////

function AXI4Lite_Master#(a,b,c,d,e,f,g)
         debugAXI4Lite_Master(AXI4Lite_Master#(a,b,c,d,e,f,g) m, Fmt msg) =
  interface AXI4Lite_Master;
    interface aw = debugSource(m.aw, $format(msg, " aw"));
    interface w  = debugSource( m.w, $format(msg, "  w"));
    interface b  = debugSink  ( m.b, $format(msg, "  b"));
    interface ar = debugSource(m.ar, $format(msg, " ar"));
    interface r  = debugSink  ( m.r, $format(msg, "  r"));
  endinterface;

function AXI4Lite_Slave#(a,b,c,d,e,f,g)
         debugAXI4Lite_Slave(AXI4Lite_Slave#(a,b,c,d,e,f,g) s, Fmt msg) =
  interface AXI4Lite_Slave;
    interface aw = debugSink  (s.aw, $format(msg, " aw"));
    interface w  = debugSink  ( s.w, $format(msg, "  w"));
    interface b  = debugSource( s.b, $format(msg, "  b"));
    interface ar = debugSink  (s.ar, $format(msg, " ar"));
    interface r  = debugSource( s.r, $format(msg, "  r"));
  endinterface;

module mkAXI4LiteDebugShim #(String debugTag) (AXI4Lite_Shim#(a,b,c,d,e,f,g));
  let shim <- mkAXI4LiteShim;
  interface  slave = shim.slave;
  interface master = debugAXI4Lite_Master(shim.master, $format(debugTag));
  interface  clear = shim.clear;
endmodule

module mkAXI4DebugShimSig #(String debugTag)
                           (AXI4Lite_Shim_Sig#(a,b,c,d,e,f,g));
  let shim <- mkAXI4LiteDebugShim(debugTag);
  let masterSig <- toAXI4Lite_Master_Sig(shim.master);
  let  slaveSig <- toAXI4Lite_Slave_Sig(shim.slave);
  interface master = masterSig;
  interface  slave = slaveSig;
  interface  clear = shim.clear;
endmodule


///////////////////////////////////
// to/from "Sig" interface utils //
////////////////////////////////////////////////////////////////////////////////

// AXI4Lite Master
module toAXI4Lite_Master_Sig #(AXI4Lite_Master#(a, b, c, d, e, f, g) master)
                              (AXI4Lite_Master_Sig#(a, b, c, d, e, f, g));
  let awSig <- toAXI4Lite_AW_Master_Sig(master.aw);
  let wSig  <- toAXI4Lite_W_Master_Sig(master.w);
  let bSig  <- toAXI4Lite_B_Master_Sig(master.b);
  let arSig <- toAXI4Lite_AR_Master_Sig(master.ar);
  let rSig  <- toAXI4Lite_R_Master_Sig(master.r);
  interface aw = awSig;
  interface w  = wSig;
  interface b  = bSig;
  interface ar = arSig;
  interface r  = rSig;
endmodule

module fromAXI4Lite_Master_Sig
  #(AXI4Lite_Master_Sig#(a, b, c, d, e, f, g) master)
   (AXI4Lite_Master#(a, b, c, d, e, f, g));
  let awNoSig <- fromAXI4Lite_AW_Master_Sig(master.aw);
  let wNoSig  <- fromAXI4Lite_W_Master_Sig(master.w);
  let bNoSig  <- fromAXI4Lite_B_Master_Sig(master.b);
  let arNoSig <- fromAXI4Lite_AR_Master_Sig(master.ar);
  let rNoSig  <- fromAXI4Lite_R_Master_Sig(master.r);
  interface aw = awNoSig;
  interface w  = wNoSig;
  interface b  = bNoSig;
  interface ar = arNoSig;
  interface r  = rNoSig;
endmodule

module liftAXI4Lite_Master_Sig
  #( function AXI4Lite_Master#(a, b, c, d, e, f, g)
     f (AXI4Lite_Master#(a1, b1, c1, d1, e1, f1, g1) x)
   , AXI4Lite_Master_Sig#(a1, b1, c1, d1, e1, f1, g1) m)
   (AXI4Lite_Master_Sig#(a, b, c, d, e, f, g));
  let mNoSig <- fromAXI4Lite_Master_Sig (m);
  let ret <- toAXI4Lite_Master_Sig (f (mNoSig));
  return ret;
endmodule

// AXI4Lite Slave
module toAXI4Lite_Slave_Sig #(AXI4Lite_Slave#(a, b, c, d, e, f, g) slave)
                             (AXI4Lite_Slave_Sig#(a, b, c, d, e, f, g));
  let awSig <- toAXI4Lite_AW_Slave_Sig(slave.aw);
  let wSig  <- toAXI4Lite_W_Slave_Sig(slave.w);
  let bSig  <- toAXI4Lite_B_Slave_Sig(slave.b);
  let arSig <- toAXI4Lite_AR_Slave_Sig(slave.ar);
  let rSig  <- toAXI4Lite_R_Slave_Sig(slave.r);
  interface aw = awSig;
  interface w  = wSig;
  interface b  = bSig;
  interface ar = arSig;
  interface r  = rSig;
endmodule

module fromAXI4Lite_Slave_Sig
  #(AXI4Lite_Slave_Sig#(a, b, c, d, e, f, g) slave)
   (AXI4Lite_Slave#(a, b, c, d, e, f, g));
  let awNoSig <- fromAXI4Lite_AW_Slave_Sig(slave.aw);
  let wNoSig  <- fromAXI4Lite_W_Slave_Sig(slave.w);
  let bNoSig  <- fromAXI4Lite_B_Slave_Sig(slave.b);
  let arNoSig <- fromAXI4Lite_AR_Slave_Sig(slave.ar);
  let rNoSig  <- fromAXI4Lite_R_Slave_Sig(slave.r);
  interface aw = awNoSig;
  interface w  = wNoSig;
  interface b  = bNoSig;
  interface ar = arNoSig;
  interface r  = rNoSig;
endmodule

module liftAXI4Lite_Slave_Sig
  #( function AXI4Lite_Slave#(a, b, c, d, e, f, g)
     f (AXI4Lite_Slave#(a1, b1, c1, d1, e1, f1, g1) x)
   , AXI4Lite_Slave_Sig#(a1, b1, c1, d1, e1, f1, g1) s)
   (AXI4Lite_Slave_Sig#(a, b, c, d, e, f, g));
  let sNoSig <- fromAXI4Lite_Slave_Sig (s);
  let ret <- toAXI4Lite_Slave_Sig (f (sNoSig));
  return ret;
endmodule

/////////////////////////////
// to unguarded interfaces //
////////////////////////////////////////////////////////////////////////////////

module toUnguarded_AXI4Lite_Master #(AXI4Lite_Master#(a, b, c, d, e, f, g) m)
                                    (AXI4Lite_Master#(a, b, c, d, e, f, g));
  let u_aw <- toUnguardedSource(m.aw, ?);
  let u_w  <- toUnguardedSource(m.w, ?);
  let u_b  <- toUnguardedSink(m.b);
  let u_ar <- toUnguardedSource(m.ar, ?);
  let u_r  <- toUnguardedSink(m.r);
  interface aw = u_aw;
  interface w  = u_w;
  interface b  = u_b;
  interface ar = u_ar;
  interface r  = u_r;
endmodule

module toUnguarded_AXI4Lite_Slave #(AXI4Lite_Slave#(a, b, c, d, e, f, g) s)
                                   (AXI4Lite_Slave#(a, b, c, d, e, f, g));
  let u_aw <- toUnguardedSink(s.aw);
  let u_w  <- toUnguardedSink(s.w);
  let u_b  <- toUnguardedSource(s.b, ?);
  let u_ar <- toUnguardedSink(s.ar);
  let u_r  <- toUnguardedSource(s.r, ?);
  interface aw = u_aw;
  interface w  = u_w;
  interface b  = u_b;
  interface ar = u_ar;
  interface r  = u_r;
endmodule

function AXI4Lite_Master#(a,b,c,d,e,f,g) guard_AXI4Lite_Master
        (AXI4Lite_Master#(a,b,c,d,e,f,g) raw, Bool block) =
  interface AXI4Lite_Master;
    interface aw = guardSource(raw.aw, block);
    interface w  = guardSource(raw.w, block);
    interface b  = guardSink(raw.b, block);
    interface ar = guardSource(raw.ar, block);
    interface r  = guardSink(raw.r, block);
  endinterface;

function AXI4Lite_Slave#(a,b,c,d,e,f,g) guard_AXI4Lite_Slave
        (AXI4Lite_Slave#(a,b,c,d,e,f,g) raw, Bool block) =
  interface AXI4Lite_Slave;
    interface aw = guardSink(raw.aw, block);
    interface w  = guardSink(raw.w, block);
    interface b  = guardSource(raw.b, block);
    interface ar = guardSink(raw.ar, block);
    interface r  = guardSource(raw.r, block);
  endinterface;

///////////////////////
// addr field utils //
////////////////////////////////////////////////////////////////////////////////

function AXI4Lite_Master #(aPost,t0,t1,t2,t3,t4,t5)
         fmapAddress_AXI4Lite_Master
         ( function Bit #(aPost) f (Bit #(aPre) x)
         , AXI4Lite_Master #(aPre,t0,t1,t2,t3,t4,t5) m);
  return interface AXI4Lite_Master;
    interface Source aw;
      method drop = m.aw.drop;
      method canPeek = m.aw.canPeek;
      method peek;
        let x = m.aw.peek;
        return AXI4Lite_AWFlit { awaddr: f (x.awaddr)
                               , awprot: x.awprot
                               , awuser: x.awuser };
      endmethod
    endinterface
    interface w = m.w;
    interface b = m.b;
    interface Source ar;
      method drop = m.ar.drop;
      method canPeek = m.ar.canPeek;
      method peek;
        let x = m.ar.peek;
        return AXI4Lite_ARFlit { araddr: f (x.araddr)
                               , arprot: x.arprot
                               , aruser: x.aruser };
      endmethod
    endinterface
    interface r = m.r;
  endinterface;
endfunction

function AXI4Lite_Slave #(aPost,t0,t1,t2,t3,t4,t5)
         fmapAddress_AXI4Lite_Slave
         ( function Bit #(aPre) f (Bit #(aPost) x)
         , AXI4Lite_Slave #(aPre,t0,t1,t2,t3,t4,t5) s);
  return interface AXI4Lite_Slave;
    interface Sink aw;
      method canPut = s.aw.canPut;
      method put (x) = s.aw.put (AXI4Lite_AWFlit { awaddr: f (x.awaddr)
                                                 , awprot: x.awprot
                                                 , awuser: x.awuser });
    endinterface
    interface w = s.w;
    interface b = s.b;
    interface Sink ar;
      method canPut = s.ar.canPut;
      method put (x) = s.ar.put (AXI4Lite_ARFlit { araddr: f (x.araddr)
                                                 , arprot: x.arprot
                                                 , aruser: x.aruser });
    endinterface
    interface r = s.r;
  endinterface;
endfunction

///////////////////////
// User field utils //
////////////////////////////////////////////////////////////////////////////////

function AXI4Lite_Master #(a,b,c,d,e,f,g)
         zeroUserFields_AXI4Lite_Master
         (AXI4Lite_Master #(a,b,c_,d_,e_,f_,g_) m);
  return interface AXI4Lite_Master;
    interface Source aw;
      method drop = m.aw.drop;
      method canPeek = m.aw.canPeek;
      method peek;
        let x = m.aw.peek;
        return AXI4Lite_AWFlit { awaddr: x.awaddr
                               , awprot: x.awprot
                               , awuser: 0 };
      endmethod
    endinterface
    interface Source w;
      method drop = m.w.drop;
      method canPeek = m.w.canPeek;
      method peek;
        let x = m.w.peek;
        return AXI4Lite_WFlit { wdata: x.wdata, wstrb: x.wstrb, wuser: 0 };
      endmethod
    endinterface
    interface Sink b;
      method canPut = m.b.canPut;
      method put (x) = m.b.put (AXI4Lite_BFlit { bresp: x.bresp, buser: 0 });
    endinterface
    interface Source ar;
      method drop = m.ar.drop;
      method canPeek = m.ar.canPeek;
      method peek;
        let x = m.ar.peek;
        return AXI4Lite_ARFlit { araddr: x.araddr
                               , arprot: x.arprot
                               , aruser: 0 };
      endmethod
    endinterface
    interface Sink r;
      method canPut = m.r.canPut;
      method put (x) = m.r.put (AXI4Lite_RFlit { rdata: x.rdata
                                               , rresp: x.rresp
                                               , ruser: 0 });
    endinterface
  endinterface;
endfunction

function AXI4Lite_Slave#(a,b,c,d,e,f,g)
         zeroUserFields_AXI4Lite_Slave
         (AXI4Lite_Slave #(a,b,c_,d_,e_,f_,g_) s);
  return interface AXI4Lite_Slave;
    interface Sink aw;
      method canPut = s.aw.canPut;
      method put (x) = s.aw.put (AXI4Lite_AWFlit { awaddr: x.awaddr
                                                 , awprot: x.awprot
                                                 , awuser: 0 });
    endinterface
    interface Sink w;
      method canPut = s.w.canPut;
      method put (x) = s.w.put (AXI4Lite_WFlit { wdata: x.wdata
                                               , wstrb: x.wstrb
                                               , wuser: 0 });
    endinterface
    interface Source b;
      method drop = s.b.drop;
      method canPeek = s.b.canPeek;
      method peek = AXI4Lite_BFlit { bresp: s.b.peek.bresp, buser: 0 };
    endinterface
    interface Sink ar;
      method canPut = s.ar.canPut;
      method put (x) = s.ar.put (AXI4Lite_ARFlit { araddr: x.araddr
                                                 , arprot: x.arprot
                                                 , aruser: 0 });
    endinterface
    interface Source r;
      method drop = s.r.drop;
      method canPeek = s.r.canPeek;
      method peek;
        let x = s.r.peek;
        return AXI4Lite_RFlit { rdata: x.rdata
                              , rresp: x.rresp
                              , ruser: 0 };
      endmethod
    endinterface
  endinterface;
endfunction

/*
module mkAXI4Lite_Master_Xactor (AXI4Lite_Master_Xactor#(a, b, c, d, e, f, g));
  let shim <- mkAXI4LiteShimBypassFIFOF;
  let master <- toAXI4Lite_Master_Sig(shim.master);
  let clearing <- mkConfigReg(False);
  rule do_clear (clearing);
    shim.clear;
    clearing <= False;
  endrule
  method clear if (!clearing) = action clearing <= True; endaction;
  interface slave = guard_AXI4Lite_Slave(shim.slave, clearing);
  interface masterSig = master;
endmodule

module mkAXI4Lite_Slave_Xactor (AXI4Lite_Slave_Xactor#(a, b, c, d, e, f, g));
  let shim <- mkAXI4LiteShimBypassFIFOF;
  let slvSig <- toAXI4Lite_Slave_Sig(shim.slave);
  let clearing <- mkConfigReg(False);
  rule do_clear(clearing);
    shim.clear;
    clearing <= False;
  endrule
  method clear if (!clearing) = action clearing <= True; endaction;
  interface master = guard_AXI4Lite_Master(shim.master, clearing);
  interface slaveSig = slvSig;
endmodule
*/

// Truncate addr field of outgoing flits
function AXI4Lite_Master#(a_,b,c,d,e,f,g) truncateAddrFieldsMasterLite (AXI4Lite_Master#(a,b,c,d,e,f,g) mstr)
  provisos (Add#(a__, a_, a));
  return interface AXI4Lite_Master;
    interface aw = interface Source;
      method canPeek = mstr.aw.canPeek;
      method peek;
        AXI4Lite_AWFlit#(a, c) x = mstr.aw.peek;
        AXI4Lite_AWFlit#(a_, c) ret = ?;
        ret.awaddr = truncate(x.awaddr);
        ret.awprot = x.awprot;
        ret.awuser = x.awuser;
        return ret;
      endmethod
      method drop = mstr.aw.drop;
    endinterface;
    interface w = mstr.w;
    interface b = mstr.b;
    interface ar = interface Source;
      method canPeek = mstr.ar.canPeek;
      method peek;
        AXI4Lite_ARFlit#(a, f) x = mstr.ar.peek;
        AXI4Lite_ARFlit#(a_, f) ret = ?;
        ret.araddr = truncate(x.araddr);
        ret.arprot = x.arprot;
        ret.aruser = x.aruser;
        return ret;
      endmethod
      method drop = mstr.ar.drop;
    endinterface;
    interface r = mstr.r;
  endinterface;
endfunction

///////////////////////////////
// AXI4Lite "no route" slave //
////////////////////////////////////////////////////////////////////////////////

/*
module mkNoRouteAXI4Lite_Slave (AXI4Lite_Slave #(a,b,c,d,e,f,g));
  let noRouteWrite <- mkNoRouteSlave;
  let noRouteRead  <- mkNoRouteSlave;
  interface aw = noRouteWrite.sink;
  interface  w = nullSink;
  interface  b = noRouteWrite.source;
  interface ar = noRouteRead.sink;
  interface  r = noRouteRead.source;
endmodule
*/
