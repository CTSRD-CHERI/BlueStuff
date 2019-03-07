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

// AXI4 imports
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

////////////////////////////////
// AXI4 Write channel helpers //
////////////////////////////////////////////////////////////////////////////////

module mergeWrite#(
  Source#(AXI4_AWFlit#(id_, addr_, awuser_)) aw,
  Source#(AXI4_WFlit#(data_, wuser_)) w)
  (Source#(AXI4_WriteFlit#(id_, addr_, data_, awuser_, wuser_)));

  let flitLeft <- mkReg(0);
  let doDrop   <- mkPulseWire;

  let outflit  = (flitLeft == 0) ?
    FirstFlit(tuple2(aw.peek, w.peek)) :
    OtherFlit(w.peek);
  let newCanPeek = (flitLeft == 0) ? aw.canPeek && w.canPeek : w.canPeek;

  rule genFirst (doDrop && flitLeft == 0);
    aw.drop;
    w.drop;
    // burst length given by AxLEN + 1
    flitLeft <= aw.peek.awlen;
  endrule

  rule genOther (doDrop && flitLeft > 0);
    let wflit = w.peek;
    w.drop;
    // decrement flit counter
    flitLeft <= flitLeft - 1;
    // check for error conditions
    if (wflit.wlast && flitLeft > 1) begin
      $display("Expecting more write data flits");
      $finish(0);
    end else if (!wflit.wlast && flitLeft == 1) begin
      $display("Expecting last write data flit");
      $finish(0);
    end
  endrule

  method canPeek = newCanPeek;
  method peek if (newCanPeek) = outflit;
  method drop if (newCanPeek) = doDrop.send;
endmodule

module splitWrite#(
  Sink#(AXI4_AWFlit#(id_, addr_, awuser_)) aw,
  Sink#(AXI4_WFlit#(data_, wuser_)) w)
  (Sink#(AXI4_WriteFlit#(id_, addr_, data_, awuser_, wuser_)));

  let flitLeft <- mkReg(0);
  let doPut <- mkWire;
  let canDoPut = (flitLeft == 0) ? aw.canPut && w.canPut : w.canPut;

  rule putFirst (flitLeft == 0);
    case (doPut) matches
      tagged FirstFlit{.awflit, .wflit}: begin
        aw.put(awflit);
        w.put(wflit);
        // burst length given by AxLEN + 1
        flitLeft <= awflit.awlen;
      end
      default: begin
        $display("Expecting FirstFlit of merged write");
        $finish(0);
      end
    endcase
  endrule

  rule putOther (flitLeft > 0);
    case (doPut) matches
      tagged OtherFlit .wflit: begin
        w.put(wflit);
        // decrement flit counter
        flitLeft <= flitLeft - 1;
        // check for error conditions
        if (wflit.wlast && flitLeft > 1) begin
          $display("Expecting more write data flits");
          $finish(0);
        end else if (!wflit.wlast && flitLeft == 1) begin
          $display("Expecting last write data flit");
          $finish(0);
        end
      end
      default: begin
        $display("Expecting OtherFlit of merged write");
        $finish(0);
      end
    endcase
  endrule

  method put(x) if (canDoPut) = action doPut <= x; endaction;
  method canPut = canDoPut;

endmodule

////////////////////////////////
// AXI4 Slave addr width shim //
////////////////////////////////////////////////////////////////////////////////

function AXI4_Slave#(a, addr_out, c, d, e, f, g, h) expandAXI4_Slave_Addr
  (AXI4_Slave#(a, addr_in, c, d, e, f, g, h) s)
  provisos (Add#(a__, addr_in, addr_out)); // addr_out >= addr_in
  return interface AXI4_Slave;
    interface aw = interface Sink;
      method canPut = s.aw.canPut;
      method put(x) = s.aw.put(AXI4_AWFlit{
        awid: x.awid,
        awaddr: truncate(x.awaddr),
        awlen: x.awlen,
        awsize: x.awsize,
        awburst: x.awburst,
        awlock: x.awlock,
        awcache: x.awcache,
        awprot: x.awprot,
        awqos: x.awqos,
        awregion: x.awregion,
        awuser: x.awuser
      });
    endinterface;
    interface  w = s.w;
    interface  b = s.b;
    interface ar = interface Sink;
      method canPut = s.ar.canPut;
      method put(x) = s.ar.put(AXI4_ARFlit{
        arid: x.arid,
        araddr: truncate(x.araddr),
        arlen: x.arlen,
        arsize: x.arsize,
        arburst: x.arburst,
        arlock: x.arlock,
        arcache: x.arcache,
        arprot: x.arprot,
        arqos: x.arqos,
        arregion: x.arregion,
        aruser: x.aruser
      });
    endinterface;
    interface  r = s.r;
  endinterface;
endfunction

////////////////////////////////
// AXI4 Shim Master <-> Slave //
////////////////////////////////////////////////////////////////////////////////

// XXX
// Macro to work around the fact that we cannot pass FIFOF module constructor
// and use it for different channels with different types (need Rank2Types)...?
`define defAXI4ShimFIFOF (name, mkFF)\
module mkAXI4Shim``name (AXI4_Shim#(a, b, c, d, e, f, g, h));\
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
  interface master = interface AXI4_Master;\
    interface aw = toSource(awff);\
    interface  w = toSource(wff);\
    interface  b = toSink(bff);\
    interface ar = toSource(arff);\
    interface  r = toSink(rff);\
  endinterface;\
  interface slave = interface AXI4_Slave;\
    interface aw = toSink(awff);\
    interface  w = toSink(wff);\
    interface  b = toSource(bff);\
    interface ar = toSink(arff);\
    interface  r = toSource(rff);\
  endinterface;\
endmodule

`defAXI4ShimFIFOF(BypassFIFOF, mkBypassFIFOF)
`defAXI4ShimFIFOF(BypassFF1, mkSizedBypassFIFOF(1))
`defAXI4ShimFIFOF(FF1, mkFIFOF1)
`defAXI4ShimFIFOF(SizedFIFOF4, mkSizedFIFOF(4))
`defAXI4ShimFIFOF(UGSizedFIFOF4, mkUGSizedFIFOF(4))

module mkAXI4Shim (AXI4_Shim#(a, b, c, d, e, f, g, h));
  AXI4_Shim#(a, b, c, d, e, f, g, h) shim <- mkAXI4ShimBypassFIFOF;
  return shim;
endmodule

/////////////////////////////////////
// to/from "Synth" interface utils //
////////////////////////////////////////////////////////////////////////////////

// AXI4 Master
function AXI4_Master_Synth#(a, b, c, d, e, f, g, h)
  toAXI4_Master_Synth (AXI4_Master#(a, b, c, d, e, f, g, h) master) =
  interface AXI4_Master_Synth;
    interface aw = toAXI4_AW_Master_Synth(master.aw);
    interface w  = toAXI4_W_Master_Synth(master.w);
    interface b  = toAXI4_B_Master_Synth(master.b);
    interface ar = toAXI4_AR_Master_Synth(master.ar);
    interface r  = toAXI4_R_Master_Synth(master.r);
  endinterface;

function AXI4_Master#(a, b, c, d, e, f, g, h)
  fromAXI4_Master_Synth (AXI4_Master_Synth#(a, b, c, d, e, f, g, h) master) =
  interface AXI4_Master;
    interface aw = fromAXI4_AW_Master_Synth(master.aw);
    interface w  = fromAXI4_W_Master_Synth(master.w);
    interface b  = fromAXI4_B_Master_Synth(master.b);
    interface ar = fromAXI4_AR_Master_Synth(master.ar);
    interface r  = fromAXI4_R_Master_Synth(master.r);
  endinterface;

// AXI4 Slave
function AXI4_Slave_Synth#(a, b, c, d, e, f, g, h)
  toAXI4_Slave_Synth (AXI4_Slave#(a, b, c, d, e, f, g, h) slave) =
  interface AXI4_Slave_Synth;
    interface aw = toAXI4_AW_Slave_Synth(slave.aw);
    interface w  = toAXI4_W_Slave_Synth(slave.w);
    interface b  = toAXI4_B_Slave_Synth(slave.b);
    interface ar = toAXI4_AR_Slave_Synth(slave.ar);
    interface r  = toAXI4_R_Slave_Synth(slave.r);
  endinterface;

function AXI4_Slave#(a, b, c, d, e, f, g, h)
  fromAXI4_Slave_Synth (AXI4_Slave_Synth#(a, b, c, d, e, f, g, h) slave) =
  interface AXI4_Slave;
    interface aw = fromAXI4_AW_Slave_Synth(slave.aw);
    interface w  = fromAXI4_W_Slave_Synth(slave.w);
    interface b  = fromAXI4_B_Slave_Synth(slave.b);
    interface ar = fromAXI4_AR_Slave_Synth(slave.ar);
    interface r  = fromAXI4_R_Slave_Synth(slave.r);
  endinterface;

/////////////////////////////
// to unguarded interfaces //
////////////////////////////////////////////////////////////////////////////////
// XXX
// See SourceSink.bsv for details on shortcomings of the toUnguardedSource and
// the toUnguardedSink modules

module mkAXI4_Master_Xactor (AXI4_Master_Xactor#(a, b, c, d, e, f, g, h));
  let shim <- mkAXI4ShimUGSizedFIFOF4;
  method clear = shim.clear;
  interface slave = shim.slave;
  interface masterSynth = toAXI4_Master_Synth(shim.master);
endmodule

module toUnguarded_AXI4_Master#(AXI4_Master#(a, b, c, d, e, f, g, h) m)
  (AXI4_Master#(a, b, c, d, e, f, g, h));
  let u_aw <- toUnguardedSource(m.aw, ?);
  let u_w  <- toUnguardedSource(m.w, ?);
  let u_b  <- toUnguardedSink(m.b);
  let u_ar <- toUnguardedSource(m.ar, ?);
  let u_r  <- toUnguardedSink(m.r);
  return interface AXI4_Master;
    interface aw = u_aw;
    interface w  = u_w;
    interface b  = u_b;
    interface ar = u_ar;
    interface r  = u_r;
  endinterface;
endmodule

/*
module mkAXI4_Master_Xactor (AXI4_Master_Xactor#(a, b, c, d, e, f, g, h));
  let shim <- mkAXI4ShimSizedFIFOF4;
  let u_master <- toUnguarded_AXI4_Master(shim.master);
  //method clear = shim.clear;
  method clear = noAction;
  interface slave = shim.slave;
  interface masterSynth = toAXI4_Master_Synth(u_master);
endmodule
*/

module mkAXI4_Slave_Xactor (AXI4_Slave_Xactor#(a, b, c, d, e, f, g, h));
  let shim <- mkAXI4ShimUGSizedFIFOF4;
  method clear = shim.clear;
  interface master = shim.master;
  interface slaveSynth = toAXI4_Slave_Synth(shim.slave);
endmodule

module toUnguarded_AXI4_Slave#(AXI4_Slave#(a, b, c, d, e, f, g, h) s)
  (AXI4_Slave#(a, b, c, d, e, f, g, h));
  let u_aw <- toUnguardedSink(s.aw);
  let u_w  <- toUnguardedSink(s.w);
  let u_b  <- toUnguardedSource(s.b, ?);
  let u_ar <- toUnguardedSink(s.ar);
  let u_r  <- toUnguardedSource(s.r, ?);
  return interface AXI4_Slave;
    interface aw = u_aw;
    interface w  = u_w;
    interface b  = u_b;
    interface ar = u_ar;
    interface r  = u_r;
  endinterface;
endmodule

/*
module mkAXI4_Slave_Xactor (AXI4_Slave_Xactor#(a, b, c, d, e, f, g, h));
  let shim <- mkAXI4ShimSizedFIFOF4;
  //let shim <- mkAXI4ShimFF;
  let debug_slave = interface AXI4_Slave;
    interface aw = shim.slave.aw;
    interface w  = shim.slave.w;
    interface b  = shim.slave.b;
    interface ar = debugSink(shim.slave.ar, $format("Slave XActor - AR - shim G slave side"));
    interface r  = shim.slave.r;
  endinterface;
  let u_slave <- toUnguarded_AXI4_Slave(debug_slave);
  let debug_u_slave = interface AXI4_Slave;
    interface aw = u_slave.aw;
    interface w  = u_slave.w;
    interface b  = u_slave.b;
    interface ar = debugSink(u_slave.ar, $format("Slave XActor - AR - shim UG slave side"));
    interface r  = u_slave.r;
  endinterface;
  let debug_synth_slave = interface AXI4_Slave_Synth;
    interface aw = toAXI4_AW_Slave_Synth(debug_u_slave.aw);
    interface w  = toAXI4_W_Slave_Synth(debug_u_slave.w);
    interface b  = toAXI4_B_Slave_Synth(debug_u_slave.b);
    interface ar = onArFlit(toAXI4_AR_Slave_Synth(debug_u_slave.ar), $format("Slave XActor - AR - Synth slave side"));
    interface r  = toAXI4_R_Slave_Synth(debug_u_slave.r);
  endinterface;
  method clear = shim.clear;
  interface master = shim.master;
  interface slaveSynth = debug_synth_slave;
endmodule
*/
