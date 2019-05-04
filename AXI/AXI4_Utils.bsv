/*-
 * Copyright (c) 2018-2019 Alexandre Joannou
 * Copyright (c) 2019 Peter Rugg
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
import AXI4_AXI4Lite_Types :: *;
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

//////////////////////////////////////////
// AXI4 Burst Master <-> NonBurst Slave //
////////////////////////////////////////////////////////////////////////////////

module mkBurstToNoBurst#(Bool debug) (AXI4_Shim#(a, b, c, d, e, f, g, h))
  provisos(Add#(a__, SizeOf#(AXI4_Len), b));

  // Shims
  //let inShim <- mkAXI4ShimUGSizedFIFOF4;
  //let outShim <- mkAXI4ShimUGSizedFIFOF4;
  let inShim <- mkAXI4ShimBypassFIFOF;
  let outShim <- mkAXI4ShimBypassFIFOF;
  // handy names
  let inAW = inShim.master.aw;
  let inW  = inShim.master.w;
  let inB  = inShim.master.b;
  let inAR = inShim.master.ar;
  let inR  = inShim.master.r;
  let outAW = outShim.slave.aw;
  let outW  = outShim.slave.w;
  let outB  = outShim.slave.b;
  let outAR = outShim.slave.ar;
  let outR  = outShim.slave.r;
  // internal state
  let lastReadRspFF   <- mkSizedFIFOF(4);
  let countWriteRspFF <- mkSizedFIFOF(4);
  Reg#(Bit#(SizeOf#(AXI4_Len))) writesSent <- mkReg(0);
  Reg#(Bit#(SizeOf#(AXI4_Len))) readsSent <- mkReg(0);
  Reg#(Bit#(SizeOf#(AXI4_Len))) flitReceived <- mkReg(0);

  // To guaranty atomicity of bursts, require that we only deal with one read or
  // one write at a time...
  // Dynamic priority handling
  Reg#(Bool) lastWasRead <- mkReg(False);

  // helper functions
  function getFlitAddr(addr, size, burst, cnt) = case (burst)
    INCR: return addr + (zeroExtend(cnt) << pack(size));
    default: return addr;
  endcase;
  Bool allowWrites = !inAR.canPeek || lastWasRead;
  //Bool allowWrites = (!inAR.canPeek || lastWasRead) && !lastReadRspFF.notEmpty;
  Bool allowReads  = (!inAW.canPeek && !inW.canPeek) || !lastWasRead;

  (* fire_when_enabled *)
  rule dbg (debug);
    Fmt dbg_str = $format("inAW.canPeek:\t ", fshow(inAW.canPeek))
                + $format("\toutAW.canPut:\t ", fshow(outAW.canPut))
                + $format("\n\tinW.canPeek:\t ", fshow(inW.canPeek))
                + $format("\toutW.canPut:\t ", fshow(outW.canPut))
                + $format("\n\tinB.canPut:\t ", fshow(inB.canPut))
                + $format("\toutB.canPeek:\t ", fshow(outB.canPeek))
                + $format("\n\tinAR.canPeek:\t ", fshow(inAR.canPeek))
                + $format("\toutAR.canPut:\t ", fshow(outAR.canPut))
                + $format("\n\tinR.canPut:\t ", fshow(inR.canPut))
                + $format("\toutR.canPeek:\t ", fshow(outR.canPeek));
    Fmt state_str = $format("lastWasRead: ", fshow(lastWasRead),
                            " allowReads: ", fshow(allowReads),
                            " allowWrites: ", fshow(allowWrites),
                            "\nwritesSent: %d", writesSent,
                            " readsSent: %d", readsSent,
                            " flitReceived: %d", flitReceived);
    $display("%0t: ", $time, dbg_str);
    $display("%0t: ", $time, state_str);
  endrule
  (* fire_when_enabled *)
  rule dbgff (debug);
    Fmt state_str = $format("countWriteRspFF - notFull: ", fshow(countWriteRspFF.notFull),
                            " notEmpty: ", fshow(countWriteRspFF.notEmpty),
                            " first: %d", countWriteRspFF.first)
                  + $format("\n\tlastReadRspFF - notFull: ", fshow(lastReadRspFF.notFull),
                            " notEmpty: ", fshow(lastReadRspFF.notEmpty),
                            " first: ", fshow(lastReadRspFF.first));
    $display("%0t: ", $time, state_str);
  endrule
  // Writes
  //////////////////////////////////////////////////////////////////////////////
  rule forward_write_req (allowWrites
                          && inAW.canPeek && inW.canPeek
                          && outAW.canPut && outW.canPut);
    // prepare new AW request flit
    let awflit = inAW.peek;
    let newawflit = awflit;
    newawflit.awaddr = getFlitAddr(awflit.awaddr, awflit.awsize, awflit.awburst,
                                   writesSent);
    newawflit.awlen = 0;
    newawflit.awburst = FIXED;
    // prepare new W request flit
    let newwflit = inW.peek;
    newwflit.wlast = True;
    // drop from W input and produce a AW/W output
    inW.drop;
    outAW.put(newawflit);
    outW.put(newwflit);
    // book keeping
    if (inW.peek.wlast) begin
      inAW.drop;
      countWriteRspFF.enq(awflit.awlen);
      writesSent <= 0;
    end else writesSent <= writesSent + 1;
    if (debug) $display("%0t: forward_write_req", $time,
                        "\n", fshow(awflit), "\n", fshow(inW.peek),
                        "\n", fshow(newawflit), "\n", fshow(newwflit));
  endrule
  rule handle_write_rsp (allowWrites && outB.canPeek && inB.canPut);
    // always consume the response
    outB.drop;
    // count up if not last
    if (countWriteRspFF.first > flitReceived) flitReceived <= flitReceived + 1;
    // on last response, forward it and reset book keeping
    else begin
      countWriteRspFF.deq;
      inB.put(outB.peek);
      flitReceived <= 0;
      lastWasRead <= False;
    end
    if (debug) $display("%0t: handle_write_rsp - ", $time, fshow(outB.peek));
  endrule

  // Reads
  //////////////////////////////////////////////////////////////////////////////
  rule forward_read_req (allowReads
                         && inAR.canPeek && outAR.canPut);
    // prepare new request flit
    let arflit = inAR.peek;
    let newflit = arflit;
    newflit.araddr = getFlitAddr(arflit.araddr, arflit.arsize, arflit.arburst,
                                 readsSent);
    newflit.arlen = 0;
    newflit.arburst = FIXED;
    // is this the last request ?
    let isLast = (readsSent == arflit.arlen);
    // produce a AR output
    outAR.put(newflit);
    lastReadRspFF.enq(isLast);
    // book keeping
    if (isLast) begin
      inAR.drop;
      readsSent <= 0;
    end else readsSent <= readsSent + 1;
    if (debug) $display("%0t: forward_read_req", $time,
                        "\n", fshow(arflit), "\n", fshow(newflit),
                        "\nisLast: ", fshow(isLast),
                        " readsSent: %0d", readsSent);
  endrule
  rule forward_read_rsp (allowReads && outR.canPeek && inR.canPut);
    let isLast = lastReadRspFF.first;
    // prepare new response flit
    let newflit   = outR.peek;
    newflit.rlast = isLast;
    // consume and produce on AXI
    outR.drop;
    inR.put(newflit);
    // book keeping
    lastReadRspFF.deq;
    if (isLast) lastWasRead <= True;
    if (debug) $display("%0t: forward_read_rsp - ", $time, fshow(newflit));
  endrule

  // Interface
  //////////////////////////////////////////////////////////////////////////////
  method clear = action
    inShim.clear;
    outShim.clear;
    lastReadRspFF.clear;
    countWriteRspFF.clear;
    writesSent <= 0;
    readsSent <= 0;
    flitReceived <= 0;
  endaction;
  interface slave  = inShim.slave;
  interface master = outShim.master;

endmodule

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
`defAXI4ShimFIFOF(FF, mkFIFOF)
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

function AXI4_Master#(a,b,c,d,e,f,g,h) guard_AXI4_Master (AXI4_Master#(a,b,c,d,e,f,g,h) raw, Bool block) = interface AXI4_Master;
  interface aw = guardSource(raw.aw, block);
  interface w  = guardSource(raw.w, block);
  interface b  = guardSink(raw.b, block);
  interface ar = guardSource(raw.ar, block);
  interface r  = guardSink(raw.r, block);
endinterface;

function AXI4_Slave#(a,b,c,d,e,f,g,h) guard_AXI4_Slave (AXI4_Slave#(a,b,c,d,e,f,g,h) raw, Bool block) = interface AXI4_Slave;
  interface aw = guardSink(raw.aw, block);
  interface w  = guardSink(raw.w, block);
  interface b  = guardSource(raw.b, block);
  interface ar = guardSink(raw.ar, block);
  interface r  = guardSource(raw.r, block);
endinterface;

module mkAXI4_Master_Xactor (AXI4_Master_Xactor#(a, b, c, d, e, f, g, h));
  let shim <- mkAXI4ShimSizedFIFOF4;
  let ug_master <- toUnguarded_AXI4_Master(shim.master);
  let clearing <- mkReg(False);
  rule do_clear (clearing);
    shim.clear;
    clearing <= False;
  endrule
  method clear if (!clearing) = action clearing <= True; endaction;
  interface slave = guard_AXI4_Slave(shim.slave, clearing);
  interface masterSynth = toAXI4_Master_Synth(ug_master);
endmodule

module mkAXI4_Slave_Xactor (AXI4_Slave_Xactor#(a, b, c, d, e, f, g, h));
  let shim <- mkAXI4ShimSizedFIFOF4;
  let ug_slave <- toUnguarded_AXI4_Slave(shim.slave);
  let clearing <- mkReg(False);
  rule do_clear(clearing);
    shim.clear;
    clearing <= False;
  endrule
  method clear if (!clearing) = action clearing <= True; endaction;
  interface master = guard_AXI4_Master(shim.master, clearing);
  interface slaveSynth = toAXI4_Slave_Synth(ug_slave);
endmodule
