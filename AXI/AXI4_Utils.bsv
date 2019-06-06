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

//////////////////
// AXI4 Limiter //
////////////////////////////////////////////////////////////////////////////////

module mkAXI4Limiter#(parameter Integer maxReqs) (AXI4_Shim#(a, b, c, d, e, f, g, h));

  // req counting
  FIFOF#(Bit#(0)) limWriteFF <- mkSizedFIFOF(maxReqs);
  let pendingW <- mkReg(False);
  FIFOF#(Bit#(0)) limReadFF <- mkSizedFIFOF(maxReqs);
  // inner shim
  let shim <- mkAXI4ShimFF;
  let slv = shim.slave;
  // interfaces
  method clear = action
    limWriteFF.clear;
    pendingW <= False;
    limReadFF.clear;
    shim.clear;
  endaction;
  interface slave = interface AXI4_Slave;
    interface aw = interface Sink;
      method canPut = slv.aw.canPut && limWriteFF.notFull && !pendingW;
      //method put = slv.aw.put;
      method put if (slv.aw.canPut && limWriteFF.notFull && !pendingW) = slv.aw.put;
    endinterface;
    interface w = interface Sink;
      method canPut = slv.w.canPut && limWriteFF.notFull;
      //method put(x) = action
      method put(x) if (slv.w.canPut && limWriteFF.notFull) = action
        slv.w.put(x);
        pendingW <= !x.wlast;
        if (x.wlast) limWriteFF.enq(?);
      endaction;
    endinterface;
    interface b = interface Source;
      method canPeek = slv.b.canPeek && limWriteFF.notEmpty;
      //method peek = slv.b.peek;
      method peek if (slv.b.canPeek && limWriteFF.notEmpty) = slv.b.peek;
      //method drop = action
      method drop if (slv.b.canPeek && limWriteFF.notEmpty) = action
        limWriteFF.deq;
        slv.b.drop;
      endaction;
    endinterface;
    interface ar = interface Sink;
      method canPut = slv.ar.canPut && limReadFF.notFull;
      //method put(x) = action
      method put(x) if (slv.ar.canPut && limReadFF.notFull) = action
        limReadFF.enq(?);
        slv.ar.put(x);
      endaction;
    endinterface;
    interface r = interface Source;
      method canPeek = slv.r.canPeek && limReadFF.notEmpty;
      //method peek = slv.r.peek;
      method peek if (slv.r.canPeek && limReadFF.notEmpty) = slv.r.peek;
      //method drop = action
      method drop if (slv.r.canPeek && limReadFF.notEmpty) = action
        if (slv.r.peek.rlast) limReadFF.deq;
        slv.r.drop;
      endaction;
    endinterface;
  endinterface;
  interface master = shim.master;

endmodule

//////////////////////////////////////////
// AXI4 Burst Master <-> NonBurst Slave //
////////////////////////////////////////////////////////////////////////////////

module mkBurstToNoBurst (AXI4_Shim#(a, b, c, d, e, f, g, h))
  provisos(Add#(a__, SizeOf#(AXI4_Len), b));

  // Shims
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
  Bool allowReads  = (!inAW.canPeek && !inW.canPeek) || !lastWasRead;

  // DEBUG //
  //////////////////////////////////////////////////////////////////////////////
  Bool debug = False;
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
  rule forward_write_req (allowWrites && inAW.canPeek && inW.canPeek
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
    // DEBUG //
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
    // DEBUG //
    if (debug) $display("%0t: handle_write_rsp - ", $time, fshow(outB.peek));
  endrule

  // Reads
  //////////////////////////////////////////////////////////////////////////////
  rule forward_read_req (allowReads && inAR.canPeek && outAR.canPut);
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
    // DEBUG //
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
    // DEBUG //
    if (debug) $display("%0t: forward_read_rsp - ", $time, fshow(newflit));
  endrule

  // Interface
  //////////////////////////////////////////////////////////////////////////////
  method clear = action
    inShim.clear;
    outShim.clear;
    lastReadRspFF.clear;
    countWriteRspFF.clear;
    writesSent   <= 0;
    readsSent    <= 0;
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

module fromAXI4_Master_Synth#(AXI4_Master_Synth#(a, b, c, d, e, f, g, h) master) (AXI4_Master#(a, b, c, d, e, f, g, h));
  let aw <- fromAXI4_AW_Master_Synth(master.aw);
  let w  <- fromAXI4_W_Master_Synth(master.w);
  let b  <- fromAXI4_B_Master_Synth(master.b);
  let ar <- fromAXI4_AR_Master_Synth(master.ar);
  let r  <- fromAXI4_R_Master_Synth(master.r);
  return interface AXI4_Master;
    interface aw = aw;
    interface w  = w;
    interface b  = b;
    interface ar = ar;
    interface r  = r;
  endinterface;
endmodule

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

module fromAXI4_Slave_Synth#(AXI4_Slave_Synth#(a, b, c, d, e, f, g, h) slave) (AXI4_Slave#(a, b, c, d, e, f, g, h));
  let aw <- fromAXI4_AW_Slave_Synth(slave.aw);
  let w  <- fromAXI4_W_Slave_Synth(slave.w);
  let b  <- fromAXI4_B_Slave_Synth(slave.b);
  let ar <- fromAXI4_AR_Slave_Synth(slave.ar);
  let r  <- fromAXI4_R_Slave_Synth(slave.r);
  return interface AXI4_Slave;
    interface aw = aw;
    interface w  = w;
    interface b  = b;
    interface ar = ar;
    interface r  = r;
  endinterface;
endmodule

// Truncate addr field of incomming flits
function AXI4_Slave#(a,b,c,d,e,f,g,h) truncateAddrFields (AXI4_Slave#(a,b_,c,d,e,f,g,h) slv)
  provisos (Add#(a__, b_, b));
  return interface AXI4_Slave;
    interface aw = interface Sink;
      method canPut = slv.aw.canPut;
      method put (x) = slv.aw.put(AXI4_AWFlit {
        awid:     x.awid,
        awaddr:   truncate(x.awaddr),
        awlen:    x.awlen,
        awsize:   x.awsize,
        awburst:  x.awburst,
        awlock:   x.awlock,
        awcache:  x.awcache,
        awprot:   x.awprot,
        awqos:    x.awqos,
        awregion: x.awregion,
        awuser:   x.awuser
      });
    endinterface;
    interface w = slv.w;
    interface b = slv.b;
    interface ar = interface Sink;
      method canPut = slv.ar.canPut;
      method put (x) = slv.ar.put(AXI4_ARFlit {
        arid:     x.arid,
        araddr:   truncate(x.araddr),
        arlen:    x.arlen,
        arsize:   x.arsize,
        arburst:  x.arburst,
        arlock:   x.arlock,
        arcache:  x.arcache,
        arprot:   x.arprot,
        arqos:    x.arqos,
        arregion: x.arregion,
        aruser:   x.aruser
      });
    endinterface;
    interface r = slv.r;
  endinterface;
endfunction

////////////////////
// ID field utils //
////////////////////////////////////////////////////////////////////////////////

function AXI4_Master#(id_wide,b,c,d,e,f,g,h) extendIDFields (AXI4_Master#(id_narrow,b,c,d,e,f,g,h) m, Bit#(TSub#(id_wide, id_narrow)) id_top)
  provisos (Add#(a__, id_narrow, id_wide));
  return interface AXI4_Master;
    interface Source aw;
      method drop = m.aw.drop;
      method canPeek = m.aw.canPeek;
      method peek;
        let x = m.aw.peek;
        return AXI4_AWFlit {
          awid:     {id_top, x.awid},
          awaddr:   x.awaddr,
          awlen:    x.awlen,
          awsize:   x.awsize,
          awburst:  x.awburst,
          awlock:   x.awlock,
          awcache:  x.awcache,
          awprot:   x.awprot,
          awqos:    x.awqos,
          awregion: x.awregion,
          awuser:   x.awuser
          };
      endmethod
    endinterface
    interface Source w;
      method drop = m.w.drop;
      method canPeek = m.w.canPeek;
      method peek;
        let x = m.w.peek;
        return AXI4_WFlit {
          wdata: x.wdata,
          wstrb: x.wstrb,
          wlast: x.wlast,
          wuser: x.wuser
        };
      endmethod
    endinterface
    interface Sink b;
      method canPut = m.b.canPut;
      method put(x) = m.b.put(AXI4_BFlit {
        bid:   truncate(x.bid),
        bresp: x.bresp,
        buser: x.buser
      });
    endinterface
    interface Source ar;
      method drop = m.ar.drop;
      method canPeek = m.ar.canPeek;
      method peek;
        let x = m.ar.peek;
        return AXI4_ARFlit {
          arid:     {id_top, x.arid},
          araddr:   x.araddr,
          arlen:    x.arlen,
          arsize:   x.arsize,
          arburst:  x.arburst,
          arlock:   x.arlock,
          arcache:  x.arcache,
          arprot:   x.arprot,
          arqos:    x.arqos,
          arregion: x.arregion,
          aruser:   x.aruser
          };
      endmethod
    endinterface
    interface Sink r;
      method canPut = m.r.canPut;
      method put(x) = m.r.put(AXI4_RFlit {
        rid:   truncate(x.rid),
        rdata: x.rdata,
        rresp: x.rresp,
        rlast: x.rlast,
        ruser: x.ruser
      });
    endinterface
  endinterface;
endfunction

///////////////////////
// User field utils //
////////////////////////////////////////////////////////////////////////////////

function AXI4_Master#(a,b,c,d,e,f,g,h) zeroMasterUserFields (AXI4_Master#(a,b,c,d_,e_,f_,g_,h_) m);
  return interface AXI4_Master;
    interface Source aw;
      method drop = m.aw.drop;
      method canPeek = m.aw.canPeek;
      method peek;
        let x = m.aw.peek;
        return AXI4_AWFlit {
          awid:     x.awid,
          awaddr:   x.awaddr,
          awlen:    x.awlen,
          awsize:   x.awsize,
          awburst:  x.awburst,
          awlock:   x.awlock,
          awcache:  x.awcache,
          awprot:   x.awprot,
          awqos:    x.awqos,
          awregion: x.awregion,
          awuser:   0
          };
      endmethod
    endinterface
    interface Source w;
      method drop = m.w.drop;
      method canPeek = m.w.canPeek;
      method peek;
        let x = m.w.peek;
        return AXI4_WFlit {
          wdata: x.wdata,
          wstrb: x.wstrb,
          wlast: x.wlast,
          wuser: 0
        };
      endmethod
    endinterface
    interface Sink b;
      method canPut = m.b.canPut;
      method put(x) = m.b.put(AXI4_BFlit {
        bid:   x.bid,
        bresp: x.bresp,
        buser: 0
      });
    endinterface
    interface Source ar;
      method drop = m.ar.drop;
      method canPeek = m.ar.canPeek;
      method peek;
        let x = m.ar.peek;
        return AXI4_ARFlit {
          arid:     x.arid,
          araddr:   x.araddr,
          arlen:    x.arlen,
          arsize:   x.arsize,
          arburst:  x.arburst,
          arlock:   x.arlock,
          arcache:  x.arcache,
          arprot:   x.arprot,
          arqos:    x.arqos,
          arregion: x.arregion,
          aruser:   0
          };
      endmethod
    endinterface
    interface Sink r;
      method canPut = m.r.canPut;
      method put(x) = m.r.put(AXI4_RFlit {
        rid:   x.rid,
        rdata: x.rdata,
        rresp: x.rresp,
        rlast: x.rlast,
        ruser: 0
      });
    endinterface
  endinterface;
endfunction

// Transform a slave that expects zeroed user fields to a slave that ignores user fields
function AXI4_Slave#(a,b,c,d,e,f,g,h) zeroSlaveUserFields (AXI4_Slave#(a,b,c,d_,e_,f_,g_,h_) slv);
  return interface AXI4_Slave;
    interface Sink aw;
      method canPut = slv.aw.canPut;
      method Action put (x);
        slv.aw.put(AXI4_AWFlit {
          awid: x.awid,
          awaddr: x.awaddr,
          awlen: x.awlen,
          awsize: x.awsize,
          awburst: x.awburst,
          awlock: x.awlock,
          awcache: x.awcache,
          awprot: x.awprot,
          awqos: x.awqos,
          awregion: x.awregion,
          awuser: 0
          });
      endmethod
    endinterface
    interface Sink w;
      method canPut = slv.w.canPut;
      method Action put (x);
        slv.w.put(AXI4_WFlit {
          wdata: x.wdata,
          wstrb: x.wstrb,
          wlast: x.wlast,
          wuser: 0
        });
      endmethod
    endinterface
    interface Source b;
      method canPeek = slv.b.canPeek;
      method peek;
        return AXI4_BFlit {
          bid: slv.b.peek.bid,
          bresp: slv.b.peek.bresp,
          buser: 0
          };
      endmethod
      method drop = slv.b.drop;
    endinterface
    interface Sink ar;
      method canPut = slv.ar.canPut;
      method Action put (x);
        slv.ar.put(AXI4_ARFlit {
          arid: x.arid,
          araddr: x.araddr,
          arlen: x.arlen,
          arsize: x.arsize,
          arburst: x.arburst,
          arlock: x.arlock,
          arcache: x.arcache,
          arprot: x.arprot,
          arqos: x.arqos,
          arregion: x.arregion,
          aruser: 0
          });
      endmethod
    endinterface
    interface Source r;
      method canPeek = slv.r.canPeek;
      method peek;
        return AXI4_RFlit {
          rid: slv.r.peek.rid,
          rdata: slv.r.peek.rdata,
          rresp: slv.r.peek.rresp,
          rlast: slv.r.peek.rlast,
          ruser: 0
          };
      endmethod
      method drop = slv.r.drop;
    endinterface
  endinterface;
endfunction

///////////////////////
// Width conversions //
////////////////////////////////////////////////////////////////////////////////

typedef enum { COMBINE, PAD_FIRST, PAD_LAST } ReadSplitOption deriving (Bits, Eq, FShow);

//Module to double the data width of a slave. Assumes no bursts, data address aligned to data size.
module toWider_AXI4_Slave #(AXI4_Slave#(id_, addr_,  narrow_, awuser_, wuser_, buser_, aruser_, ruser_) narrow)
  (AXI4_Slave#(id_, addr_, wide_, awuser_, wuser_, buser_, aruser_, ruser_)) provisos (Add#(narrow_, narrow_, wide_), Add#(wide_, a__, 128), Add#(b__, SizeOf#(AXI4_Size_Bits), addr_));

  let debug = False;

  //TODO make more general: currently only works to double width
  //TODO out-of-order responses - allow multiple outstanding with same ID
  //TODO give error response if either response is an error?

  //Transactions pending at wide side
  let shim <- mkAXI4ShimBypassFIFOF;
  let in = shim.master;

  //Buffers for second halves of wide write flits (will be sent next)
  let second_aw  <- mkSizedBypassFIFOF(1);
  let second_w   <- mkSizedBypassFIFOF(1);

  //Records whether responses should be dropped (i.e. first half of what was originally 1 xaction)
  let drop_b     <- mkSizedBypassFIFOF(1);

  //Buffer for second half of ar request (will be sent next)
  let second_ar  <- mkSizedBypassFIFOF(1);

  //Records whether read responses should be combined with another response, or represent an entire
  //xaction with the first or last bits to be zero-filled
  let split_ar   <- mkSizedBypassFIFOF(1);

  //Buffers the first half of a read response (will be recombined next)
  let first_r    <- mkSizedBypassFIFOF(1);


  //Dynamic arbitration: alternate between reads and writes
  let lastWasRead <- mkReg(False);

  //Control signals
  let busy = drop_b.notEmpty || split_ar.notEmpty;
  let noPendingWrite = !in.aw.canPeek || !in.w.canPeek;
  let noPendingRead = !in.ar.canPeek;
  let allowRead = (!lastWasRead || noPendingWrite) && !busy;
  let allowWrite = (lastWasRead || noPendingRead) && !busy;

  //useful bindings
  let halfBitIdx = valueOf(TSub#(TLog#(narrow_), 3)); //Index of bit which determines which half is referred to by an address

  //determines whether a request requires two half-width requests to be sent on the narrow bus
  function Bool crossesBoundary (Bit #(addr_) addr, AXI4_Size size);
    return addr[halfBitIdx] == 1'b0 && (addr + zeroExtend(fromAXI4_Size(size)) > (addr[valueOf(addr_)-1:halfBitIdx] << halfBitIdx) + (fromInteger(valueOf(narrow_)) >> 3));
  endfunction

  function getFirstSize (addr, size) = crossesBoundary(addr, size) ? toAXI4_Size((fromInteger(valueOf(narrow_)) >> 3) - addr[halfBitIdx-1:0]) : Valid(size);
  function getSecondSize (addr, size) = toAXI4_Size(fromAXI4_Size(size) - (fromInteger(valueOf(narrow_)) >> 3) + addr[halfBitIdx:0]);

  rule send_first_aw_w (allowWrite);
    lastWasRead <= False;
    let old_aw = in.aw.peek;
    let old_w = in.w.peek;
    let requiresSplit = crossesBoundary(old_aw.awaddr, old_aw.awsize);
    let firstSize = getFirstSize(old_aw.awaddr, old_aw.awsize);
    let secondSize = getSecondSize(old_aw.awaddr, old_aw.awsize);

    if (!isValid(firstSize) || (!isValid(secondSize) && requiresSplit)) begin
      $display("Error in toWider_AXI4_Slave: split aw transactions would not have power of two size - ", fshow(old_aw));
      $finish(0);
    end

    if (!(old_aw.awlen == 0 && old_w.wlast)) begin
      $display("Error in toWider_AXI4_Slave: aw burst transaction attempted - not supported\n", fshow(old_aw), "\n", fshow(old_w));
    end

    let new_aw = old_aw;
    new_aw.awsize = firstSize.Valid;
    narrow.aw.put(new_aw);

    AXI4_WFlit #(narrow_, wuser_) new_w = AXI4_WFlit {
        wdata: requiresSplit ? old_w.wdata[valueOf(narrow_)-1:0] : (old_aw.awaddr[halfBitIdx] == 1'b0 ? old_w.wdata [valueOf(narrow_)-1:0] : old_w.wdata [valueOf(wide_)-1:valueOf(narrow_)]),
        wstrb: requiresSplit ? old_w.wstrb[(fromInteger(valueOf(narrow_)) >> 3) -1 : 0] : (old_aw.awaddr[halfBitIdx] == 1'b0 ? old_w.wstrb [(fromInteger(valueOf(narrow_)) >> 3) -1:0] : old_w.wstrb[(fromInteger(valueOf(wide_)) >> 3) -1 : fromInteger(valueOf(narrow_)) >> 3]),
        wlast: True,
        wuser: old_w.wuser
      };

    narrow.w.put(new_w);

    if (requiresSplit) begin
      let new_aw_2 = old_aw;
      new_aw_2.awsize = secondSize.Valid;
      new_aw_2.awaddr = old_aw.awaddr + (fromInteger(valueOf(narrow_)) >> 3);
      second_aw.enq(new_aw_2);

      second_w.enq(AXI4_WFlit {
        wdata: old_w.wdata[valueOf(wide_)-1:valueOf(narrow_)],
        wstrb: old_w.wstrb[(fromInteger(valueOf(wide_)) >> 3)-1: fromInteger(valueOf(narrow_))>>3],
        wlast: True,
        wuser: old_w.wuser
        });

      drop_b.enq(True);
    end else begin
      drop_b.enq(False);
    end
    in.w.drop;
    in.aw.drop;
    if (debug) begin
      $display("send_first_aw_w fired");
      $display("\told_aw: ", fshow(old_aw));
      $display("\tnew_aw: ", fshow(new_aw));
      $display("\told_w: ", fshow(old_w));
      $display("\tnew_w: ", fshow(new_w));
      $display("\trequiresSplit: ", fshow(requiresSplit));
    end
  endrule

  rule send_second_aw;
    narrow.aw.put(second_aw.first);
    second_aw.deq;
    drop_b.enq(False);
    if (debug) begin
      $display("send_second_aw fired");
      $display("\tsecond_aw.first: ", fshow(second_aw.first));
    end
  endrule

  rule send_second_w;
    narrow.w.put(second_w.first);
    second_w.deq;
    if (debug) begin
      $display("send_second_w fired");
      $display("\tsecond_w.first: ", fshow(second_w.first));
    end
  endrule

  rule take_b;
    drop_b.deq;
    narrow.b.drop;
    if (!drop_b.first) begin
      in.b.put(narrow.b.peek);
    end
    if (debug) begin
      $display("take_b fired");
      $display("\tnarrow.b.peek: ", fshow(narrow.b.peek));
      $display("\tdrop_b.first: ", fshow(drop_b.first));
    end
  endrule

  rule send_first_ar(allowRead);
    lastWasRead <= True;
    let old_ar = in.ar.peek;
    let requiresSplit = crossesBoundary(old_ar.araddr, old_ar.arsize);
    let firstSize = getFirstSize(old_ar.araddr, old_ar.arsize);
    let secondSize = getSecondSize(old_ar.araddr, old_ar.arsize);

    if (!isValid(firstSize) || (!isValid(secondSize) && requiresSplit)) begin
      $display("Error in toWider_AXI4_Slave: split ar transactions would not have power of two size - ", fshow(old_ar));
      $finish(0);
    end
    if (!(old_ar.arlen == 0)) begin
      $display("Error in toWider_AXI4_Slave: ar burst transaction attempted - not supported\n", fshow(old_ar));
    end

    let new_ar = old_ar;
    new_ar.arsize = firstSize.Valid;
    narrow.ar.put(new_ar);

    if (requiresSplit) begin
      let new_ar_2 = old_ar;
      new_ar_2.arsize = secondSize.Valid;
      new_ar_2.araddr = old_ar.araddr + (fromInteger(valueOf(narrow_)) >> 3);
      second_ar.enq(new_ar_2);
      split_ar.enq(COMBINE);
    end else if (old_ar.araddr[halfBitIdx] == 1'b1) begin
      split_ar.enq(PAD_FIRST);
    end else begin
      split_ar.enq(PAD_LAST);
    end
    in.ar.drop;
    if (debug) begin
      $display("send_first_ar fired");
      $display("\told_ar: ", fshow(old_ar));
      $display("\tnew_ar: ", fshow(new_ar));
      $display("\trequiresSplit: ", fshow(requiresSplit));
    end
  endrule

  rule send_second_ar;
    narrow.ar.put(second_ar.first);
    second_ar.deq;
    if (debug) begin
      $display("send_second_ar fired");
      $display("\tsecond_ar.first: ", fshow(second_ar.first));
    end
  endrule

  rule receive_first_r(!first_r.notEmpty);
    narrow.r.drop;
    if (!narrow.r.peek.rlast) begin
      $display("Error in toWider_AXI4_Slave: r burst transaction attempted - not supported.");
      $finish(0);
    end
    case (split_ar.first)
      COMBINE: first_r.enq(narrow.r.peek.rdata);
      PAD_FIRST: begin
        in.r.put(AXI4_RFlit {
        rid: narrow.r.peek.rid,
        rdata: {narrow.r.peek.rdata, 0},
        rresp: narrow.r.peek.rresp,
        rlast: narrow.r.peek.rlast,
        ruser: narrow.r.peek.ruser
        });
        split_ar.deq;
      end
      PAD_LAST: begin
        in.r.put(AXI4_RFlit {
        rid: narrow.r.peek.rid,
        rdata: {0, narrow.r.peek.rdata},
        rresp: narrow.r.peek.rresp,
        rlast: narrow.r.peek.rlast,
        ruser: narrow.r.peek.ruser
        });
        split_ar.deq;
      end
    endcase
    if (debug) begin
      $display("receive_first_r fired");
      $display("\tnarrow.r.peek ", fshow(narrow.r.peek));
      $display("\tsplit_ar.first ", fshow(split_ar.first));
    end
  endrule

  rule receive_second_r;
    first_r.deq;
    narrow.r.drop;
    split_ar.deq;
    if (!narrow.r.peek.rlast) begin
      $display("Error in toWider_AXI4_Slave: r burst transaction attempted - not supported.");
      $finish(0);
    end
    AXI4_RFlit #(id_, wide_, ruser_) new_r = AXI4_RFlit {
      rid: narrow.r.peek.rid,
      rdata: {narrow.r.peek.rdata, first_r.first},
      rresp: narrow.r.peek.rresp,
      rlast: narrow.r.peek.rlast,
      ruser: narrow.r.peek.ruser
      };
    in.r.put(new_r);
    if (debug) begin
      $display("receive_second_r fired");
      $display("\tnarrow.r.peek ", fshow(narrow.r.peek));
    end
  endrule

  return shim.slave;
endmodule

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

function AXI4_Master#(a,b,c,d,e,f,g,h) guard_AXI4_Master
        (AXI4_Master#(a,b,c,d,e,f,g,h) raw, Bool block) =
  interface AXI4_Master;
    interface aw = guardSource(raw.aw, block);
    interface w  = guardSource(raw.w, block);
    interface b  = guardSink(raw.b, block);
    interface ar = guardSource(raw.ar, block);
    interface r  = guardSink(raw.r, block);
  endinterface;

function AXI4_Slave#(a,b,c,d,e,f,g,h) guard_AXI4_Slave
        (AXI4_Slave#(a,b,c,d,e,f,g,h) raw, Bool block) =
  interface AXI4_Slave;
    interface aw = guardSink(raw.aw, block);
    interface w  = guardSink(raw.w, block);
    interface b  = guardSource(raw.b, block);
    interface ar = guardSink(raw.ar, block);
    interface r  = guardSource(raw.r, block);
  endinterface;

module mkAXI4_Master_Xactor (AXI4_Master_Xactor#(a, b, c, d, e, f, g, h));
  let shim <- mkAXI4ShimBypassFIFOF;
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
  let shim <- mkAXI4ShimBypassFIFOF;
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

module mkAXI4_Slave_Widening_Xactor (AXI4_Slave_Width_Xactor#(a, b, c, d, e, f, g, h, i, j, k, l, m, n)) provisos (Add#(c,c,d), Add#(d, _, 128), Add#(a__, SizeOf#(AXI4_Size_Bits), b));
  let shim <- mkAXI4ShimSizedFIFOF4;
  let widened_slave <- toWider_AXI4_Slave(shim.slave);
  let ug_slave <- toUnguarded_AXI4_Slave(zeroSlaveUserFields(widened_slave));
  let clearing <- mkReg(False);
  rule do_clear(clearing);
    shim.clear;
    clearing <= False;
  endrule
  method clear if (!clearing) = action clearing <= True; endaction;
  interface master = guard_AXI4_Master(shim.master, clearing);
  interface slaveSynth = toAXI4_Slave_Synth(ug_slave);
endmodule
