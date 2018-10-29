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
// AXI Write channel helpers //
////////////////////////////////////////////////////////////////////////////////

module mergeWrite#(
  Source#(AWFlit#(id_, addr_, awuser_)) aw,
  Source#(WFlit#(data_, wuser_)) w)
  (Source#(AXIWriteFlit#(id_, addr_, data_, awuser_, wuser_)));

  let flitLeft <- mkReg(0);
  let doGet    <- mkPulseWire;

  let outflit  = (flitLeft == 0) ?
    FirstFlit(tuple2(aw.peek, w.peek)) :
    OtherFlit(w.peek);
  let canDoGet = (flitLeft == 0) ? aw.canGet && w.canGet : w.canGet;

  rule genFirst (doGet && flitLeft == 0);
    let awflit <- aw.get;
    let _      <- w.get;
    // burst length given by AxLEN + 1
    flitLeft <= awflit.awlen;
  endrule

  rule genOther (doGet && flitLeft > 0);
    let wflit <- w.get;
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

  method peek   if (canDoGet) = outflit;
  method get    if (canDoGet) = actionvalue
    doGet.send;
    return outflit;
  endactionvalue;
  method canGet = canDoGet;
endmodule

module splitWrite#(
  Sink#(AWFlit#(id_, addr_, awuser_)) aw,
  Sink#(WFlit#(data_, wuser_)) w)
  (Sink#(AXIWriteFlit#(id_, addr_, data_, awuser_, wuser_)));

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

///////////////////////////////
// AXI Shim Master <-> Slave //
////////////////////////////////////////////////////////////////////////////////

module mkAXIShim (AXIShim#(a, b, c, d, e, f, g, h));
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

/////////////////////////////////////////
// to "Synth" version of the interface //
////////////////////////////////////////////////////////////////////////////////

// AXI Master
module toAXIMasterSynth#(AXIMaster#(a, b, c, d, e, f, g, h) master)
  (AXIMasterSynth#(a, b, c, d, e, f, g, h));
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

// AXI Slave
module toAXISlaveSynth#(AXISlave#(a, b, c, d, e, f, g, h) master)
  (AXISlaveSynth#(a, b, c, d, e, f, g, h));
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
