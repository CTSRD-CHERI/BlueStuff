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

import List :: *;
import Vector :: *;
import Printf :: *;

import AXI4_Types :: *;
import SourceSink :: *;
import ListExtra :: *;
import Interconnect :: *;
import Routable :: *;

///////////////////////////////
// AXI write channel helpers //
////////////////////////////////////////////////////////////////////////////////

typedef union tagged {
  Tuple2#(AWFlit#(id_, addr_, user_), WFlit#(data_, user_)) FirstFlit;
  WFlit#(data_, user_) OtherFlit;
} MergedWriteFlit#(
  numeric type id_,
  numeric type addr_,
  numeric type data_,
  numeric type user_) deriving (Bits);
instance Routable#(MergedWriteFlit#(id_, addr_, data_, user_), Bit#(addr_));
  function routingField(x) = case (x) matches
    tagged FirstFlit {.aw, .w}: routingField(aw);
    default: ?;
  endcase;
  function isLast(x) = case (x) matches
    tagged FirstFlit {.aw, .w}: w.wlast;
    tagged OtherFlit .w: w.wlast;
  endcase;
endinstance

module mergeWrite#(
  Source#(AWFlit#(id_, addr_, user_)) aw,
  Source#(WFlit#(data_, user_)) w)
  (Source#(MergedWriteFlit#(id_, addr_, data_, user_)));

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
  Sink#(AWFlit#(id_, addr_, user_)) aw,
  Sink#(WFlit#(data_, user_)) w)
  (Sink#(MergedWriteFlit#(id_, addr_, data_, user_)));

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

///////////////////
// Other helpers //
////////////////////////////////////////////////////////////////////////////////

function AWFlit#(sid_, addr_, user_) prependAWMID (
  AWFlit#(id_, addr_, user_) flit,
  Bit#(n) mid) provisos (Add#(id_, n, sid_));
  AWFlit#(sid_, addr_, user_) newFlit = ?;
  newFlit.awid     = {mid, flit.awid};
  newFlit.awaddr   = flit.awaddr;
  newFlit.awlen    = flit.awlen;
  newFlit.awsize   = flit.awsize;
  newFlit.awburst  = flit.awburst;
  newFlit.awlock   = flit.awlock;
  newFlit.awcache  = flit.awcache;
  newFlit.awprot   = flit.awprot;
  newFlit.awqos    = flit.awqos;
  newFlit.awregion = flit.awregion;
  newFlit.awuser   = flit.awuser;
  return newFlit;
endfunction
function Source#(AWFlit#(sid_, addr_, user_)) widenAW (
  Source#(AWFlit#(id_, addr_, user_)) src,
  Bit#(n) mid) provisos (Add#(id_, n, sid_)) = interface Source;
    method canGet = src.canGet;
    method peek   = prependAWMID(src.peek, mid);
    method get    = actionvalue
      let flit <- src.get;
      return prependAWMID(flit, mid);
    endactionvalue;
  endinterface;

function BFlit#(id_, user_) stripBMID (BFlit#(sid_, user_) flit)
  provisos (Add#(id_, n, sid_));
  BFlit#(id_, user_) newFlit = ?;
  newFlit.bid   = truncate(flit.bid);
  newFlit.bresp = flit.bresp;
  newFlit.buser = flit.buser;
  return newFlit;
endfunction
function Sink#(BFlit#(sid_, user_)) widenB (Sink#(BFlit#(id_, user_)) snk)
  provisos (Add#(id_, n, sid_)) = interface Sink;
    method canPut = snk.canPut;
    method put(x) = snk.put(stripBMID(x));
  endinterface;

function ARFlit#(sid_, addr_, user_) prependARMID (
  ARFlit#(id_, addr_, user_) flit,
  Bit#(n) mid) provisos (Add#(id_, n, sid_));
  ARFlit#(sid_, addr_, user_) newFlit = ?;
  newFlit.arid     = {mid, flit.arid};
  newFlit.araddr   = flit.araddr;
  newFlit.arlen    = flit.arlen;
  newFlit.arsize   = flit.arsize;
  newFlit.arburst  = flit.arburst;
  newFlit.arlock   = flit.arlock;
  newFlit.arcache  = flit.arcache;
  newFlit.arprot   = flit.arprot;
  newFlit.arqos    = flit.arqos;
  newFlit.arregion = flit.arregion;
  newFlit.aruser   = flit.aruser;
  return newFlit;
endfunction
function Source#(ARFlit#(sid_, addr_, user_)) widenAR (
  Source#(ARFlit#(id_, addr_, user_)) src,
  Bit#(n) mid) provisos (Add#(id_, n, sid_)) = interface Source;
    method canGet = src.canGet;
    method peek   = prependARMID(src.peek, mid);
    method get    = actionvalue
      let flit <- src.get;
      return prependARMID(flit, mid);
    endactionvalue;
  endinterface;

function RFlit#(id_, data_, user_) stripRMID (RFlit#(sid_, data_, user_) flit)
  provisos (Add#(id_, n, sid_));
  RFlit#(id_, data_, user_) newFlit = ?;
  newFlit.rid   = truncate(flit.rid);
  newFlit.rdata = flit.rdata;
  newFlit.rresp = flit.rresp;
  newFlit.rlast = flit.rlast;
  newFlit.ruser = flit.ruser;
  return newFlit;
endfunction
function Sink#(RFlit#(sid_, data_, user_)) widenR (
  Sink#(RFlit#(id_, data_, user_)) snk)
  provisos (Add#(id_, n, sid_)) = interface Sink;
    method canPut = snk.canPut;
    method put(x) = snk.put(stripRMID(x));
  endinterface;

/////////////
// AXI bus //
////////////////////////////////////////////////////////////////////////////////

module mkAXIBus#(
    MappingTable#(nRoutes, addr_) maptab,
    Vector#(nMasters, AXIMaster#(id_, addr_, data_, user_)) masters,
    Vector#(nSlaves, AXISlave#(sid_, addr_, data_, user_)) slaves
  ) (Empty) provisos (
    Add#(id_, TLog#(nMasters), sid_),
    // assertion on argument sizes
    Add#(1, a__, nMasters), // at least one master is needed
    Add#(1, b__, nSlaves), // at least one slave is needed
    Add#(nRoutes, 0, nSlaves) // nRoutes == nSlaves
  );

  // count the number of masters and slaves
  if (valueOf(nMasters) < 1)
    error(sprintf("mkAXIBus needs at least one master (%0d provided)", valueOf(nMasters)));
  if (valueOf(nSlaves) < 1)
    error(sprintf("mkAXIBus needs at least one slave (%0d provided)", valueOf(nSlaves)));

  // prepare masters
  Vector#(nMasters, Source#(MergedWriteFlit#(sid_, addr_, data_, user_))) write_masters = newVector;
  Vector#(nMasters, Sink#(BFlit#(sid_, user_))) b_masters = newVector;
  Vector#(nMasters, Source#(ARFlit#(sid_, addr_, user_))) ar_masters = newVector;
  Vector#(nMasters, Sink#(RFlit#(sid_, data_, user_))) r_masters = newVector;
  for (Integer i = 0; i < valueOf(nMasters); i = i + 1) begin
    Bit#(TLog#(nMasters)) mid = fromInteger(i);
    // merge from write masters
    let new_master <- mergeWrite(widenAW(masters[i].aw, mid), masters[i].w);
    write_masters[i] = new_master;
    b_masters[i]     = widenB(masters[i].b);
    ar_masters[i]    = widenAR(masters[i].ar, mid);
    r_masters[i]     = widenR(masters[i].r);
  end

  // prepare slaves
  Vector#(nSlaves, Sink#(MergedWriteFlit#(sid_, addr_, data_, user_))) write_slaves = newVector;
  Vector#(nSlaves, Source#(BFlit#(sid_, user_))) b_slaves = newVector;
  Vector#(nSlaves, Sink#(ARFlit#(sid_, addr_, user_))) ar_slaves = newVector;
  Vector#(nSlaves, Source#(RFlit#(sid_, data_, user_))) r_slaves = newVector;
  for (Integer i = 0; i < valueOf(nSlaves); i = i + 1) begin  
    // split to write slaves
    let new_slave <- splitWrite(slaves[i].aw, slaves[i].w);
    write_slaves[i] = new_slave;
    b_slaves[i]     = slaves[i].b;
    ar_slaves[i]    = slaves[i].ar;
    r_slaves[i]     = slaves[i].r;
  end

  // routing function from slave to master
  function Vector#(nMasters, Bool) routeBack(Bit#(sid_) x);
    Bit#(nMasters) dest = 1;
    Bit#(TLog#(nMasters)) y = truncateLSB(x); 
    return unpack(dest << y);
  endfunction
  // connect with standard busses
  mkOneWayBus(routeFromMappingTable(maptab), write_masters, write_slaves);
  mkOneWayBus(routeBack, b_slaves, b_masters);
  mkOneWayBus(routeFromMappingTable(maptab), ar_masters, ar_slaves);
  mkOneWayBus(routeBack, r_slaves, r_masters);

endmodule
