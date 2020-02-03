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

package Interconnect;

import Printf :: *;
import Vector :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;

import SourceSink :: *;
import MasterSlave :: *;
import OneHotArbiter :: *;
import Routable :: *;

/////////////////
// One-way bus //
////////////////////////////////////////////////////////////////////////////////
// Simple bus receiving both a flit and a one-hot vector of Bool identifying a
// unique slave identifying the slave for which the flit is destinned.
typedef enum {UNALLOCATED, ALLOCATED} BusState deriving (Bits, Eq);
module mkOneWayBus#(
    Vector#(nIns, Tuple2#(in_t, path_t)) ins,
    Vector#(nOuts, out_t#(flit_t)) outs
  ) (Empty) provisos (
    Bits#(flit_t, flit_sz), DetectLast#(flit_t),
    ToSource#(in_t, flit_t),
    ToSource#(path_t, Vector#(nOuts, Bool)),
    ToSink#(out_t#(flit_t), flit_t),
    // assertion on argument sizes
    Add#(1, c__, nIns), // at least one source is needed
    Add#(1, d__, nOuts) // at least one sink is needed
  );

  // convert arguments to Source and Sink
  let sources = map(toSource, map(tpl_1, ins));
  let   paths = map(toSource, map(tpl_2, ins));
  let   sinks = map(toSink, outs);

  /////////////////////////////////
  // general helpers and signals //
  //////////////////////////////////////////////////////////////////////////////

  // helpers
  function Action forwardFlit (x f, Bool c, Wire#(x) w) =
    action if (c) w <= f; endaction;
  function pulse(c, w) = action if (c) w.send(); endaction;
  function Bool readPulse(PulseWire w) = w._read;
  function isReady(s)     = s.canPut;
  function isAvailable(s) = s.canPeek;
  function drainSource(s) = s.drop;
  // for each source, establish whether a transaction can occur
  Vector#(nOuts, Wire#(Bool)) isSinkReady <- replicateM(mkDWire(False));
  for (Integer i = 0; i < valueOf(nOuts); i = i + 1)
    rule checkSinkReady; isSinkReady[i] <= isReady(sinks[i]); endrule
  let sinksReady = map(readReg, isSinkReady);
  Vector#(nIns, Bool)        pathsReqs   = map(isAvailable, paths);
  Vector#(nIns, Bool)        sourcesReqs = map(isAvailable, sources);
  Vector#(nIns, Wire#(Bool)) reqWires    <- replicateM(mkDWire(False));
  for (Integer i = 0; i < valueOf(nIns); i = i + 1) begin
    rule craftReq (sources[i].canPeek && paths[i].canPeek);
      reqWires[i] <= \or (zipWith(\&& , paths[i].peek, sinksReady));
    endrule
  end
  let reqs = map(readReg, reqWires);
  // internal state
  let arbiter <- mkOneHotArbiter(toList(reqs));
  Reg#(BusState)                state        <- mkReg(UNALLOCATED);
  Vector#(nIns, PulseWire)      sourceSelect <- replicateM(mkPulseWire);
  Vector#(nIns, Reg#(Bool))     activeSource <- replicateM(mkReg(False));
  Vector#(nOuts, Wire#(flit_t)) flitToSink   <- replicateM(mkWire);
  function activeSinkReady(dest) = \or (zipWith(\&& , dest, sinksReady));

  // do arbitration
  rule arbitrate (state == UNALLOCATED);
    let nexts <- arbiter.next;
    zipWithM_(pulse, toVector(nexts), sourceSelect);
  endrule

  //////////////////////
  // source behaviour //
  //////////////////////////////////////////////////////////////////////////////

  // per input rules
  Rules sourceRules = emptyRules;
  for (Integer i = 0; i < valueOf(nIns); i = i + 1) begin
    sourceRules = rJoinMutuallyExclusive(sourceRules, rules
      (* mutually_exclusive = "source_selected, burst" *)
      rule source_selected (state == UNALLOCATED && sourceSelect[i]);
        if (paths[i].canPeek) begin
          let flit <- get(sources[i]);
          let dest = paths[i].peek;
          if (countIf(id, dest) != 1) begin // XXX THIS SHOULD NEVER HAPPEN
            $display("%0t -- mkOneWayBus error: input %0d was selected but the requested path ",
            $time, i, fshow(dest), " is not a valid one-hot path.");
            $finish(0);
          end
          zipWithM_(forwardFlit(flit), dest, flitToSink);
          if (!detectLast(flit)) begin
            writeVReg(activeSource, map(readPulse, sourceSelect));
            state <= ALLOCATED;
          end else drainSource(paths[i]);
        end else begin // XXX THIS SHOULD NEVER HAPPEN
          $display("%0t -- mkOneWayBus error: input %0d was selected but there was no requested path.",
            $time, i);
          $finish(0);
        end
      endrule
      rule burst (state == ALLOCATED && activeSource[i] &&
                  paths[i].canPeek && activeSinkReady(paths[i].peek));
        let flit <- get(sources[i]);
        zipWithM_(forwardFlit(flit), paths[i].peek, flitToSink);
        if (detectLast(flit)) begin
          state <= UNALLOCATED;
          drainSource(paths[i]);
        end
      endrule
    endrules);
  end

  ////////////////////
  // sink behaviour //
  //////////////////////////////////////////////////////////////////////////////

  // per sink rules
  Rules sinkRules = emptyRules;
  for (Integer i = 0; i < valueOf(nOuts); i = i + 1) begin
    sinkRules = rJoinMutuallyExclusive(sinkRules, rules
      rule sink_selected;
        sinks[i].put(flitToSink[i]);
      endrule
    endrules);
  end

  // add rules
  addRules(rJoinExecutionOrder(sourceRules, sinkRules));

endmodule

/////////////////
// Two-Way Bus //
////////////////////////////////////////////////////////////////////////////////
// master wrapper state enum
typedef enum {
  UNALLOCATED, ALLOCATED, DRAIN
} MasterWrapperState deriving (Bits, Eq);
// slave wrapper state enum
typedef enum {
  UNALLOCATED, ALLOCATED
} SlaveWrapperState deriving (Bits, Eq);
module mkTwoWayBus#(
    function Vector#(nRoutes, Bool) route (routing_t val),
    Vector#(nMasters, Master#(m2s_a, s2m_b)) ms,
    Vector#(nSlaves, Slave#(m2s_b, s2m_a)) ss
  ) (Empty) provisos (
    Bits#(m2s_a, m2s_a_sz), Bits#(s2m_b, s2m_b_sz),
    Bits#(m2s_b, m2s_b_sz), Bits#(s2m_a, s2m_a_sz),
    ExpandReqRsp#(m2s_a, m2s_b, s2m_a, s2m_b, Bit#(TLog#(nMasters))),
    Routable#(m2s_a, s2m_b, routing_t),
    DetectLast#(m2s_b), DetectLast#(s2m_b),
    // assertion on argument sizes
    Add#(1, a__, nMasters), // at least one Master is needed
    Add#(1, b__, nSlaves), // at least one slave is needed
    Add#(nRoutes, 0, nSlaves) // nRoutes == nSlaves
  );

  // for each master...
  //////////////////////////////////////////////////////////////////////////////
  module wrapMaster#(Master#(m2s_a, s2m_b) m, Bit#(TLog#(nMasters)) mid)
    (Tuple3#(
      Source#(m2s_b),
      Source#(Vector#(nSlaves, Bool)),
      Sink#(s2m_b)
    ));
    FIFOF#(m2s_b)                  innerReq   <- mkFIFOF;
    FIFOF#(Vector#(nSlaves, Bool)) innerRoute <- mkFIFOF;
    FIFOF#(s2m_b)                  noRouteRsp <- mkFIFOF;
    NoRouteFoundIfc#(m2s_a, s2m_b) noRoute    <- mkNoRouteFound;
    Reg#(MasterWrapperState)       state      <- mkReg(UNALLOCATED);
    PulseWire                      drainingNoRoute <- mkPulseWire;
    Wire#(s2m_b)                   rspFwd     <- mkWire;
    // inner signals
    Source#(m2s_a) src = m.source;
    Sink#(s2m_b) snk = m.sink;
    m2s_a req = src.peek;
    m2s_b fatReq = expand(req, mid);
    Vector#(nSlaves, Bool) dest = route(routingField(req));
    Bool isRoutable = countIf(id, dest) == 1;
    // consume different kinds of flits
    (* mutually_exclusive = "firstFlit, followFlits, nonRoutableFlit, drainFlits" *)
    (* mutually_exclusive = "firstFlit, followFlits, nonRoutableGenRsp, drainFlits" *)
    rule firstFlit (state == UNALLOCATED && src.canPeek && isRoutable);
      src.drop;
      innerReq.enq(fatReq);
      innerRoute.enq(dest);
      if (!detectLast(fatReq)) state <= ALLOCATED;
    endrule
    rule followFlits (state == ALLOCATED && src.canPeek);
      src.drop;
      innerReq.enq(fatReq);
      if (detectLast(fatReq)) state <= UNALLOCATED;
    endrule
    rule nonRoutableFlit (state == UNALLOCATED && src.canPeek && !isRoutable);
      noRoute.pushReq(req);
    endrule
    rule nonRoutableGenRsp;
      match {.isLast, .rsp} <- noRoute.getRsp;
      noRouteRsp.enq(rsp);
      if (isLast) begin
        src.drop;
        if (!detectLast(fatReq)) state <= DRAIN;
      end
    endrule
    rule drainFlits (state == DRAIN);
      src.drop;
      if (detectLast(fatReq)) state <= UNALLOCATED;
    endrule
    // sink of responses
    rule drainNoRouteResponse (snk.canPut && noRouteRsp.notEmpty);
      noRouteRsp.deq;
      snk.put(noRouteRsp.first);
      drainingNoRoute.send;
    endrule
    rule forwardRsp (snk.canPut && !noRouteRsp.notEmpty);
      snk.put(rspFwd);
    endrule
    Sink#(s2m_b) rsps = interface Sink;
      method canPut   = !drainingNoRoute;
      method put(x) if (!drainingNoRoute) = action rspFwd <= x; endaction;
    endinterface;
    return tuple3(toSource(innerReq), toSource(innerRoute), rsps);
  endmodule
  Vector#(nMasters, Source#(m2s_b))                  mreqs  = newVector;
  Vector#(nMasters, Source#(Vector#(nSlaves, Bool))) mpaths = newVector;
  Vector#(nMasters, Sink#(s2m_b))                    mrsps  = newVector;
  for (Integer i = 0; i < valueOf(nMasters); i = i + 1) begin
    let ifcs <- wrapMaster(ms[i], fromInteger(i));
    match {.req, .path, .rsp} = ifcs;
    mreqs[i]  = req;
    mpaths[i] = path;
    mrsps[i]  = rsp;
  end

  // for each slave...
  //////////////////////////////////////////////////////////////////////////////
  module wrapSlave#(Slave#(m2s_b, s2m_a) s) (Tuple3#(
      Sink#(m2s_b),
      Source#(Vector#(nMasters, Bool)),
      Source#(s2m_b)
    ));
    // inner signals
    FIFOF#(Vector#(nMasters, Bool)) routeBack <- mkFIFOF;
    FIFOF#(s2m_b)                   rspBack   <- mkFIFOF;
    Source#(s2m_a) src = s.source;
    Reg#(SlaveWrapperState) state <- mkReg(UNALLOCATED);
    // consume different kinds of flits
    (* mutually_exclusive = "firstFlit, followFlits" *)
    rule firstFlit (state == UNALLOCATED && src.canPeek);
      s2m_a rspFat <- get(src);
      match {.rsp, .dest} = shrink(rspFat);
      rspBack.enq(rsp);
      routeBack.enq(unpack(1 << dest));
      if (!detectLast(rsp)) state <= ALLOCATED;
    endrule
    rule followFlits (state == ALLOCATED && src.canPeek);
      s2m_a rspFat <- get(src);
      match {.rsp, .dest} = shrink(rspFat);
      rspBack.enq(rsp);
      if (detectLast(rsp)) state <= UNALLOCATED;
    endrule
    return tuple3(s.sink, toSource(routeBack), toSource(rspBack));
  endmodule
  Vector#(nSlaves, Sink#(m2s_b))                     sreqs  = newVector;
  Vector#(nSlaves, Source#(Vector#(nMasters, Bool))) spaths = newVector;
  Vector#(nSlaves, Source#(s2m_b))                   srsps  = newVector;
  for (Integer i = 0; i < valueOf(nSlaves); i = i + 1) begin
    let ifcs <- wrapSlave(ss[i]);
    match {.req, .path, .rsp} = ifcs;
    sreqs[i]  = req;
    spaths[i] = path;
    srsps[i]  = rsp;
  end

  // Master to Slave bus
  //////////////////////////////////////////////////////////////////////////////
  mkOneWayBus(zip(mreqs, mpaths), sreqs);
  // Slave to Master bus
  //////////////////////////////////////////////////////////////////////////////
  mkOneWayBus(zip(srsps, spaths), mrsps);

endmodule

//////////////////////////
// In Order Two-Way Bus //
////////////////////////////////////////////////////////////////////////////////
// type to wrap the route back to the initiating master
typedef struct {
  Vector#(n, Bool) path;
  t flit;
} UpFlit#(type t, numeric type n) deriving (Bits);
instance DetectLast#(UpFlit#(t, n)) provisos (DetectLast#(t));
  function detectLast(x) = detectLast(x.flit);
endinstance
module mkInOrderTwoWayBus#(
    function Vector#(nRoutes, Bool) route (routing_t val),
    Vector#(nMasters, Master#(m2s_t, s2m_t)) ms,
    Vector#(nSlaves, Slave#(m2s_t, s2m_t)) ss
  ) (Empty) provisos (
    Bits#(m2s_t, m2s_sz), Bits#(s2m_t, s2m_sz),
    Routable#(m2s_t, s2m_t, routing_t),
    DetectLast#(m2s_t), DetectLast#(s2m_t),
    // assertion on argument sizes
    Add#(1, a__, nMasters), // at least one Master is needed
    Add#(1, b__, nSlaves), // at least one slave is needed
    Add#(nRoutes, 0, nSlaves) // nRoutes == nSlaves
  );

  // for each master...
  //////////////////////////////////////////////////////////////////////////////
  module wrapMaster#(Master#(m2s_t, s2m_t) m, Vector#(nMasters, Bool) orig)
    (Tuple3#(
      Source#(UpFlit#(m2s_t, nMasters)),
      Source#(Vector#(nSlaves, Bool)),
      Sink#(s2m_t)
    ));
    FIFOF#(UpFlit#(m2s_t, nMasters)) innerReq   <- mkFIFOF;
    FIFOF#(Vector#(nSlaves, Bool))   innerRoute <- mkFIFOF;
    FIFOF#(Maybe#(s2m_t))            innerRsp   <- mkFIFOF;
    NoRouteFoundIfc#(m2s_t, s2m_t)   noRoute    <- mkNoRouteFound;
    Reg#(MasterWrapperState)         state      <- mkReg(UNALLOCATED);
    // inner signals
    Source#(m2s_t) src = m.source;
    Sink#(s2m_t) snk = m.sink;
    Vector#(nSlaves, Bool) dest = route(routingField(src.peek));
    Bool isRoutable = countIf(id, dest) == 1;
    // consume different kinds of flits
    (* mutually_exclusive = "firstFlit, followFlits, nonRoutableFlit, drainFlits" *)
    (* mutually_exclusive = "firstFlit, followFlits, nonRoutableGenRsp, drainFlits" *)
    rule firstFlit (state == UNALLOCATED && src.canPeek && isRoutable);
      let req <- get(src);
      innerReq.enq(UpFlit {path: orig, flit: req});
      innerRsp.enq(Invalid);
      innerRoute.enq(dest);
      if (!detectLast(req)) state <= ALLOCATED;
    endrule
    rule followFlits (state == ALLOCATED && src.canPeek);
      let req <- get(src);
      innerReq.enq(UpFlit {path: orig, flit: req});
      if (detectLast(req)) state <= UNALLOCATED;
    endrule
    rule nonRoutableFlit (state == UNALLOCATED && src.canPeek && !isRoutable);
      noRoute.pushReq(src.peek);
    endrule
    rule nonRoutableGenRsp;
      match {.isLast, .rsp} <- noRoute.getRsp;
      innerRsp.enq(Valid(rsp));
      if (isLast) begin
        src.drop;
        if (!detectLast(src.peek)) state <= DRAIN;
      end
    endrule
    rule drainFlits (state == DRAIN);
      let req <- get(src);
      if (detectLast(req)) state <= UNALLOCATED;
    endrule
    // sink of responses
    rule drainInnerResponse (innerRsp.notEmpty && isValid(innerRsp.first) && snk.canPut);
      innerRsp.deq;
      snk.put(innerRsp.first.Valid);
    endrule
    Sink#(s2m_t) rsps = interface Sink;
      method canPut   = innerRsp.notEmpty && !isValid(innerRsp.first) && snk.canPut;
      method put(x) if (innerRsp.notEmpty && !isValid(innerRsp.first) && snk.canPut) = action
        snk.put(x);
        if (detectLast(x)) innerRsp.deq;
      endaction;
    endinterface;
    return tuple3(toSource(innerReq), toSource(innerRoute), rsps);
  endmodule
  Vector#(nMasters, Source#(UpFlit#(m2s_t, nMasters))) mreqs  = newVector;
  Vector#(nMasters, Source#(Vector#(nSlaves, Bool)))   mpaths = newVector;
  Vector#(nMasters, Sink#(s2m_t))                      mrsps  = newVector;
  for (Integer i = 0; i < valueOf(nMasters); i = i + 1) begin
    Vector#(nMasters, Bool) orig = replicate(False);
    orig[i] = True;
    let ifcs <- wrapMaster(ms[i], orig);
    match {.req, .path, .rsp} = ifcs;
    mreqs[i]  = req;
    mpaths[i] = path;
    mrsps[i]  = rsp;
  end

  // for each slave...
  //////////////////////////////////////////////////////////////////////////////
  module wrapSlave#(Slave#(m2s_t, s2m_t) s) (Tuple3#(
      Sink#(UpFlit#(m2s_t,nMasters)),
      Source#(Vector#(nMasters, Bool)),
      Source#(s2m_t)
    ));
    FIFOF#(Vector#(nMasters, Bool)) routeBack <- mkFIFOF;
    // split the incomming flit
    Sink#(UpFlit#(m2s_t, nMasters)) withRouteSnk = interface Sink;
      method canPut = s.sink.canPut && routeBack.notFull;
      //method put(x) if (s.sink.canPut && routeBack.notFull) = action
      method put(x) = action
        routeBack.enq(x.path);
        s.sink.put(x.flit);
      endaction;
    endinterface;
    return tuple3(withRouteSnk, toSource(routeBack), s.source);
  endmodule
  Vector#(nSlaves, Sink#(UpFlit#(m2s_t, nMasters)))  sreqs  = newVector;
  Vector#(nSlaves, Source#(Vector#(nMasters, Bool))) spaths = newVector;
  Vector#(nSlaves, Source#(s2m_t))                   srsps  = newVector;
  for (Integer i = 0; i < valueOf(nSlaves); i = i + 1) begin
    let ifcs <- wrapSlave(ss[i]);
    match {.req, .path, .rsp} = ifcs;
    sreqs[i]  = req;
    spaths[i] = path;
    srsps[i]  = rsp;
  end

  // Master to Slave bus
  //////////////////////////////////////////////////////////////////////////////
  mkOneWayBus(zip(mreqs, mpaths), sreqs);
  // Slave to Master bus
  //////////////////////////////////////////////////////////////////////////////
  mkOneWayBus(zip(srsps, spaths), mrsps);

endmodule

endpackage
