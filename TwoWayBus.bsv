/*-
 * Copyright (c) 2018-2020 Alexandre Joannou
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

package TwoWayBus;

export mkTwoWayBus;
export mkInOrderTwoWayBus;

import Routable    :: *;
import OneWayBus   :: *;
import SourceSink  :: *;
import MasterSlave :: *;

import FIFOF  :: *;
import Vector :: *;

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
    (Master#(Tuple2#(m2s_b, Vector#(nSlaves, Bool)), s2m_b));
    FIFOF#(Tuple2#(m2s_b, Vector#(nSlaves, Bool))) innerReq   <- mkFIFOF;
    FIFOF#(s2m_b)                                  noRouteRsp <- mkFIFOF;
    Slave#(m2s_a, s2m_b)                           noRoute    <- mkNoRouteSlave;
    Reg#(MasterWrapperState)                       state      <- mkReg(UNALLOCATED);
    PulseWire                                      fwdRsp     <- mkPulseWire;
    // inner signals
    Source#(m2s_a) src = m.source;
    Sink#(s2m_b) snk <- toUnguardedSink(m.sink);
    m2s_a req = src.peek;
    m2s_b fatReq = expand(req, mid);
    Vector#(nSlaves, Bool) dest = route(routingField(req));
    Bool isRoutable = countIf(id, dest) == 1;
    // consume different kinds of flits
    (* mutually_exclusive = "firstFlit, followFlits, nonRoutableFlit, drainFlits" *)
    (* mutually_exclusive = "firstFlit, followFlits, nonRoutableGenRsp, drainFlits" *)
    rule firstFlit (state == UNALLOCATED && src.canPeek && isRoutable);
      src.drop;
      innerReq.enq(tuple2(fatReq, dest));
      if (!detectLast(fatReq)) state <= ALLOCATED;
    endrule
    rule followFlits (state == ALLOCATED && src.canPeek);
      src.drop;
      innerReq.enq(tuple2(fatReq, dest));
      if (detectLast(fatReq)) state <= UNALLOCATED;
    endrule
    (* descending_urgency = "nonRoutableGenRsp, nonRoutableFlit" *)
    rule nonRoutableFlit (state == UNALLOCATED && src.canPeek && !isRoutable);
      noRoute.sink.put(req);
    endrule
    rule nonRoutableGenRsp;
      let rsp <- get(noRoute.source);
      noRouteRsp.enq(rsp);
      if (detectLast(rsp)) begin
        src.drop;
        if (!detectLast(fatReq)) state <= DRAIN;
      end
    endrule
    rule drainFlits (state == DRAIN);
      src.drop;
      if (detectLast(fatReq)) state <= UNALLOCATED;
    endrule
    // sink of responses
    rule drainNoRouteResponse (!fwdRsp && snk.canPut && noRouteRsp.notEmpty);
      noRouteRsp.deq;
      snk.put(noRouteRsp.first);
    endrule
    // interface
    interface source = toSource(innerReq);
    interface sink = interface Sink;
      method canPut   = snk.canPut;
      method put(x) if (snk.canPut) = action
        fwdRsp.send;
        snk.put(x);
      endaction;
    endinterface;
  endmodule
  Vector#(nMasters, Master#(Tuple2#(m2s_b, Vector#(nSlaves, Bool)), s2m_b))
    innerMasters = newVector;
  for (Integer i = 0; i < valueOf(nMasters); i = i + 1)
    innerMasters[i] <- wrapMaster(ms[i], fromInteger(i));

  // for each slave...
  //////////////////////////////////////////////////////////////////////////////
  module wrapSlave#(Slave#(m2s_b, s2m_a) s)
    (Slave#(m2s_b, Tuple2#(s2m_b, Vector#(nMasters, Bool))));
    // inner signals
    FIFOF#(Tuple2#(s2m_b, Vector#(nMasters, Bool))) rspBack <- mkFIFOF;
    Source#(s2m_a) src = s.source;
    Reg#(SlaveWrapperState) state <- mkReg(UNALLOCATED);
    // consume different kinds of flits
    (* mutually_exclusive = "firstFlit, followFlits" *)
    rule firstFlit (state == UNALLOCATED && src.canPeek);
      s2m_a rspFat <- get(src);
      match {.rsp, .dest} = shrink(rspFat);
      rspBack.enq(tuple2(rsp, unpack(1 << dest)));
      if (!detectLast(rsp)) state <= ALLOCATED;
    endrule
    rule followFlits (state == ALLOCATED && src.canPeek);
      s2m_a rspFat <- get(src);
      match {.rsp, .dest} = shrink(rspFat);
      rspBack.enq(tuple2(rsp, unpack(1 << dest)));
      if (detectLast(rsp)) state <= UNALLOCATED;
    endrule
    // interface
    interface sink = s.sink;
    interface source = toSource(rspBack);
  endmodule
  Vector#(nSlaves, Slave#(m2s_b, Tuple2#(s2m_b, Vector#(nMasters, Bool))))
    innerSlaves = newVector;
  for (Integer i = 0; i < valueOf(nSlaves); i = i + 1)
    innerSlaves[i] <- wrapSlave(ss[i]);

  // Master to Slave bus
  //////////////////////////////////////////////////////////////////////////////
  mkOneWayBus ( map (getMasterSource, innerMasters)
              , map (getSlaveSink, innerSlaves));
  // Slave to Master bus
  //////////////////////////////////////////////////////////////////////////////
  mkOneWayBus ( map (getSlaveSource, innerSlaves)
              , map (getMasterSink, innerMasters));

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
    (Master#(Tuple2#(UpFlit#(m2s_t, nMasters), Vector#(nSlaves, Bool)), s2m_t));
    FIFOF#(Tuple2#(UpFlit#(m2s_t, nMasters), Vector#(nSlaves, Bool)))
      innerReq <- mkFIFOF;
    FIFOF#(Maybe#(s2m_t))            innerRsp   <- mkFIFOF;
    Slave#(m2s_t, s2m_t)             noRoute    <- mkNoRouteSlave;
    Reg#(MasterWrapperState)         state      <- mkReg(UNALLOCATED);
    // inner signals
    Source#(m2s_t) src = m.source;
    Sink#(s2m_t) snk <- toUnguardedSink(m.sink);
    Vector#(nSlaves, Bool) dest = route(routingField(src.peek));
    Bool isRoutable = countIf(id, dest) == 1;
    // consume different kinds of flits
    (* mutually_exclusive = "firstFlit, followFlits, nonRoutableFlit, drainFlits" *)
    (* mutually_exclusive = "firstFlit, followFlits, nonRoutableGenRsp, drainFlits" *)
    rule firstFlit (state == UNALLOCATED && src.canPeek && isRoutable);
      let req <- get(src);
      innerReq.enq(tuple2(UpFlit {path: orig, flit: req}, dest));
      innerRsp.enq(Invalid);
      if (!detectLast(req)) state <= ALLOCATED;
    endrule
    rule followFlits (state == ALLOCATED && src.canPeek);
      let req <- get(src);
      innerReq.enq(tuple2(UpFlit {path: orig, flit: req}, dest));
      if (detectLast(req)) state <= UNALLOCATED;
    endrule
    (* descending_urgency = "nonRoutableGenRsp, nonRoutableFlit" *)
    rule nonRoutableFlit (state == UNALLOCATED && src.canPeek && !isRoutable);
      noRoute.sink.put(src.peek);
    endrule
    rule nonRoutableGenRsp;
      let rsp <- get(noRoute.source);
      innerRsp.enq(Valid(rsp));
      if (detectLast(rsp)) begin
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
    // interface
    interface source = toSource(innerReq);
    interface sink = interface Sink;
      method canPut   = innerRsp.notEmpty && !isValid(innerRsp.first) && snk.canPut;
      method put(x) if (innerRsp.notEmpty && !isValid(innerRsp.first) && snk.canPut) = action
        snk.put(x);
        if (detectLast(x)) innerRsp.deq;
      endaction;
    endinterface;
  endmodule
  Vector#(nMasters, Master#( Tuple2#( UpFlit#(m2s_t, nMasters)
                                    , Vector#(nSlaves, Bool))
                           , s2m_t)) innerMasters = newVector;
  for (Integer i = 0; i < valueOf(nMasters); i = i + 1) begin
    Vector#(nMasters, Bool) orig = replicate(False);
    orig[i] = True;
    innerMasters[i] <- wrapMaster(ms[i], orig);
  end

  // for each slave...
  //////////////////////////////////////////////////////////////////////////////
  module wrapSlave#(Slave#(m2s_t, s2m_t) s)
    (Slave#(UpFlit#(m2s_t, nMasters), Tuple2#(s2m_t,Vector#(nMasters, Bool))));
    FIFOF#(Vector#(nMasters, Bool)) routeBack <- mkFIFOF;
    let routeSrc = toSource(routeBack);
    let outCanPeek = routeSrc.canPeek && s.source.canPeek;
    // split the incomming flit
    interface sink = interface Sink;
      method canPut = s.sink.canPut && routeBack.notFull;
      //method put(x) if (s.sink.canPut && routeBack.notFull) = action
      method put(x) = action
        routeBack.enq(x.path);
        s.sink.put(x.flit);
      endaction;
    endinterface;
    interface source = interface Source;
      method canPeek = outCanPeek;
      method peek if (outCanPeek) = tuple2 (s.source.peek, routeSrc.peek);
      method drop if (outCanPeek) =
        action s.source.drop; routeSrc.drop; endaction;
    endinterface;
  endmodule
  Vector#(nSlaves, Slave#( UpFlit#(m2s_t, nMasters)
                         , Tuple2#(s2m_t,Vector#(nMasters, Bool))))
    innerSlaves = newVector;
  for (Integer i = 0; i < valueOf(nSlaves); i = i + 1)
    innerSlaves[i] <- wrapSlave(ss[i]);

  // Master to Slave bus
  //////////////////////////////////////////////////////////////////////////////
  mkOneWayBus ( map (getMasterSource, innerMasters)
              , map (getSlaveSink, innerSlaves));
  // Slave to Master bus
  //////////////////////////////////////////////////////////////////////////////
  mkOneWayBus ( map (getSlaveSource, innerSlaves)
              , map (getMasterSink, innerMasters));

endmodule

endpackage
