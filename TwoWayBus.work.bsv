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
export mkRelaxedTwoWayBus;
//export mkInOrderTwoWayBus;

import Routable    :: *;
import OneWayBus   :: *;
import SourceSink  :: *;
import MasterSlave :: *;

import FIFOF  :: *;
import Vector :: *;

/////////////////
// Two-Way Bus //
////////////////////////////////////////////////////////////////////////////////
module mkTwoWayBus #( function Vector #(nSlvs, Bool)  routeUp    (r_up_t x)
                    , function Vector #(nMsts, Bool)  routeDown  (r_down_t x)
                    , slave_t                         noRouteSlv
                    , function module #(inner_master) wrapMaster ( master_t m
                                                                 , Integer idx)
                    , function module #(inner_slave)  wrapSlave  (slave_t s)
                    , Vector #(nMsts, master_t)       ms
                    , Vector #(nSlvs,  slave_t)       ss
  ) (Empty) provisos (
    Bits #(req_t,     req_sz),       Bits #(rsp_t,     rsp_sz)
  , Bits #(inner_req, inner_req_sz), Bits #(inner_rsp, inner_rsp_sz)
  , Bits #(req_fat_t, req_fat_sz),   Bits #(rsp_fat_t, rsp_fat_sz)
  , Routable #(inner_req, r_up_t)
  , Routable #(inner_rsp, r_down_t)
    // type aliases
  , Alias #(master_t,     Master #(req_t,     rsp_t))
  , Alias #(inner_master, Master #(inner_req, inner_rsp))
  , Alias #(inner_slave,  Slave  #(inner_req, inner_rsp))
  , Alias #(slave_t,      Slave  #(req_fat_t, rsp_fat_t))
    // assertion on argument sizes
  , Add #(1, a__, nMsts) // at least one Master is needed
  , Add #(1, b__, nSlvs) // at least one slave is needed
  );

  let innerMasters    <- zipWithM  (wrapMaster, ms, genVector);
  let innerSlaves     <- mapM      (wrapSlave, ss);
  let innerNoRouteSlv <- wrapSlave (noRouteSlv);

  // Master to Slave bus
  //////////////////////////////////////////////////////////////////////////////
  mkOneWayBusNoRoute ( routeUp
                     , getSlaveSink (innerNoRouteSlv)
                     , map (getMasterSource, innerMasters)
                     , map (getSlaveSink, innerSlaves));

  // Slave to Master bus
  //////////////////////////////////////////////////////////////////////////////
  mkOneWayBus ( routeDown
              , map (getSlaveSource, cons (innerNoRouteSlv, innerSlaves))
              , map (getMasterSink, innerMasters));

endmodule

/////////////////////////
// Relaxed Two-Way Bus //
////////////////////////////////////////////////////////////////////////////////
module mkRelaxedTwoWayBus #( function Vector #(n_slaves, Bool) route (r_up_t x)
                           , Vector #(n_masters, master_t) ms
                           , Vector #(n_slaves,  slave_t) ss
  ) (Empty) provisos (
    Bits #(req_t, req_sz), Bits #(req_fat_t, req_fat_sz)
  , Bits #(rsp_t, rsp_sz), Bits #(rsp_fat_t, rsp_fat_sz)
  , Bits #(inner_rsp, inner_rsp_sz)
  , Routable #(req_fat_t, r_up_t)
  , Routable #(inner_rsp, r_down_t)
  , ExpandableReqRsp #(req_t, req_fat_t, rsp_fat_t, rsp_t, n_masters)
  , FallibleRoute #(req_fat_t, rsp_fat_t)
    // type aliases
  , Alias #(r_down_t,     Bit #(TLog #(n_masters)))
  , Alias #(inner_rsp,    WithRouteInfo #(rsp_t, r_down_t))
  , Alias #(master_t,     Master #(req_t,     rsp_t))
  , Alias #(inner_master, Master #(req_fat_t, inner_rsp))
  , Alias #(inner_slave,  Slave  #(req_fat_t, inner_rsp))
  , Alias #(slave_t,      Slave  #(req_fat_t, rsp_fat_t))
    // assertion on argument sizes
  , Add #(1, a__, n_masters) // at least one Master is needed
  , Add #(1, b__, n_slaves)  // at least one slave is needed
  );

  module wrapMaster #(master_t m, Integer idx)
                     (Master #(req_fat_t, inner_rsp));
    r_down_t mid = fromInteger (idx);
    function rsp_t fromInnerRsp (inner_rsp r) = r.payload;
    interface source = mapSource (expand (mid), m.source);
    interface sink = mapSink (fromInnerRsp, m.sink);
  endmodule

  module wrapSlave #(slave_t s) (Slave #(req_fat_t, inner_rsp));
    function toInnerRsp (r);
      match {.r_info, .rsp} = shrink (r);
      return WithRouteInfo { routeInfo: r_info, payload: rsp };
    endfunction
    interface sink = s.sink;
    interface source = mapSource (toInnerRsp, s.source);
  endmodule

  function routeRsp (x) = unpack (1 << x);
  let noRouteSlv <- mkNoRouteSlave;

  mkTwoWayBus (route, routeRsp, noRouteSlv, wrapMaster, wrapSlave, ms, ss);

endmodule

/*
module mkRelaxedTwoWayBus #( function Vector #(nSlvs, Bool) route (r_up_t x)
                           , Vector #(nMsts, master_t) ms
                           , Vector #(nSlvs,  slave_t) ss
  ) (Empty) provisos (
    Bits #(req_t, req_sz), Bits #(req_fat_t, req_fat_sz)
  , Bits #(rsp_t, rsp_sz), Bits #(rsp_fat_t, rsp_fat_sz)
  , Routable #(req_fat_t, r_up_t)
  , Routable #(rsp_inner_t, r_down_t)
  , ExpandReqRsp #(req_t, req_fat_t, rsp_fat_t, rsp_t, r_down_t)
    // type aliases
  , Alias #(r_down_t, Bit #(TLog #(nMsts)))
  , Alias #(rsp_inner_t,  Tuple2 #(rsp_t, r_down_t))
  , Alias #(master_t,     Master #(req_t,     rsp_t))
  , Alias #(inner_master, Master #(req_fat_t, rsp_inner_t))
  , Alias #(inner_slave,  Slave  #(req_fat_t, rsp_inner_t))
  , Alias #(slave_t,      Slave  #(req_fat_t, rsp_fat_t))
    // assertion on argument sizes
  , Add #(1, a__, nMsts) // at least one Master is needed
  , Add #(1, b__, nSlvs) // at least one slave is needed
  );

  module wrapMaster #(master_t m, Integer idx) (inner_master);

    FIFOF #(req_fat_t)    innerReq   <- mkFIFOF;
    FIFOF #(rsp_t)        noRouteRsp <- mkFIFOF;
    Slave #(req_t, rsp_t) noRoute    <- mkNoRouteSlave;

    PulseWire fwdRsp <- mkPulseWire;
    Reg #(MasterWrapperState) state <- mkReg(UNALLOCATED);

    // inner signals
    Source #(req_t) src = m.source;
    Sink #(rsp_t) snk <- toUnguardedSink(m.sink);
    req_t req = src.peek;
    req_fat_t fatReq = expand(req, fromInteger(idx));
    Bool isRoutable = countIf(id, route(routingField(fatReq))) == 1;
    // consume different kinds of flits
    (* mutually_exclusive = "firstFlit, followFlits, nonRoutableFlit, drainFlits" *)
    (* mutually_exclusive = "firstFlit, followFlits, nonRoutableGenRsp, drainFlits" *)
    rule firstFlit (state == UNALLOCATED && src.canPeek && isRoutable);
      src.drop;
      innerReq.enq(fatReq);
      if (!isLast(fatReq)) state <= ALLOCATED;
    endrule
    rule followFlits (state == ALLOCATED && src.canPeek);
      src.drop;
      innerReq.enq(fatReq);
      if (isLast(fatReq)) state <= UNALLOCATED;
    endrule
    (* descending_urgency = "nonRoutableGenRsp, nonRoutableFlit" *)
    rule nonRoutableFlit (state == UNALLOCATED && src.canPeek && !isRoutable);
      noRoute.sink.put(req);
    endrule
    rule nonRoutableGenRsp;
      let rsp <- get(noRoute.source);
      noRouteRsp.enq(rsp);
      if (isLast(rsp)) begin
        src.drop;
        if (!isLast(fatReq)) state <= DRAIN;
      end
    endrule
    rule drainFlits (state == DRAIN);
      src.drop;
      if (isLast(fatReq)) state <= UNALLOCATED;
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
        snk.put(compose(tpl_2, shrink)(x));
      endaction;
    endinterface;
  endmodule

  module wrapSlave #(slave_t s, Integer idx) (inner_slave);
    return s;
    // interface
    interface sink = interface Sink;
      method canPut   = snk.canPut;
      method put(x) if (snk.canPut) = action
        fwdRsp.send;
        snk.put(compose(tpl_2, shrink)(x));
      endaction;
    endinterface;
    interface source = toSource(innerReq);
  endmodule

  function routeRsp (r) = id;

  mkTwoWayBus (route, routeRsp, wrapMaster, wrapSlave, ms, ss);

endmodule
*/
/*
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
      if (!isLast(req)) state <= ALLOCATED;
    endrule
    rule followFlits (state == ALLOCATED && src.canPeek);
      let req <- get(src);
      innerReq.enq(tuple2(UpFlit {path: orig, flit: req}, dest));
      if (isLast(req)) state <= UNALLOCATED;
    endrule
    (* descending_urgency = "nonRoutableGenRsp, nonRoutableFlit" *)
    rule nonRoutableFlit (state == UNALLOCATED && src.canPeek && !isRoutable);
      noRoute.sink.put(src.peek);
    endrule
    rule nonRoutableGenRsp;
      let rsp <- get(noRoute.source);
      innerRsp.enq(Valid(rsp));
      if (isLast(rsp)) begin
        src.drop;
        if (!isLast(src.peek)) state <= DRAIN;
      end
    endrule
    rule drainFlits (state == DRAIN);
      let req <- get(src);
      if (isLast(req)) state <= UNALLOCATED;
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
        if (isLast(x)) innerRsp.deq;
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
*/

endpackage
