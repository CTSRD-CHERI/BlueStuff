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

package Interconnect;

import Printf :: *;
import Vector :: *;

import SourceSink :: *;
import MasterSlave :: *;
import OneHotArbiter :: *;
import Routable :: *;

/////////////////
// One-way bus //
////////////////////////////////////////////////////////////////////////////////
typedef enum {UNALLOCATED, ALLOCATED} BusState deriving (Bits, Eq, FShow);
module mkOneWayBus#(
    function Vector#(nRoutes, Bool) route (routing_t routing_val),
    Vector#(nIns, in_t#(flit_t)) ins,
    Vector#(nOuts, out_t#(flit_t)) outs
  ) (Empty) provisos (
    Bits#(flit_t, flit_sz),
    Routable#(flit_t, routing_t),
    Arbitrable#(in_t#(flit_t)),
    ToSource#(in_t#(flit_t), flit_t),
    ToSink#(out_t#(flit_t), flit_t),
    // assertion on argument sizes
    Add#(1, a__, nIns), // at least one source is needed
    Add#(1, b__, nOuts), // at least one sink is needed
    Add#(nRoutes, 0, nOuts) // there must be as many routes as there are sinks
  );

  ////////////////
  // Assertions //
  //////////////////////////////////////////////////////////////////////////////

  // convert arguments to Source and Sink
  let sources = map(toSource, ins);
  let   sinks = map(toSink, outs);
  // XXX XXX XXX XXX
  // The routing function must return a one hot encoded List#(Bool) of the same
  // size as the list of sinks, with True in the selected sink's position. All
  // False should mean that no route was found.

  //////////////////////////////////////
  // general helpers, state and rules //
  //////////////////////////////////////////////////////////////////////////////

  // helpers
  function Action forwardFlit (x f, Bool c, Wire#(x) w) =
    action if (c) w <= f; endaction;
  function pulse(c, w) = action if (c) w.send(); endaction;
  function Bool readPulse(PulseWire w) = w._read;
  function isReady(s)  = s.canPut;
  // for each source, potential transaction state (as a Maybe one hot dest)
  Vector#(nIns, Maybe#(Vector#(nOuts, Bool))) dests = replicate(Invalid);
  for (Integer i = 0; i < valueOf(nIns); i = i + 1) begin
    if (sources[i].canGet) begin
      let desiredSink = route(routingField(sources[i].peek));
      if (\or (zipWith(\&& , desiredSink, map(isReady, sinks))))
        dests[i] = Valid(desiredSink);
    end
  end
  // internal state
  let state        <- mkReg(UNALLOCATED);
  let arbiter      <- mkOneHotArbiter(toList(map(isValid, dests)));
  Vector#(nIns, PulseWire)      sourceSelect <- replicateM(mkPulseWire);
  Vector#(nIns, Reg#(Bool))     activeSource <- replicateM(mkReg(False));
  Vector#(nOuts, Wire#(flit_t)) flitToSink   <- replicateM(mkWire);
  Vector#(nOuts, Reg#(Bool))    activeSink   <- replicateM(mkReg(False));
  let activeSinkReady =
    \or (zipWith(\&& , map(readReg, activeSink), map(isReady, sinks)));

  // do arbitration
  rule arbitrate (state == UNALLOCATED);
    let nexts <- arbiter.next;
    let _ <- zipWithM(pulse, toVector(nexts), sourceSelect);
  endrule

  //////////////////////
  // source behaviour //
  //////////////////////////////////////////////////////////////////////////////

  // per source rules
  Rules sourceRules = emptyRules;
  for (Integer i = 0; i < valueOf(nIns); i = i + 1) begin
    sourceRules = rJoinMutuallyExclusive(sourceRules, rules
      rule source_selected (state == UNALLOCATED && sourceSelect[i]);
        if (isValid(dests[i])) begin
          let flit <- sources[i].get;
          let _ <- zipWithM(forwardFlit(flit), dests[i].Valid, flitToSink);
          if (!isLast(flit)) begin
            writeVReg(activeSource, map(readPulse, sourceSelect));
            writeVReg(activeSink, dests[i].Valid);
            state <= ALLOCATED;
          end
        end else begin // XXX THIS SHOULD NEVER HAPPEN
          $display("mkSimpleBus error: source %0d selected but dests entry"
                 + "%0d is invalid", i, i);
          $finish(0);
        end
      endrule
      rule burst (state == ALLOCATED && activeSource[i] && activeSinkReady);
        let flit <- sources[i].get;
        let _ <- zipWithM(forwardFlit(flit), map(readReg, activeSink), flitToSink);
        if (isLast(flit)) state <= UNALLOCATED;
      endrule
    endrules);
  end
  addRules(sourceRules);

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
  addRules(sinkRules);

endmodule

/////////////////
// Two-way bus //
////////////////////////////////////////////////////////////////////////////////

module mkTwoWayBus#(
    function Vector#(nRoutesM2S, Bool) routeM2S (route_m2s_t m2s_val),
    function Vector#(nRoutesS2M, Bool) routeS2M (route_s2m_t s2m_val),
    Vector#(nMasters, Master#(m2s_t, s2m_t)) ms,
    Vector#(nSlaves, Slave#(m2s_t, s2m_t)) ss
  ) (Empty) provisos (
    Bits#(m2s_t, m2s_sz), Routable#(m2s_t, route_m2s_t),
    Bits#(s2m_t, s2m_sz), Routable#(s2m_t, route_s2m_t),
    // assertion on argument sizes
    Add#(1, a__, nMasters), // at least one Master is needed
    Add#(1, b__, nSlaves), // at least one slave is needed
    Add#(nRoutesM2S, 0, nSlaves), // nRoutesM2S == nSlaves
    Add#(nRoutesS2M, 0, nMasters) // nRoutesS2M == nMasters
  );

  // Master to Slave bus
  mkOneWayBus(routeM2S, map(getMasterSource, ms), map(getSlaveSink, ss));
  // Slave to Master bus
  mkOneWayBus(routeS2M, map(getSlaveSource, ss), map(getMasterSink, ms));

endmodule

endpackage
