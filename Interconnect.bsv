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
import List :: *;

import SourceSink :: *;
import OneHotArbiter :: *;

////////////////////////
// Routable typeclass //
////////////////////////////////////////////////////////////////////////////////

typeclass Routable#(type a, type b) dependencies(a determines b);
  function b    routingField (a val);
  function Bool isLast       (a val);
endtypeclass

/////////////////
// One-way bus //
////////////////////////////////////////////////////////////////////////////////
typedef enum {UNALLOCATED, ALLOCATED} BusState deriving (Bits, Eq, FShow);
module mkOneWayBus#(
    function List#(Bool) route (routing_t routing_val),
    List#(in_t#(flit_t)) ins,
    List#(out_t#(flit_t)) outs
  ) (Empty) provisos (
    Bits#(flit_t, flit_sz),
    Routable#(flit_t, routing_t),
    Arbitrable#(in_t#(flit_t)),
    ToSource#(in_t#(flit_t), flit_t),
    ToSink#(out_t#(flit_t), flit_t)
  );

  ////////////////
  // Assertions //
  //////////////////////////////////////////////////////////////////////////////

  // convert arguments to Source and Sink
  let sources = map(toSource, ins);
  let   sinks = map(toSink, outs);
  // count the number of sources and sinks
  Integer nSources = length(sources);
  if (nSources < 1)
    error(sprintf("mkBus needs at least one source (%0d provided)", nSources));
  Integer nSinks = length(sinks);
  if (nSinks < 1)
    error(sprintf("mkBus needs at least one sink (%0d provided)", nSinks));
  // XXX XXX XXX XXX
  // The routing function must return a one hot encoded List#(Bool) of the same
  // size as the list of sinks, with True in the selected sink's position. All
  // False should mean that no route was found.
  Integer nRoute = length(route(?));
  if (nSinks != nRoute)
    error(sprintf("mkBus needs the routing function to return a one hot list"
                + " with a size (%0d) equal to the number of provided sinks "
                + "(%0d)", nRoute, nSinks));

  //////////////////////////////////////
  // general helpers, state and rules //
  //////////////////////////////////////////////////////////////////////////////

  // helpers
  function writeRegList (r, v) =
    action let _ <- zipWithM(writeReg, r, v); endaction;
  function Action forwardFlit (x f, Bool c, Wire#(x) w) =
    action if (c) w <= f; endaction;
  function pulse(c, w) = action if (c) w.send(); endaction;
  function isReady(s)  = s.canPut;
  // for each source, potential transaction state (as a Maybe one hot dest)
  List#(Maybe#(List#(Bool))) dests = Nil;
  for (Integer i = 0; i < nSources; i = i + 1) begin
    let potentialDest = Invalid;
    if (sources[i].canGet) begin
      let desiredSink = route(routingField(sources[i].peek));
      if (\or (zipWith(\&& , desiredSink, map(isReady, sinks))))
        potentialDest = Valid(desiredSink);
    end
    dests = cons(potentialDest, dests);
  end
  dests = reverse(dests);
  // internal state
  let state        <- mkReg(UNALLOCATED);
  let arbiter      <- mkOneHotArbiter(map(isValid, dests));
  let sourceSelect <- replicateM(nSources, mkPulseWire);
  let activeSource <- replicateM(nSources, mkReg(False));
  let toSink       <- replicateM(nSinks, mkWire);
  let activeSink   <- replicateM(nSinks, mkReg(False));
  let activeSinkReady =
    \or (zipWith(\&& , map(readReg, activeSink), map(isReady, sinks)));

  // do arbitration
  rule arbitrate (state == UNALLOCATED);
    let nexts <- arbiter.next;
    let _ <- zipWithM(pulse, nexts, sourceSelect);
  endrule

  //////////////////////
  // source behaviour //
  //////////////////////////////////////////////////////////////////////////////

  // per source rules
  Rules sourceRules = emptyRules;
  for (Integer i = 0; i < nSources; i = i + 1) begin
    sourceRules = rJoinMutuallyExclusive(sourceRules, rules
      rule source_selected (state == UNALLOCATED && sourceSelect[i]);
        if (isValid(dests[i])) begin
          let flit <- sources[i].get;
          let _ <- zipWithM(forwardFlit(flit), dests[i].Valid, toSink);
          if (!isLast(flit)) begin
            for (Integer j = 0; j < nSources; j = j + 1) begin
              activeSource[j] <= sourceSelect[j];
              activeSink[j]   <= dests[i].Valid[j];
            end
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
        let _ <- zipWithM(forwardFlit(flit), map(readReg, activeSink), toSink);
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
  for (Integer i = 0; i < nSinks; i = i + 1) begin
    sinkRules = rJoinMutuallyExclusive(sinkRules, rules
      rule sink_selected;
        sinks[i].put(toSink[i]);
      endrule
    endrules);
  end
  addRules(sinkRules);

endmodule

endpackage
