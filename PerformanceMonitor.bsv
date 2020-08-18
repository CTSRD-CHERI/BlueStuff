/*-
 * Copyright (c) 2020 Jonas Fiala
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

package PerformanceMonitor; 

import Vector :: *;

// A module which wants to count events must return some vector of events
// However it is easier to count the events as a Struct within a module
// So if the Struct is an instance of this class it can be returned as is
typeclass BitVectorable#(type from, numeric type n, numeric type m) dependencies (from determines (n, m));
  function Vector#(m, Bit#(n)) to_vector (from e);
endtypeclass

instance BitVectorable#(Vector#(m, Bit#(n)), n, m);
   function to_vector = id;
endinstance

// A Bit#(n) will be converted to a 1 element vector
instance BitVectorable#(Bit#(n), n, 1);
   function to_vector = replicate;
endinstance


function Vector#(mm, Bit#(nn)) to_large_vector (from e)
   provisos (BitVectorable#(from, n, m), Add#(a__, n, nn), Add#(b__, m, mm));
   return append(map(zeroExtend, to_vector(e)), replicate(0));
endfunction

// Use to_vector = struct_to_vector
// with BitVectorable instances for structs of events
// The struct must only contain Bools (or Bit#(1)) for this fn to be reasonable
function Vector#(m, Bit#(1)) struct_to_vector (from e) provisos (Bits#(from, m));
   return reverse(unpack(pack(e)));
endfunction


// Wrapper interface for reporting events of a given module
interface Monitored#(type original_module, type bitvec);
  interface original_module mdle;
  method bitvec events provisos (BitVectorable#(bitvec, n, m));
endinterface

// Write is exposed to only one counter per cycle
// Could change write_* methods to return WriteOnly Vector if needed
interface PerfCounters_IFC#(numeric type ctrs, numeric type ctrW, numeric type evts);   
  (* always_ready, always_enabled *)
  method Action send_performance_events (Vector#(evts, Bit#(ctrW)) evts);

  (* always_ready *)
  method Vector#(ctrs, ReadOnly#(Bit#(ctrW))) read_counters;
  (* always_ready *)
  method Vector#(ctrs, ReadOnly#(Bit#(TLog#(evts)))) read_ctr_sels;
  (* always_ready *)
  method ReadOnly#(Bit#(ctrs)) read_ctr_inhibit;
  (* always_ready *)
  method ReadOnly#(Bit#(ctrs)) read_ctr_overflow;

  (* always_ready *)
  method Action write_counter (Bit#(TLog#(ctrs)) idx, Bit#(ctrW) val);
  (* always_ready *)
  method Action write_ctr_sel (Bit#(TLog#(ctrs)) idx, Bit#(TLog#(evts)) val);
  (* always_ready *)
  method Action write_ctr_inhibit (Bit#(ctrs) val);
endinterface


module mkPerfCounters (PerfCounters_IFC#(ctrs, ctrW, evts));
  let ctrs = valueOf(ctrs);
  let ctrW = valueOf(ctrW);
  let evts = valueOf(evts);

  // Event counters
  Vector#(ctrs, Array#(Reg#(Bit#(ctrW))))
    vec_rg_counter <- replicateM(mkCRegU(2));
  // Which event the corresponding counter will count
  Vector#(ctrs, Reg#(Bit#(TLog#(evts))))
    vec_rg_event_sel <- replicateM(mkRegU);

  Reg#(Bit#(ctrs)) rg_ctr_inhibit <- mkReg(0);
  Wire#(Bit#(ctrs)) wr_overflow <- mkDWire(0);

  // Since [i] is not a function, cannot map it to a Vector
  function a_type sel_snd_port (Array#(a_type) arr) = arr[1];

  // Should be called every cycle with vector of how many times each event
  // occured in the given cycle. The maximum is 2^ctrW, but an event 'should'
  // never happen so many times in a cycle:
  // So map zeroExtend to the vector before passing it to this method
  method Action send_performance_events (Vector#(evts, Bit#(ctrW)) events);
    Bit#(ctrs) overflow = 0;
    for (Integer i = 0; i < ctrs; i = i + 1) begin
      // Get the number of times the selected event fired this cycle
      Bit#(ctrW) event_count = 0;
      if (vec_rg_event_sel[i] <= fromInteger(evts - 1))
      event_count = events[vec_rg_event_sel[i]];

      // Check that the given counter is not inhibited
      Bool inhibit = unpack(rg_ctr_inhibit[i]);
      // Count event
      if (!inhibit) begin
        Bit#(TAdd#(ctrW, 1)) count_sum = zeroExtend(vec_rg_counter[i][0])
                                          + zeroExtend(event_count);
        vec_rg_counter[i][0] <= truncate(count_sum);
        overflow[i] = truncateLSB(count_sum);
      end

      // $display("<time %0t, StatCounters> Counter #%2d Counting event %0d: Adding %0d to %0d (Inhibit %0d)",
      //   $time, i, vec_rg_event_sel[i], event_count, vec_rg_counter[i][0], inhibit);
    end
    wr_overflow <= overflow;
    // $display("-------------------------CLK-------------------------");
  endmethod

  method Vector#(ctrs, ReadOnly#(Bit#(ctrW))) read_counters =
    map(compose(regToReadOnly, sel_snd_port), vec_rg_counter);

  method Vector#(ctrs, ReadOnly#(Bit#(TLog#(evts)))) read_ctr_sels =
    map(regToReadOnly, vec_rg_event_sel);

  method ReadOnly#(Bit#(ctrs)) read_ctr_inhibit =
    regToReadOnly(rg_ctr_inhibit);

  method ReadOnly#(Bit#(ctrs)) read_ctr_overflow =
    regToReadOnly(wr_overflow);


  method write_counter (Bit#(TLog#(ctrs)) idx) =
    vec_rg_counter[idx][1]._write;

  method write_ctr_sel (Bit#(TLog#(ctrs)) idx) =
    vec_rg_event_sel[idx]._write;

  method write_ctr_inhibit = rg_ctr_inhibit._write;
endmodule
endpackage
