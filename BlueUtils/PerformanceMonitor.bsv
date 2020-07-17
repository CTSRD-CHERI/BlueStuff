package PerformanceMonitor; 

import Vector :: * ;

// A module which wants to count events must return some vector of events
// However it is easier to count the events as a Struct within a module
// So if the Struct is an instance of this class it can be returned as is
typeclass EventsList#(type evts, numeric type n, numeric type m);
  function Vector#(m, Bit#(n)) getList (evts e);
endtypeclass

// Write is exposed to only one counter per cycle
// Could change write_* methods to return WriteOnly Vector if needed
interface PerfCounters_IFC #(numeric type ctrs, numeric type ctrW, numeric type evts);   
  (* always_ready, always_enabled *)
  method Action send_performance_events (Vector #(evts, Bit #(ctrW)) evts);

  (* always_ready *)
  method Vector #(ctrs, ReadOnly #(Bit #(ctrW))) read_counters;
  (* always_ready *)
  method Vector #(ctrs, ReadOnly #(Bit #(TLog #(evts)))) read_ctr_sels;
  (* always_ready *)
  method ReadOnly #(Bit #(ctrs)) read_ctr_inhibit;
  (* always_ready *)
  method ReadOnly #(Bit #(ctrs)) read_ctr_overflow;

  (* always_ready *)
  method Action write_counter (Bit #(TLog #(ctrs)) idx, Bit #(ctrW) val);
  (* always_ready *)
  method Action write_ctr_sel (Bit #(TLog #(ctrs)) idx, Bit #(TLog #(evts)) val);
  (* always_ready *)
  method Action write_ctr_inhibit (Bit #(ctrs) val);
endinterface


module mkPerfCounters_Core (PerfCounters_IFC #(ctrs, ctrW, evts))
  provisos (Add#(a__, TDiv#(ctrW, 2), ctrW));
  let ctrs = valueOf(ctrs);
  let ctrW = valueOf(ctrW);
  let evts = valueOf(evts);

  // Event counters
  Vector #(ctrs, Array #(Reg #(Bit #(ctrW))))
    vec_rg_counter <- replicateM(mkCRegU(2));
  // Which event the corresponding counter will count
  Vector #(ctrs, Reg #(Bit #(TLog #(evts))))
    vec_rg_event_sel <- replicateM(mkRegU);

  Reg #(Bit #(ctrs)) rg_ctr_inhibit <- mkReg(0);
  Wire #(Bit #(ctrs)) wr_overflow <- mkDWire(0);

  // Since [i] is not a function, cannot map it to a Vector
  function a_type sel_snd_port (Array# (a_type) arr) = arr[1];

  // Should be called every cycle with vector of how many times each event
  // occured in the given cycle. The maximum is 2^ctrW, but an event 'should'
  // never happen so many times in a cycle:
  // So map zeroExtend to the vector before passing it to this method
  method Action send_performance_events (Vector #(evts, Bit #(ctrW)) events);
    Bit #(ctrs) overflow = 0;
    for (Integer i = 0; i < ctrs; i = i + 1) begin
      // Get the number of times the selected event fired this cycle
      Bit #(ctrW) event_count = 0;
      if (vec_rg_event_sel[i] <= fromInteger(evts - 1))
      event_count = events[vec_rg_event_sel[i]];

      // Check that the given counter is not inhibited
      Bool inhibit = unpack(rg_ctr_inhibit[i]);
      // Count event
      if (!inhibit) begin
        Bit #(TAdd #(ctrW, 1)) count_sum = zeroExtend (vec_rg_counter[i][0])
                                          + zeroExtend (event_count);
        vec_rg_counter[i][0] <= truncate (count_sum);
        overflow[i] = truncateLSB (count_sum);
      end

      // $display("<time %0t, StatCounters> Counter #%2d Counting event %0d: Adding %0d to %0d (Inhibit %0d)",
      //   $time, i, vec_rg_event_sel[i], event_count, vec_rg_counter[i][0], inhibit);
    end
    wr_overflow <= overflow;
    // $display("-------------------------CLK-------------------------");
  endmethod

  method Vector #(ctrs, ReadOnly #(Bit #(ctrW))) read_counters =
    map (compose (regToReadOnly, sel_snd_port), vec_rg_counter);

  method Vector #(ctrs, ReadOnly #(Bit #(TLog #(evts)))) read_ctr_sels =
    map (regToReadOnly, vec_rg_event_sel);

  method ReadOnly #(Bit #(ctrs)) read_ctr_inhibit =
    regToReadOnly (rg_ctr_inhibit);

  method ReadOnly #(Bit #(ctrs)) read_ctr_overflow =
    regToReadOnly (wr_overflow);


  method write_ctr_sel (Bit #(TLog #(ctrs)) idx) =
    vec_rg_event_sel[idx]._write;

  method write_counter (Bit #(TLog #(ctrs)) idx) =
    vec_rg_counter[idx][1]._write;

  method write_ctr_inhibit = rg_ctr_inhibit._write;

endmodule

// For a synthesize module, the values must be fixed
`define ctrs 3
`define ctrW 64
`define evts 17

(* synthesize *)
module mkPerfCounters (PerfCounters_IFC #(`ctrs, `ctrW, `evts));
  PerfCounters_IFC #(`ctrs, `ctrW, `evts) perf_counters <- mkPerfCounters_Core;
  return perf_counters;
endmodule

endpackage
