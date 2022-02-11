/*-
 * Copyright (c) 2022 Alexandre Joannou
 * All rights reserved.
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory (Department of Computer Science and
 * Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
 * DARPA SSITH research programme.
 *
 * This material is based upon work supported by the DoD Information Analysis
 * Center Program Management Office (DoD IAC PMO), sponsored by the Defense
 * Technical Information Center (DTIC) under Contract No. FA807518D0004.  Any
 * opinions, findings and conclusions or recommendations expressed in this
 * material are those of the author(s) and do not necessarily reflect the views
 * of the Air Force Installation Contracting Agency (AFICA).
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

package CounterHelpers;

interface LevelTriggeredCounterIfc #(numeric type n);
  method Action setThreshold (Bit #(n) x);
  method Action setLevel (Bit #(n) x);
  method Action increment;
  method Action decrement;
  method Bool levelReached;
endinterface

module mkLevelTriggeredCounter #(Integer threshold)
                                (LevelTriggeredCounterIfc #(n));

  let regLvl <- mkReg (0);
  let regThreshold <- mkReg (fromInteger (threshold));
  let doSetLvl <- mkRWire;
  let doInc <- mkPulseWire;
  let doDec <- mkPulseWire;

  (* fire_when_enabled, no_implicit_conditions *)
  rule updateLvl;
    let newVal = regLvl;
    if (doInc) newVal = newVal + 1;
    if (doDec) newVal = newVal - 1;
    regLvl <= fromMaybe (newVal, doSetLvl.wget);
  endrule

  method setThreshold = regThreshold._write;
  method setLevel = doSetLvl.wset;
  method increment = doInc.send;
  method decrement = doDec.send;
  method levelReached = regLvl >= regThreshold;

endmodule

interface TimerIfc #(numeric type n);
  method Action start (Bit #(n) x);
  method Bool done;
endinterface

module mkTimer (TimerIfc #(n));

  let cnt <- mkLevelTriggeredCounter (0);

  (* fire_when_enabled, no_implicit_conditions *)
  rule tick (!cnt.levelReached); cnt.increment; endrule

  method start (x) = action
    cnt.setLevel (0);
    cnt.setThreshold (x);
  endaction;
  method done = cnt.levelReached;

endmodule

typedef enum { Rising, Falling } EdgeType deriving (Eq);

module mkEdgeDetector #(t x) (ReadOnly #(EdgeType)) provisos (Bits #(t, 1));

  let r <- mkRegU;

  (* fire_when_enabled, no_implicit_conditions *)
  rule latchInput; r <= pack (x); endrule

  method _read if (r != pack (x)) = (pack (x) == 1'b0) ? Falling : Rising;

endmodule

module mkSpecificEdgeDetector #(EdgeType edgeType, t x) (ReadOnly #(Bool))
  provisos (Bits #(t, 1));

  let detector <- mkEdgeDetector (x);
  let detected <- mkDWire (False);

  // XXX unclear why, but the "no_implicit_conditions" annotation on this rule
  // seems to cause bsc to choke
  (* fire_when_enabled *)
  rule detect (detector == edgeType); detected <= True; endrule

  method _read = detected;

endmodule

module mkRisingEdgeDetector #(t x) (ReadOnly #(Bool)) provisos (Bits #(t, 1));
  let ifc <- mkSpecificEdgeDetector (Rising, x);
  return ifc;
endmodule

module mkFallingEdgeDetector #(t x) (ReadOnly #(Bool)) provisos (Bits #(t, 1));
  let ifc <- mkSpecificEdgeDetector (Falling, x);
  return ifc;
endmodule

endpackage
