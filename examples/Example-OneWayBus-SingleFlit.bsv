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

import Interconnect :: *;
import ListExtra :: *;

import FIFOF :: *;

typedef struct {
  Bit#(i) id;
  Bit#(o) dest;
} Flit#(numeric type i, numeric type o) deriving (Bits);
instance FShow#(Flit#(a,b));
  function fshow(x) =
    $format("[id = %0d, dest = %b]", x.id, x.dest);
endinstance
instance Routable#(Flit#(x,y), Bit#(y));
  function routingField (x) = x.dest;
  function isLast (x) = True;
endinstance

function List#(Bool) route (Bit#(a) x);
  case (firstHotToOneHot(bitToList(x))) matches
    tagged Valid .dest: return dest;
    tagged Invalid: return cons(True, replicate(valueOf(a)-1, False));
  endcase
endfunction

function Bit#(n) firstHot(Bit#(n) x);
  Bit#(n) res = 0;
  Bool found = False;
  for (Integer i = valueOf(n) - 1; i >= 0; i = i - 1) begin
    if (x[i] == 1) found = True;
    res[i] = (found) ? 0 : x[i];
  end
  return res;
endfunction

typedef 4 NIns;
typedef 3 NOuts;

interface NextDest#(numeric type n);
  method ActionValue#(Bit#(n)) next;
endinterface
module mkNextDest(NextDest#(n));
  Reg#(Bit#(n)) val <- mkReg(1);
  method next = actionvalue
    let newVal = val << 1;
    if (newVal == 0) newVal = 1;
    val <= newVal;
    return val;
  endactionvalue;
endmodule

module top (Empty);

  Integer nIns  = valueOf(NIns);
  Integer nOuts = valueOf(NOuts);

  let destGen <- replicateM(nIns, mkNextDest);

  List#(FIFOF#(Flit#(NIns, NOuts))) ins  <- replicateM(nIns, mkFIFOF);
  List#(FIFOF#(Flit#(NIns, NOuts))) outs <- replicateM(nOuts, mkFIFOF);

  let cnt <- mkReg(0);
  rule count; cnt <= cnt + 1; endrule

  for (Integer i = 0; i < nIns; i = i + 1) begin
    rule doEnq;
      let newDest <- destGen[i].next;
      ins[i].enq(Flit {
        id: fromInteger(i),
        dest: newDest
      });
    endrule
  end
  for (Integer i = 0; i < nOuts; i = i + 1)
    rule doDeq (cnt % fromInteger(i+1) == fromInteger(i));
      outs[i].deq;
      $display("%0t -- dest %0d received from id %0d -- ", $time, i, outs[i].first.id, fshow(outs[i].first));
    endrule
    
  mkOneWayBus(route, ins, outs);

  rule terminate(cnt > 200); $finish(0); endrule

endmodule
