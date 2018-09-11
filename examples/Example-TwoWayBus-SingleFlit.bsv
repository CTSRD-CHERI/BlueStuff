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


import Routable :: *;
import Interconnect :: *;
import ListExtra :: *;
import SourceSink :: *;
import MasterSlave :: *;

import FIFOF :: *;
import Vector :: *;

function Vector#(n, Bool) route (Bit#(a) x);
  case (firstHotToOneHot(toList(unpack((x))))) matches
    tagged Valid .dest: return toVector(dest);
    tagged Invalid: return replicate(False);
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

typedef 4 NMasters;
typedef 3 NSlaves;

typedef struct {
  Bit#(a) from;
  Bit#(b) to;
} Req#(numeric type a, numeric type b) deriving (Bits, FShow);
instance Routable#(Req#(a,b), Bit#(b));
  function routingField (x) = x.to;
  function isLast       (x) = True;
endinstance
typedef Req#(a, b) Rsp#(numeric type a, numeric type b);

module mkMaster#(Bit#(a) me) (Master#(Req#(a, b), Rsp#(b, a)));
  Reg#(Bit#(b)) dest <- mkReg(1);
  let ff <- mkFIFOF;
  rule enq;
    let newDest = dest << 1;
    if (newDest == 0) newDest = 1;
    dest <= newDest;
    ff.enq(Req { from: me, to: dest });
  endrule
  interface source = interface Source;
    method get    = actionvalue ff.deq; return ff.first; endactionvalue;
    method peek   = ff.first;
    method canGet = True;
  endinterface;
  interface sink = interface Sink;
    method put(x) = action $display("%0t -- Master %b received ", $time, me, fshow(x)); endaction;
    method canPut = True;
  endinterface;
endmodule

module mkSlave#(Bit#(b) me) (Slave#(Req#(a, b), Rsp#(b, a)));
  let ff <- mkFIFOF;
  interface sink = interface Sink;
    method put(x) = action
      $display("%0t -- Slave %b received ", $time, me, fshow(x));
      ff.enq(Rsp {from: me, to: x.from});
    endaction;
    method canPut = ff.notFull;
  endinterface;
  interface source = interface Source;
    method get    = actionvalue ff.deq; return ff.first; endactionvalue;
    method peek   = ff.first;
    method canGet = ff.notEmpty;
  endinterface;
endmodule

module top (Empty);

  function Bit#(n) intToOneHot(Integer i) = (1 << i);
  Vector#(NMasters, Bit#(NMasters)) mIDs = genWith(intToOneHot);
  Vector#(NSlaves, Bit#(NSlaves))   sIDs = genWith(intToOneHot);
  Vector#(NMasters, Master#(Req#(NMasters, NSlaves),Rsp#(NSlaves, NMasters))) masters <- mapM(mkMaster, mIDs);
  Vector#(NSlaves, Slave#(Req#(NMasters, NSlaves),Rsp#(NSlaves, NMasters)))   slaves  <- mapM(mkSlave, sIDs);

  let cnt <- mkReg(0);
  rule count; cnt <= cnt + 1; endrule

  mkTwoWayBus(route, route, masters, slaves);

  rule terminate(cnt > 200); $finish(0); endrule

endmodule
