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


import Routable :: *;
import Interconnect :: *;
import ListExtra :: *;
import SourceSink :: *;
import MasterSlave :: *;

import FIFOF :: *;
import Vector :: *;
import ConfigReg :: *;

typedef 1 NMasters;
typedef 1 NSlaves;
Integer nb_flit = 16;
Integer nb_rsp = 4;

function Bit#(n) firstHot(Bit#(n) x);
  Bit#(n) res = 0;
  Bool found = False;
  for (Integer i = valueOf(n) - 1; i >= 0; i = i - 1) begin
    if (x[i] == 1) found = True;
    res[i] = (found) ? 0 : x[i];
  end
  return res;
endfunction

typedef struct {
  Vector#(NSlaves, Bool) to;
  Bit#(32) payload;
  Bool last;
} Req deriving (Bits);
instance FShow#(Req);
  function fshow(x) = $format( "Req { to:%b, payload: %0d, last: "
                             , x.to, x.payload, fshow (x.last), " }");
endinstance

typedef enum {OK, KO} RStatus deriving (Bits, Eq, FShow);
typedef struct {
  RStatus status;
} Rsp deriving (Bits, FShow);

instance Routable#(Req, Rsp, Vector#(NSlaves, Bool));
  function routingField (x) = x.to;
  module mkNoRouteFound (NoRouteFoundIfc#(Req, Rsp));
    FIFOF#(Bit#(0)) ff <- mkFIFOF1;
    method pushReq (x) = ff.enq(?);
    method getRsp = actionvalue
      ff.deq;
      return tuple2(True, Rsp { status: KO });
    endactionvalue;
  endmodule
endinstance

instance DetectLast#(Req); function detectLast (x) = x.last; endinstance
instance DetectLast#(Rsp); function detectLast = constFn(True); endinstance

module mkMaster (Master#(Req, Rsp));
  Reg#(Vector#(NSlaves, Bool)) dest <- mkConfigReg(cons(True, replicate(False)));
  let ff <- mkFIFOF;
  Reg#(Bool) reqSent <- mkReg (False);
  Reg#(Bit#(32)) rspCnt <- mkReg (0);
  Reg#(Bit#(32)) cnt <- mkReg (0);
  rule enq (!reqSent);
    dest <= rotate(dest);
    let req = Req { to: dest
                  , payload: cnt
                  , last: cnt == fromInteger (nb_flit - 1) };
    ff.enq(req);
    if (cnt == fromInteger (nb_flit - 1)) begin
      cnt <= 0;
      reqSent <= True;
    end else cnt <= cnt + 1;
    $display("%0t -- Master sending ", $time, fshow(req));
  endrule
  interface source = toSource(ff);
  interface sink = interface Sink;
    method put(x) if (reqSent) = action
      $display("%0t -- Master received ", $time, fshow(x));
      reqSent <= False;
      if (rspCnt == fromInteger (nb_rsp - 1)) $finish(0);
      else rspCnt <= rspCnt + 1;
    endaction;
    method canPut = True;
  endinterface;
endmodule

module mkSlave (Slave#(Req, Rsp));
  let ff <- mkFIFOF;
  interface sink = interface Sink;
    method put(x) = action
      $display("%0t -- -- -- Slave received ", $time, fshow(x));
      if (x.last) begin
        $display("%0t -- -- -- Slave sending rsp", $time);
        ff.enq(Rsp { status: OK });
      end
    endaction;
    method canPut = ff.notFull;
  endinterface;
  interface source = toSource(ff);
endmodule

module top (Empty);

  Vector#(NMasters, Master#(Req, Rsp)) masters <- replicateM(mkMaster);
  Vector#(NSlaves,  Slave#(Req, Rsp))  slaves  <- replicateM(mkSlave);

  mkInOrderTwoWayBus(id, masters, slaves);
  //mkTwoWayBus(id, masters, slaves);

endmodule
