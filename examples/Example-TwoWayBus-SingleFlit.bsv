/*-
 * Copyright (c) 2018-2019 Alexandre Joannou
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

function Bit#(n) firstHot(Bit#(n) x);
  Bit#(n) res = 0;
  Bool found = False;
  for (Integer i = valueOf(n) - 1; i >= 0; i = i - 1) begin
    if (x[i] == 1) found = True;
    res[i] = (found) ? 0 : x[i];
  end
  return res;
endfunction

typedef 1 NMasters;
typedef 1 NSlaves;

typedef struct {
  Vector#(NSlaves, Bool) to;
} Req deriving (Bits);
instance FShow#(Req);
  function fshow(x) = $format("Req { to:%b }", x.to);
endinstance

typedef enum {OK, KO} RStatus deriving (Bits, Eq, FShow);
typedef struct {
  RStatus status;
} Rsp deriving (Bits, FShow);

instance Routable#(Req, Rsp, Vector#(NSlaves, Bool));
  function routingField (x) = x.to;
  module mkNoRouteSlave (Slave#(Req, Rsp));
    FIFOF#(Bit#(0)) ff <- mkFIFOF1;
    interface sink = interface Sink;
      method canPut = ff.notFull;
      method put (x) = ff.enq(?);
    endinterface;
    interface source = interface Source;
      method canPeek = ff.notEmpty;
      method peek if (ff.notEmpty) = Rsp { status: KO };
      method drop if (ff.notEmpty) = ff.deq;
    endinterface;
  endmodule
endinstance

instance DetectLast#(Req); function detectLast = constFn(True); endinstance
instance DetectLast#(Rsp); function detectLast = constFn(True); endinstance

module mkMaster (Master#(Req, Rsp));
  Reg#(Vector#(NSlaves, Bool)) dest <- mkConfigReg(cons(True, replicate(False)));
  let ff <- mkFIFOF;
  rule enq;
    dest <= rotate(dest);
    let req = Req { to: dest };
    ff.enq(req);
    $display("%0t -- Master sending ", $time, fshow(req));
  endrule
  interface source = toSource(ff);
  interface sink = interface Sink;
    method put(x) = action $display("%0t -- Master received ", $time, fshow(x)); endaction;
    method canPut = True;
  endinterface;
endmodule

module mkSlave (Slave#(Req, Rsp));
  let ff <- mkFIFOF;
  interface sink = interface Sink;
    method put(x) = action
      $display("%0t -- Slave received ", $time, fshow(x));
      ff.enq(Rsp { status: OK });
    endaction;
    method canPut = ff.notFull;
  endinterface;
  interface source = toSource(ff);
endmodule

module top (Empty);

  Vector#(NMasters, Master#(Req, Rsp)) masters <- replicateM(mkMaster);
  Vector#(NSlaves,  Slave#(Req, Rsp))  slaves  <- replicateM(mkSlave);

  let cnt <- mkReg(0);
  rule count; cnt <= cnt + 1; endrule

  mkInOrderTwoWayBus(id, masters, slaves);

  rule terminate(cnt > 3000); $finish(0); endrule

endmodule
