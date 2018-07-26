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


//////////////////////////
// Virtualize interface //
//////////////////////////

typeclass Virtualizable#(type a);
  module virtualize#(a x, Integer n)(Array#(a));
endtypeclass

// virtualizable instance for Reg, with static priority
instance Virtualizable#(Reg#(a)) provisos (Bits#(a, a__));
  module virtualize#(Reg#(a) r, Integer n)(Array#(Reg#(a)));

    Reg#(a) ifc[n];
    Rules ifcRules = emptyRules;

    for (Integer i = 0; i < n; i = i + 1) begin
      Wire#(a) w_write <- mkWire;
      ifcRules = rJoinDescendingUrgency(ifcRules, rules
        rule doWrite; r <= w_write; endrule
      endrules);
      ifc[i] = interface Reg;
        method _read = r._read;
        method _write(x) = action w_write <= x; endaction;
      endinterface;
    end

    addRules(ifcRules);

    return ifc;

  endmodule

endinstance

// virtualizable instance for Server, with static priority
import ClientServer :: *;
import GetPut :: *;
import FIFO :: *;
import SpecialFIFOs :: *;
import Printf :: *;

typeclass NeedRsp#(type req_t);
  function Bool needRsp(req_t req);
endtypeclass

instance Virtualizable#(Server#(req_t, rsp_t))
provisos (NeedRsp#(req_t), Bits#(req_t, a__), Bits#(rsp_t, b__));

  module virtualize#(Server#(req_t, rsp_t) server, Integer n)(Array#(Server#(req_t, rsp_t)));

    `define MAX_IDX_SZ 4
    if (log2(n) > `MAX_IDX_SZ)
      error(sprintf("Asked for %0d interfaces, virtualize for Server can't support more than %0d", n, 2**`MAX_IDX_SZ));

    Server#(req_t, rsp_t) ifc[n];
    FIFO#(Bit#(`MAX_IDX_SZ)) ifcIdx <- mkFIFO;
    Rules ifcRules = emptyRules;

    for (Integer i = 0; i < n; i = i + 1) begin
      let reqFF <- mkBypassFIFO;
      let rspFF <- mkBypassFIFO;
      ifcRules = rJoinDescendingUrgency(ifcRules, rules
        rule doSendReq;
          reqFF.deq;
          let req = reqFF.first;
          server.request.put(req);
          if (needRsp(req)) ifcIdx.enq(fromInteger(i));
        endrule
        rule doGetRsp (ifcIdx.first == fromInteger(i));
          ifcIdx.deq;
          let rsp <- server.response.get;
          rspFF.enq(rsp);
        endrule
      endrules);
      ifc[i] = interface Server;
        interface request = interface Put; method put = reqFF.enq; endinterface;
        interface response = interface Get;
          method get = actionvalue rspFF.deq; return rspFF.first; endactionvalue;
        endinterface;
      endinterface;
    end

    addRules(ifcRules);

    return ifc;

  endmodule
endinstance
