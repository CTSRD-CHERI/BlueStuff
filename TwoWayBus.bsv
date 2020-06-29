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

package TwoWayBus;

export mkTwoWayBus;
export mkRelaxedTwoWayBus;
export mkInOrderTwoWayBus;

import Routable    :: *;
import OneWayBus   :: *;
import SourceSink  :: *;
import MasterSlave :: *;

import FIFOF  :: *;
import Vector :: *;

/////////////////
// Two-Way Bus //
////////////////////////////////////////////////////////////////////////////////
module mkTwoWayBus #(
  function Vector #(nSlvs, Bool)  routeUp    (r_up_t x)
, function Vector #(nMsts, Bool)  routeDown  (r_down_t x)
, slave_t                         noRouteSlv
, function module #(inner_master) wrapMaster ( master_t m
                                             , Integer idx)
, function module #(inner_slave)  wrapSlave  (slave_t s)
, Vector #(nMsts, master_t)       ms
, Vector #(nSlvs,  slave_t)       ss
) (Empty) provisos (
  Bits #(req_t,     req_sz),       Bits #(rsp_t,     rsp_sz)
, Bits #(inner_req, inner_req_sz), Bits #(inner_rsp, inner_rsp_sz)
, Bits #(req_fat_t, req_fat_sz),   Bits #(rsp_fat_t, rsp_fat_sz)
, Routable #(inner_req, r_up_t)
, Routable #(inner_rsp, r_down_t)
  // type aliases
, Alias #(master_t,     Master #(req_t,     rsp_t))
, Alias #(inner_master, Master #(inner_req, inner_rsp))
, Alias #(inner_slave,  Slave  #(inner_req, inner_rsp))
, Alias #(slave_t,      Slave  #(req_fat_t, rsp_fat_t))
  // assertion on argument sizes
, Add #(1, a__, nMsts) // at least one Master is needed
, Add #(1, b__, nSlvs) // at least one slave is needed
);

  let innerMasters    <- zipWithM  (wrapMaster, ms, genVector);
  let innerSlaves     <- mapM      (wrapSlave, ss);
  let innerNoRouteSlv <- wrapSlave (noRouteSlv);

  // Master to Slave bus
  //////////////////////////////////////////////////////////////////////////////
  mkOneWayBusNoRoute ( routeUp
                     , getSlaveSink (innerNoRouteSlv)
                     , map (getMasterSource, innerMasters)
                     , map (getSlaveSink, innerSlaves));

  // Slave to Master bus
  //////////////////////////////////////////////////////////////////////////////
  mkOneWayBus ( routeDown
              , map (getSlaveSource, cons (innerNoRouteSlv, innerSlaves))
              , map (getMasterSink, innerMasters));

endmodule

/////////////////////////
// Relaxed Two-Way Bus //
////////////////////////////////////////////////////////////////////////////////
module mkRelaxedTwoWayBus #( function Vector #(n_slaves, Bool) route (r_up_t x)
                           , Vector #(n_masters, master_t) ms
                           , Vector #(n_slaves,  slave_t) ss
  ) (Empty) provisos (
    Bits #(req_t,     req_sz),       Bits #(rsp_t,     rsp_sz)
  , Bits #(inner_req, inner_req_sz), Bits #(inner_rsp, inner_rsp_sz)
  , Bits #(req_fat_t, req_fat_sz),   Bits #(rsp_fat_t, rsp_fat_sz)
  , Routable #(inner_req, r_up_t)
  , Routable #(inner_rsp, r_down_t)
  , FallibleRoute #(req_fat_t, rsp_fat_t)
  , ExpandableReqRsp #(req_t, req_fat_t, rsp_fat_t, rsp_t, n_masters)
    // type aliases
  , Alias #(r_down_t,     Bit #(TLog #(n_masters)))
  , Alias #(inner_req,    WithMetaInfo  #(req_t, r_down_t))
  , Alias #(inner_rsp,    WithRouteInfo #(rsp_t, r_down_t))
  , Alias #(master_t,     Master #(req_t,     rsp_t))
  , Alias #(inner_master, Master #(inner_req, inner_rsp))
  , Alias #(inner_slave,  Slave  #(inner_req, inner_rsp))
  , Alias #(slave_t,      Slave  #(req_fat_t, rsp_fat_t))
    // assertion on argument sizes
  , Add #(1, a__, n_masters) // at least one Master is needed
  , Add #(1, b__, n_slaves)  // at least one slave is needed
  );

  // the wrapper for a master:
  // - augments requests with the master's index on the bus for later route
  //   back using the WithMetaInfo type
  // - extracts the response from the payload field in the WithRouteInfo type
  //   used for inner responses
  module wrapMaster #(master_t m, Integer idx)
                     (Master #(inner_req, inner_rsp));
    function inner_req toInnerReq (req_t r) =
      WithMetaInfo { payload: r, metaInfo: fromInteger (idx) };
    function rsp_t fromInnerRsp (inner_rsp r) = r.payload;
    interface source = mapSource (toInnerReq, m.source);
    interface sink   = mapSink   (fromInnerRsp, m.sink);
  endmodule

  // the wrapper for a slave:
  // - creates the fat request out of the information in the WithMetaInfo
  //   request received from the Master using the expand method
  // - prepares the inner response from the Slave's response into a
  //   WithRouteInfo using the shrink method
  module wrapSlave #(slave_t s) (Slave #(inner_req, inner_rsp));
    function req_fat_t fromInnerReq (inner_req r) =
      expand (r.metaInfo, r.payload);
    function inner_rsp toInnerRsp (rsp_fat_t r);
      Tuple2 #(r_down_t, rsp_t) x = shrink (r); // XXX extra type info needed
      match {.r_info, .rsp} = x;                // XXX to help bsc...
      return WithRouteInfo { routeInfo: r_info, payload: rsp };
    endfunction
    interface sink   = mapSink   (fromInnerReq, s.sink);
    interface source = mapSource (toInnerRsp, s.source);
  endmodule

  let noRouteSlv <- mkNoRouteSlave;

  mkTwoWayBus ( route, indexToOneHot, noRouteSlv
              , wrapMaster, wrapSlave, ms, ss);

endmodule

//////////////////////////
// In-Order Two-Way Bus //
////////////////////////////////////////////////////////////////////////////////
module mkInOrderTwoWayBus #( function Vector #(n_slaves, Bool) route (r_up_t x)
                           , Vector #(n_masters, master_t) ms
                           , Vector #(n_slaves,  slave_t) ss
  ) (Empty) provisos (
    Bits #(req_t,     req_sz),       Bits #(rsp_t,     rsp_sz)
  , Bits #(inner_req, inner_req_sz), Bits #(inner_rsp, inner_rsp_sz)
  , Routable #(req_t, r_up_t)
  , Has_isLast #(rsp_t)
  //, FallibleRoute #(inner_req, inner_rsp)
  , FallibleRoute #(req_t, rsp_t)
    // type aliases
  , Alias #(r_down_t,     Bit #(TLog #(n_masters)))
  , Alias #(inner_req,    WithMetaInfo  #(req_t, r_down_t))
  , Alias #(inner_rsp,    WithRouteInfo #(rsp_t, r_down_t))
  , Alias #(master_t,     Master #(req_t,     rsp_t))
  , Alias #(slave_t,      Slave  #(req_t, rsp_t))
    // assertion on argument sizes
  , Add #(1, a__, n_masters) // at least one Master is needed
  , Add #(1, b__, n_slaves)  // at least one slave is needed
  );

  // the wrapper for a master:
  // - augments requests with the master's index on the bus for later route
  //   back using the WithMetaInfo type
  // - extracts the response from the payload field in the WithRouteInfo type
  //   used for inner responses
  module wrapMaster #(master_t m, Integer idx)
                     (Master #(inner_req, inner_rsp));
    function inner_req toInnerReq (req_t r) =
      WithMetaInfo { payload: r, metaInfo: fromInteger (idx) };
    function rsp_t fromInnerRsp (inner_rsp r) = r.payload;
    interface source = mapSource (toInnerReq, m.source);
    interface sink   = mapSink   (fromInnerRsp, m.sink);
  endmodule

  // the wrapper for a slave. Note: The slave MUST respond in order
  // - extract the routing information from the inner request and stash it in a
  //   fifo while the vanilla request is being processed by the slave
  // - dequeue the fifo and augment the vanilla slave response with the routing
  //   information for the inner response
  module wrapSlave #(slave_t s) (Slave #(inner_req, inner_rsp));
    let ff <- mkUGFIFOF;
    let reqInfoSaved <- mkReg (False);
    let guardSnk =
      s.sink.canPut && (reqInfoSaved || (!reqInfoSaved && ff.notFull));
    let guardSrc = s.source.canPeek && ff.notEmpty;
    interface sink = interface Sink;
      method canPut = guardSnk;
      method put (x) if (guardSnk) = action
        if (!reqInfoSaved) begin
          ff.enq (x.metaInfo);
          if (!isLast (x)) reqInfoSaved <= True;
        end
        if (isLast (x)) reqInfoSaved <= False;
        s.sink.put (x.payload);
      endaction;
    endinterface;
    interface source = interface Source;
      method canPeek = guardSrc;
      method peek if (guardSrc) = WithRouteInfo { routeInfo: ff.first
                                                , payload: s.source.peek };
      method drop if (guardSrc) = action
        s.source.drop;
        if (isLast (s.source.peek)) ff.deq;
      endaction;
    endinterface;
  endmodule

  let noRouteSlv <- mkNoRouteSlave;

  mkTwoWayBus ( route, indexToOneHot, noRouteSlv
              , wrapMaster, wrapSlave, ms, ss);

endmodule

endpackage
