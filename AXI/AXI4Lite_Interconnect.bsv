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

import List :: *;
import Vector :: *;
import Printf :: *;

import AXI4Lite_Types :: *;
import AXI4Lite_Utils :: *;
import SourceSink :: *;
import MasterSlave :: *;
//import ListExtra :: *;
import Interconnect :: *;
import Routable :: *;

//////////////////
// AXI Lite bus //
////////////////////////////////////////////////////////////////////////////////

`define PARAMS addr_, data_, awuser_, wuser_, buser_, aruser_, ruser_

module mkAXI4LiteBus_Synth #(
  function Vector #(nSlaves, Bool) route (Bit #(addr_) val)
, Vector #(nMasters, AXI4Lite_Master_Synth #(`PARAMS)) masters
, Vector #(nSlaves,  AXI4Lite_Slave_Synth  #(`PARAMS)) slaves
) (Empty) provisos (
  Add #(1, a__, nSlaves)
, Add #(1, b__, nMasters)
);
  let msNoSynth <- mapM (fromAXI4Lite_Master_Synth, masters);
  let ssNoSynth <- mapM (fromAXI4Lite_Slave_Synth,  slaves);
  mkAXI4LiteBus (route, msNoSynth, ssNoSynth);
endmodule

module mkAXI4LiteBus #(
  function Vector #(nSlaves, Bool) route (Bit #(addr_) val)
, Vector #(nMasters, AXI4Lite_Master #(`PARAMS)) masters
, Vector #(nSlaves,  AXI4Lite_Slave  #(`PARAMS)) slaves
) (Empty) provisos (
  Add #(1, a__, nSlaves)
, Add #(1, b__, nMasters)
);

  // prepare masters
  Vector #( nMasters
          , Master #( AXI4Lite_WriteFlit #(addr_, data_, awuser_, wuser_)
                    , AXI4Lite_BFlit #(buser_)))
    write_masters = newVector;
  Vector #(nMasters, Master #( AXI4Lite_ARFlit #(addr_, aruser_)
                             , AXI4Lite_RFlit #(data_, ruser_)))
    read_masters = newVector;
  for (Integer i = 0; i < valueOf (nMasters); i = i + 1) begin
    Bit #(TLog #(nMasters)) mid = fromInteger (i);
    // merge from write masters
    write_masters[i] = interface Master;
      interface source = mergeLiteWrite (masters[i].aw, masters[i].w);
      interface sink   = masters[i].b;
    endinterface;
    read_masters[i]    = interface Master;
      interface source = masters[i].ar;
      interface sink   = masters[i].r;
    endinterface;
  end

  // prepare slaves
  Vector #( nSlaves
          , Slave #( AXI4Lite_WriteFlit #(addr_, data_, awuser_, wuser_)
                   , AXI4Lite_BFlit #(buser_)))
    write_slaves = newVector;
  Vector #(nSlaves, Slave #( AXI4Lite_ARFlit #(addr_, aruser_)
                           , AXI4Lite_RFlit #(data_, ruser_)))
    read_slaves = newVector;
  for (Integer i = 0; i < valueOf (nSlaves); i = i + 1) begin
    // split to write slaves
    write_slaves[i] = interface Slave;
      interface sink   = splitLiteWrite (slaves[i].aw, slaves[i].w);
      interface source = slaves[i].b;
    endinterface;
    read_slaves[i] = interface Slave;
      interface sink   = slaves[i].ar;
      interface source = slaves[i].r;
    endinterface;
  end

  // connect with standard busses
  mkInOrderTwoWayBus (route, write_masters, write_slaves);
  mkInOrderTwoWayBus (route, read_masters,  read_slaves);

endmodule

`undef PARAMS
