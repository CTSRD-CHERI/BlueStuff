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

import List :: *;
import Vector :: *;
import Printf :: *;

import AXI4Lite_Types :: *;
import AXI4Lite_Utils :: *;
import SourceSink :: *;
import MasterSlave :: *;
import ListExtra :: *;
import Interconnect :: *;
import Routable :: *;

//////////////////
// AXI Lite bus //
////////////////////////////////////////////////////////////////////////////////

`define PARAMS addr_, data_, awuser_, wuser_, buser_, aruser_, ruser_

module mkAXILiteBus#(
    MappingTable#(nRoutes, addr_) maptab,
    Vector#(nMasters, AXILiteMaster#(`PARAMS)) masters,
    Vector#(nSlaves, AXILiteSlave#(`PARAMS)) slaves
  ) (Empty) provisos (
    Routable#(
      AXILiteWriteFlit#(addr_, data_, awuser_, wuser_),
      BLiteFlit#(buser_),
      Bit#(addr_)),
    Routable#(
      ARLiteFlit#(addr_, aruser_),
      RLiteFlit#(data_, ruser_),
      Bit#(addr_)),
    // assertion on argument sizes
    Add#(1, a__, nMasters), // at least one master is needed
    Add#(1, b__, nSlaves), // at least one slave is needed
    Add#(nRoutes, 0, nSlaves) // nRoutes == nSlaves
  );

  // prepare masters
  Vector#(nMasters,
    Master#(AXILiteWriteFlit#(addr_, data_, awuser_, wuser_),
    BLiteFlit#(buser_)))
    write_masters = newVector;
  Vector#(nMasters,
    Master#(ARLiteFlit#(addr_, aruser_),
    RLiteFlit#(data_, ruser_)))
    read_masters  = newVector;
  for (Integer i = 0; i < valueOf(nMasters); i = i + 1) begin
    Bit#(TLog#(nMasters)) mid = fromInteger(i);
    // merge from write masters
    write_masters[i] = interface Master;
      interface source = mergeLiteWrite(masters[i].aw, masters[i].w);
      interface sink   = masters[i].b;
    endinterface;
    read_masters[i]  = interface Master;
      interface source = masters[i].ar;
      interface sink   = masters[i].r;
    endinterface;
  end

  // prepare slaves
  Vector#(nSlaves,
    Slave#(AXILiteWriteFlit#(addr_, data_, awuser_, wuser_),
    BLiteFlit#(buser_)))
    write_slaves = newVector;
  Vector#(nSlaves,
    Slave#(ARLiteFlit#(addr_, aruser_),
    RLiteFlit#(data_, ruser_)))
    read_slaves   = newVector;
  for (Integer i = 0; i < valueOf(nSlaves); i = i + 1) begin  
    // split to write slaves
    write_slaves[i] = interface Slave;
      interface sink   = splitLiteWrite(slaves[i].aw, slaves[i].w);
      interface source = slaves[i].b;
    endinterface;
    read_slaves[i]  = interface Slave;
      interface sink   = slaves[i].ar;
      interface source = slaves[i].r;
    endinterface;
  end

  // connect with standard busses
  mkInOrderTwoWayBus(routeFromMappingTable(maptab), write_masters, write_slaves);
  mkInOrderTwoWayBus(routeFromMappingTable(maptab), read_masters, read_slaves);

endmodule

`undef PARAMS
