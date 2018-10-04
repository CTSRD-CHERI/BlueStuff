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

import AXI4_Types :: *;
import AXI4_Utils :: *;
import SourceSink :: *;
import MasterSlave :: *;
import ListExtra :: *;
import Interconnect :: *;
import Routable :: *;

/////////////
// AXI bus //
////////////////////////////////////////////////////////////////////////////////

module mkAXIBus#(
    MappingTable#(nRoutes, addr_) maptab,
    Vector#(nMasters, AXIMaster#(id_, addr_, data_, user_)) masters,
    Vector#(nSlaves, AXISlave#(sid_, addr_, data_, user_)) slaves
  ) (Empty) provisos (
    Add#(id_, TLog#(nMasters), sid_),
    ExpandReqRsp#(
      AXIWriteFlit#(id_, addr_, data_, user_),
      AXIWriteFlit#(sid_, addr_, data_, user_),
      BFlit#(sid_, user_),
      BFlit#(id_, user_),
      Bit#(TLog#(nMasters))),
    ExpandReqRsp#(
      ARFlit#(id_, addr_, user_),
      ARFlit#(sid_, addr_, user_),
      RFlit#(sid_, data_, user_),
      RFlit#(id_, data_, user_),
      Bit#(TLog#(nMasters))),
    // assertion on argument sizes
    Add#(1, a__, nMasters), // at least one master is needed
    Add#(1, b__, nSlaves), // at least one slave is needed
    Add#(nRoutes, 0, nSlaves) // nRoutes == nSlaves
  );

  // prepare masters
  Vector#(nMasters,
    Master#(AXIWriteFlit#(id_, addr_, data_, user_), BFlit#(id_, user_))
  ) write_masters = newVector;
  Vector#(nMasters,
    Master#(ARFlit#(id_, addr_, user_), RFlit#(id_, data_, user_))
  ) read_masters = newVector;
  for (Integer i = 0; i < valueOf(nMasters); i = i + 1) begin
    Bit#(TLog#(nMasters)) mid = fromInteger(i);
    // merge from write masters
    let merged <- mergeWrite(masters[i].aw, masters[i].w);
    write_masters[i] = interface Master;
      interface source = merged;
      interface sink   = masters[i].b;
    endinterface;
    read_masters[i]    = interface Master;
      interface source = masters[i].ar;
      interface sink   = masters[i].r;
    endinterface;
  end

  // prepare slaves
  Vector#(nSlaves,
    Slave#(AXIWriteFlit#(sid_, addr_, data_, user_), BFlit#(sid_, user_))
  ) write_slaves = newVector;
  Vector#(nSlaves,
    Slave#(ARFlit#(sid_, addr_, user_), RFlit#(sid_, data_, user_))
  ) read_slaves = newVector;
  for (Integer i = 0; i < valueOf(nSlaves); i = i + 1) begin  
    // split to write slaves
    let split <- splitWrite(slaves[i].aw, slaves[i].w);
    write_slaves[i] = interface Slave;
      interface sink   = split;
      interface source = slaves[i].b;
    endinterface;
    read_slaves[i] = interface Slave;
      interface sink   = slaves[i].ar;
      interface source = slaves[i].r;
    endinterface;
  end

  // connect with standard busses
  mkTwoWayBus(routeFromMappingTable(maptab), write_masters, write_slaves);
  mkTwoWayBus(routeFromMappingTable(maptab), read_masters, read_slaves);

endmodule
