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

import AXI4_Types :: *;
import AXI4_AW_Utils :: *;
import AXI4_W_Utils :: *;
import AXI4_B_Utils :: *;
import AXI4_AR_Utils :: *;
import AXI4_R_Utils :: *;

import FIFOF :: *;

//////////////////////
// Util Typeclasses //
////////////////////////////////////////////////////////////////////////////////

typeclass NullSource#(type a);
  module mkNullSource(a);
endtypeclass

typeclass Sink#(type a);
  module mkSink(a);
endtypeclass

///////////////////////////////
// AXI Address Write Channel //
////////////////////////////////////////////////////////////////////////////////

// NullSource / Sink instances

instance NullSource#(AWMaster#(a, b, c));
  module mkNullSource(AWMaster#(a, b, c));
    method awid     = ?;
    method awaddr   = ?;
    method awlen    = ?;
    method awsize   = ?;
    method awburst  = ?;
    method awlock   = ?;
    method awcache  = ?;
    method awprot   = ?;
    method awqos    = ?;
    method awregion = ?;
    method awuser   = ?;
    method awvalid  = False;
    method awready(_) = noAction;
  endmodule
endinstance

instance NullSource#(AWLiteMaster#(a));
  module mkNullSource(AWLiteMaster#(a));
    method awaddr  = ?;
    method awprot  = ?;
    method awvalid = False;
    method awready(_) = noAction;
  endmodule
endinstance

instance Sink#(AWSlave#(a, b, c));
  module mkSink(AWSlave#(a, b, c));
    method awid    (_) = noAction;
    method awaddr  (_) = noAction;
    method awlen   (_) = noAction;
    method awsize  (_) = noAction;
    method awburst (_) = noAction;
    method awlock  (_) = noAction;
    method awcache (_) = noAction;
    method awprot  (_) = noAction;
    method awqos   (_) = noAction;
    method awregion(_) = noAction;
    method awuser  (_) = noAction;
    method awvalid (_) = noAction;
    method awready = True;
  endmodule
endinstance

instance Sink#(AWLiteSlave#(a));
  module mkSink(AWLiteSlave#(a));
    method awaddr  (_) = noAction;
    method awprot  (_) = noAction;
    method awvalid (_) = noAction;
    method awready = True;
  endmodule
endinstance

////////////////////////////
// AXI Write Data Channel //
////////////////////////////////////////////////////////////////////////////////

// NullSource / Sink instances

instance NullSource#(WMaster#(a, b));
  module mkNullSource(WMaster#(a, b));
    method wdata  = ?;
    method wstrb  = ?;
    method wlast  = ?;
    method wuser  = ?;
    method wvalid = False;
    method wready(_) = noAction;
  endmodule
endinstance

instance NullSource#(WLiteMaster#(a));
  module mkNullSource(WLiteMaster#(a));
    method wdata  = ?;
    method wstrb  = ?;
    method wvalid = False;
    method wready(_) = noAction;
  endmodule
endinstance

instance Sink#(WSlave#(a, b));
  module mkSink(WSlave#(a, b));
    method wdata (_) = noAction;
    method wstrb (_) = noAction;
    method wlast (_) = noAction;
    method wuser (_) = noAction;
    method wvalid(_) = noAction;
    method wready = True;
  endmodule
endinstance

instance Sink#(WLiteSlave#(a));
  module mkSink(WLiteSlave#(a));
    method wdata (_) = noAction;
    method wstrb (_) = noAction;
    method wvalid(_) = noAction;
    method wready = True;
  endmodule
endinstance

////////////////////////////////
// AXI Write Response Channel //
////////////////////////////////////////////////////////////////////////////////

// NullSource / Sink instances

instance Sink#(BMaster#(a, b));
  module mkSink(BMaster#(a, b));
    method bid   (_) = noAction;
    method bresp (_) = noAction;
    method buser (_) = noAction;
    method bvalid(_) = noAction;
    method bready = True;
  endmodule
endinstance

instance Sink#(BLiteMaster);
  module mkSink(BLiteMaster);
    method bresp (_) = noAction;
    method bvalid(_) = noAction;
    method bready = True;
  endmodule
endinstance

instance NullSource#(BSlave#(a, b));
  module mkNullSource(BSlave#(a, b));
    method bid    = ?;
    method bresp  = ?;
    method buser  = ?;
    method bvalid = False;
    method bready(_) = noAction;
  endmodule
endinstance

instance NullSource#(BLiteSlave);
  module mkNullSource(BLiteSlave);
    method bresp  = ?;
    method bvalid = False;
    method bready(_) = noAction;
  endmodule
endinstance

//////////////////////////////
// AXI Read Address Channel //
////////////////////////////////////////////////////////////////////////////////

// NullSource / Sink instances

instance NullSource#(ARMaster#(a, b, c));
  module mkNullSource(ARMaster#(a, b, c));
    method arid     = ?;
    method araddr   = ?;
    method arlen    = ?;
    method arsize   = ?;
    method arburst  = ?;
    method arlock   = ?;
    method arcache  = ?;
    method arprot   = ?;
    method arqos    = ?;
    method arregion = ?;
    method aruser   = ?;
    method arvalid  = False;
    method arready(_) = noAction;
  endmodule
endinstance

instance NullSource#(ARLiteMaster#(a));
  module mkNullSource(ARLiteMaster#(a));
    method araddr  = ?;
    method arprot  = ?;
    method arvalid = False;
    method arready(_) = noAction;
  endmodule
endinstance

instance Sink#(ARSlave#(a, b, c));
  module mkSink(ARSlave#(a, b, c));
    method arid    (_) = noAction;
    method araddr  (_) = noAction;
    method arlen   (_) = noAction;
    method arsize  (_) = noAction;
    method arburst (_) = noAction;
    method arlock  (_) = noAction;
    method arcache (_) = noAction;
    method arprot  (_) = noAction;
    method arqos   (_) = noAction;
    method arregion(_) = noAction;
    method aruser  (_) = noAction;
    method arvalid (_) = noAction;
    method arready = True;
  endmodule
endinstance

instance Sink#(ARLiteSlave#(a));
  module mkSink(ARLiteSlave#(a));
    method araddr  (_) = noAction;
    method arprot  (_) = noAction;
    method arvalid (_) = noAction;
    method arready = True;
  endmodule
endinstance

///////////////////////////
// AXI Read Data Channel //
////////////////////////////////////////////////////////////////////////////////

// NullSource / Sink instances

instance Sink#(RMaster#(a, b, c));
  module mkSink(RMaster#(a, b, c));
    method rid   (_) = noAction;
    method rdata (_) = noAction;
    method rresp (_) = noAction;
    method rlast (_) = noAction;
    method ruser (_) = noAction;
    method rvalid(_) = noAction;
    method rready = True;
  endmodule
endinstance

instance Sink#(RLiteMaster#(a));
  module mkSink(RLiteMaster#(a));
    method rdata (_) = noAction;
    method rresp (_) = noAction;
    method rvalid(_) = noAction;
    method rready = True;
  endmodule
endinstance

instance NullSource#(RSlave#(a, b, c));
  module mkNullSource(RSlave#(a, b, c));
    method rid    = ?;
    method rdata  = ?;
    method rresp  = ?;
    method rlast  = ?;
    method ruser  = ?;
    method rvalid = False;
    method rready(_) = noAction;
  endmodule
endinstance

instance NullSource#(RLiteSlave#(a));
  module mkNullSource(RLiteSlave#(a));
    method rdata  = ?;
    method rresp  = ?;
    method rvalid = False;
    method rready(_) = noAction;
  endmodule
endinstance

////////////////
// AXI Master //
////////////////////////////////////////////////////////////////////////////////

// Shim for AXIMaster to FIFOFs#(AXIFlits)

interface AXIMasterShim#(
  numeric type id_,
  numeric type addr_,
  numeric type data_,
  numeric type user_);
  interface AXIMaster#(id_, addr_, data_, user_) master;
  interface FIFOF#(AWFlit#(id_, addr_, user_))   awff;
  interface FIFOF#(WFlit#(data_, user_))         wff;
  interface FIFOF#(BFlit#(id_, user_))           bff;
  interface FIFOF#(ARFlit#(id_, addr_, user_))   arff;
  interface FIFOF#(RFlit#(id_, data_, user_))    rff;
endinterface

module mkAXIMasterShim (AXIMasterShim#(id_, addr_, data_, user_));
  let awshim <- mkAWMasterShim;
  let  wshim <- mkWMasterShim;
  let  bshim <- mkBMasterShim;
  let arshim <- mkARMasterShim;
  let  rshim <- mkRMasterShim;
  interface master = interface AXIMaster;
    interface aw = awshim.master;
    interface  w = wshim.master;
    interface  b = bshim.master;
    interface ar = arshim.master;
    interface  r = rshim.master;
  endinterface;
  interface awff = awshim.fifof;
  interface  wff = wshim.fifof;
  interface  bff = bshim.fifof;
  interface arff = arshim.fifof;
  interface  rff = rshim.fifof;
endmodule

interface AXILiteMasterShim#(numeric type addr_, numeric type data_);
  interface AXILiteMaster#(addr_, data_) master;
  interface FIFOF#(AWLiteFlit#(addr_))   awff;
  interface FIFOF#(WLiteFlit#(data_))    wff;
  interface FIFOF#(BLiteFlit)            bff;
  interface FIFOF#(ARLiteFlit#(addr_))   arff;
  interface FIFOF#(RLiteFlit#(data_))    rff;
endinterface

module mkAXILiteMasterShim (AXILiteMasterShim#(addr_, data_));
  let awshim <- mkAWLiteMasterShim;
  let  wshim <- mkWLiteMasterShim;
  let  bshim <- mkBLiteMasterShim;
  let arshim <- mkARLiteMasterShim;
  let  rshim <- mkRLiteMasterShim;
  interface master = interface AXILiteMaster;
    interface aw = awshim.master;
    interface  w = wshim.master;
    interface  b = bshim.master;
    interface ar = arshim.master;
    interface  r = rshim.master;
  endinterface;
  interface awff = awshim.fifof;
  interface  wff = wshim.fifof;
  interface  bff = bshim.fifof;
  interface arff = arshim.fifof;
  interface  rff = rshim.fifof;
endmodule

///////////////
// AXI Slave //
////////////////////////////////////////////////////////////////////////////////

// Shim for AXISlave to FIFOFs#(AXIFlits)

interface AXISlaveShim#(
  numeric type id_,
  numeric type addr_,
  numeric type data_,
  numeric type user_);
  interface AXISlave#(id_, addr_, data_, user_) slave;
  interface FIFOF#(AWFlit#(id_, addr_, user_))  awff;
  interface FIFOF#(WFlit#(data_, user_))        wff;
  interface FIFOF#(BFlit#(id_, user_))          bff;
  interface FIFOF#(ARFlit#(id_, addr_, user_))  arff;
  interface FIFOF#(RFlit#(id_, data_, user_))   rff;
endinterface

module mkAXISlaveShim (AXISlaveShim#(id_, addr_, data_, user_));
  let awshim <- mkAWSlaveShim;
  let  wshim <- mkWSlaveShim;
  let  bshim <- mkBSlaveShim;
  let arshim <- mkARSlaveShim;
  let  rshim <- mkRSlaveShim;
  interface slave = interface AXISlave;
    interface aw = awshim.slave;
    interface  w = wshim.slave;
    interface  b = bshim.slave;
    interface ar = arshim.slave;
    interface  r = rshim.slave;
  endinterface;
  interface awff = awshim.fifof;
  interface  wff = wshim.fifof;
  interface  bff = bshim.fifof;
  interface arff = arshim.fifof;
  interface  rff = rshim.fifof;
endmodule

interface AXILiteSlaveShim#(numeric type addr_, numeric type data_);
  interface AXILiteSlave#(addr_, data_) slave;
  interface FIFOF#(AWLiteFlit#(addr_))  awff;
  interface FIFOF#(WLiteFlit#(data_))   wff;
  interface FIFOF#(BLiteFlit)           bff;
  interface FIFOF#(ARLiteFlit#(addr_))  arff;
  interface FIFOF#(RLiteFlit#(data_))   rff;
endinterface

module mkAXILiteSlaveShim (AXILiteSlaveShim#(addr_, data_));
  let awshim <- mkAWLiteSlaveShim;
  let  wshim <- mkWLiteSlaveShim;
  let  bshim <- mkBLiteSlaveShim;
  let arshim <- mkARLiteSlaveShim;
  let  rshim <- mkRLiteSlaveShim;
  interface slave = interface AXILiteSlave;
    interface aw = awshim.slave;
    interface  w = wshim.slave;
    interface  b = bshim.slave;
    interface ar = arshim.slave;
    interface  r = rshim.slave;
  endinterface;
  interface awff = awshim.fifof;
  interface  wff = wshim.fifof;
  interface  bff = bshim.fifof;
  interface arff = arshim.fifof;
  interface  rff = rshim.fifof;
endmodule

///////////////////////////
// AXI NullSource / Sink //
////////////////////////////////////////////////////////////////////////////////

instance NullSource#(AXIMaster#(a, b, c, d));
  module mkNullSource(AXIMaster#(a, b, c, d));
    let aw_source <- mkNullSource;
    let w_source  <- mkNullSource;
    let b_sink    <- mkSink;
    let ar_source <- mkNullSource;
    let r_sink    <- mkSink;
    interface aw = aw_source;
    interface w  = w_source;
    interface b  = b_sink;
    interface ar = ar_source;
    interface r  = r_sink;
  endmodule
endinstance

instance Sink#(AXISlave#(a, b, c, d));
  module mkSink(AXISlave#(a, b, c, d));
    let aw_sink  <- mkSink;
    let w_sink   <- mkSink;
    let b_source <- mkNullSource;
    let ar_sink  <- mkSink;
    let r_source <- mkNullSource;
    interface aw = aw_sink;
    interface w  = w_sink;
    interface b  = b_source;
    interface ar = ar_sink;
    interface r  = r_source;
  endmodule
endinstance
