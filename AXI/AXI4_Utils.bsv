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

// AXI imports
import AXI4_Types :: *;
import AXI4_AW_Utils :: *;
import AXI4_W_Utils :: *;
import AXI4_B_Utils :: *;
import AXI4_AR_Utils :: *;
import AXI4_R_Utils :: *;

// BlueBasics import
import SourceSink :: *;

// Standard
import FIFOF :: *;
import SpecialFIFOs :: *;

//////////////////////
// Util Typeclasses //
////////////////////////////////////////////////////////////////////////////////

// NullSource / NullSink
typeclass NullSource#(type a);
  module mkNullSource(a);
endtypeclass

typeclass NullSink#(type a);
  module mkNullSink(a);
endtypeclass

// Shim types and typeclasses

interface SinkAXI#(type t, type axi_t);
  interface Sink#(t) sink;
  interface axi_t axi;
endinterface

typeclass MkSinkAXI#(type t, type axi_t) dependencies (axi_t determines t);
  module mkSinkAXI (SinkAXI#(t, axi_t));
endtypeclass

interface SourceAXI#(type t, type axi_t);
  interface Source#(t) source;
  interface axi_t axi;
endinterface

typeclass MkSourceAXI#(type t, type axi_t) dependencies (axi_t determines t);
  module mkSourceAXI (SourceAXI#(t, axi_t));
endtypeclass

///////////////////////////////
// AXI Address Write Channel //
////////////////////////////////////////////////////////////////////////////////

// NullSource / NullSink instances

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

instance NullSink#(AWSlave#(a, b, c));
  module mkNullSink(AWSlave#(a, b, c));
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

instance NullSink#(AWLiteSlave#(a));
  module mkNullSink(AWLiteSlave#(a));
    method awaddr  (_) = noAction;
    method awprot  (_) = noAction;
    method awvalid (_) = noAction;
    method awready = True;
  endmodule
endinstance

// Shim instances

instance MkSinkAXI#(t, AWMaster#(id_, addr_, user_))
  provisos (Bits#(t, t_sz), ToAXIAWFlit#(t, id_, addr_, user_));
  module mkSinkAXI (SinkAXI#(t, AWMaster#(id_, addr_, user_)));
    let ff     <- mkBypassFIFOF;
    let master <- toAXIAWMaster(ff);
    interface sink = toSink(ff);
    interface axi  = master;
  endmodule
endinstance

instance MkSinkAXI#(t, AWLiteMaster#(addr_))
  provisos (Bits#(t, t_sz), ToAXIAWLiteFlit#(t, addr_));
  module mkSinkAXI (SinkAXI#(t, AWLiteMaster#(addr_)));
    let ff     <- mkBypassFIFOF;
    let master <- toAXIAWLiteMaster(ff);
    interface sink = toSink(ff);
    interface axi  = master;
  endmodule
endinstance

instance MkSourceAXI#(t, AWSlave#(id_, addr_, user_))
  provisos (Bits#(t, t_sz), FromAXIAWFlit#(t, id_, addr_, user_));
  module mkSourceAXI (SourceAXI#(t, AWSlave#(id_, addr_, user_)));
    let ff    <- mkBypassFIFOF;
    let slave <- toAXIAWSlave(ff);
    interface source = toSource(ff);
    interface axi    = slave;
  endmodule
endinstance

instance MkSourceAXI#(t, AWLiteSlave#(addr_))
  provisos (Bits#(t, t_sz), FromAXIAWLiteFlit#(t, addr_));
  module mkSourceAXI (SourceAXI#(t, AWLiteSlave#(addr_)));
    let ff    <- mkBypassFIFOF;
    let slave <- toAXIAWLiteSlave(ff);
    interface source = toSource(ff);
    interface axi    = slave;
  endmodule
endinstance

////////////////////////////
// AXI Write Data Channel //
////////////////////////////////////////////////////////////////////////////////

// NullSource / NullSink instances

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

instance NullSink#(WSlave#(a, b));
  module mkNullSink(WSlave#(a, b));
    method wdata (_) = noAction;
    method wstrb (_) = noAction;
    method wlast (_) = noAction;
    method wuser (_) = noAction;
    method wvalid(_) = noAction;
    method wready = True;
  endmodule
endinstance

instance NullSink#(WLiteSlave#(a));
  module mkNullSink(WLiteSlave#(a));
    method wdata (_) = noAction;
    method wstrb (_) = noAction;
    method wvalid(_) = noAction;
    method wready = True;
  endmodule
endinstance

// Shim instances

instance MkSinkAXI#(t, WMaster#(data_, user))
  provisos (Bits#(t, t_sz), ToAXIWFlit#(t, data_, user));
  module mkSinkAXI (SinkAXI#(t, WMaster#(data_, user)));
    let ff     <- mkBypassFIFOF;
    let master <- toAXIWMaster(ff);
    interface sink = toSink(ff);
    interface axi  = master;
  endmodule
endinstance

instance MkSinkAXI#(t, WLiteMaster#(data_))
  provisos (Bits#(t, t_sz), ToAXIWLiteFlit#(t, data_));
  module mkSinkAXI (SinkAXI#(t, WLiteMaster#(data_)));
    let ff     <- mkBypassFIFOF;
    let master <- toAXIWLiteMaster(ff);
    interface sink = toSink(ff);
    interface axi  = master;
  endmodule
endinstance

instance MkSourceAXI#(t, WSlave#(data_, user))
  provisos (Bits#(t, t_sz), FromAXIWFlit#(t, data_, user));
  module mkSourceAXI (SourceAXI#(t, WSlave#(data_, user)));
    let ff    <- mkBypassFIFOF;
    let slave <- toAXIWSlave(ff);
    interface source = toSource(ff);
    interface axi    = slave;
  endmodule
endinstance

instance MkSourceAXI#(t, WLiteSlave#(data_))
  provisos (Bits#(t, t_sz), FromAXIWLiteFlit#(t, data_));
  module mkSourceAXI (SourceAXI#(t, WLiteSlave#(data_)));
    let ff    <- mkBypassFIFOF;
    let slave <- toAXIWLiteSlave(ff);
    interface source = toSource(ff);
    interface axi    = slave;
  endmodule
endinstance

////////////////////////////////
// AXI Write Response Channel //
////////////////////////////////////////////////////////////////////////////////

// NullSource / NullSink instances

instance NullSink#(BMaster#(a, b));
  module mkNullSink(BMaster#(a, b));
    method bid   (_) = noAction;
    method bresp (_) = noAction;
    method buser (_) = noAction;
    method bvalid(_) = noAction;
    method bready = True;
  endmodule
endinstance

instance NullSink#(BLiteMaster);
  module mkNullSink(BLiteMaster);
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

// Shim instances

instance MkSourceAXI#(t, BMaster#(id_, user_))
  provisos (Bits#(t, t_sz), FromAXIBFlit#(t, id_, user_));
  module mkSourceAXI (SourceAXI#(t, BMaster#(id_, user_)));
    let ff     <- mkBypassFIFOF;
    let master <- toAXIBMaster(ff);
    interface source = toSource(ff);
    interface axi    = master;
  endmodule
endinstance

instance MkSourceAXI#(t, BLiteMaster)
  provisos (Bits#(t, t_sz), FromAXIBLiteFlit#(t));
  module mkSourceAXI (SourceAXI#(t, BLiteMaster));
    let ff     <- mkBypassFIFOF;
    let master <- toAXIBLiteMaster(ff);
    interface source = toSource(ff);
    interface axi    = master;
  endmodule
endinstance

instance MkSinkAXI#(t, BSlave#(id_, user_))
  provisos (Bits#(t, t_sz), ToAXIBFlit#(t, id_, user_));
  module mkSinkAXI (SinkAXI#(t, BSlave#(id_, user_)));
    let ff    <- mkBypassFIFOF;
    let slave <- toAXIBSlave(ff);
    interface sink = toSink(ff);
    interface axi  = slave;
  endmodule
endinstance

instance MkSinkAXI#(t, BLiteSlave)
  provisos (Bits#(t, t_sz), ToAXIBLiteFlit#(t));
  module mkSinkAXI (SinkAXI#(t, BLiteSlave));
    let ff    <- mkBypassFIFOF;
    let slave <- toAXIBLiteSlave(ff);
    interface sink = toSink(ff);
    interface axi  = slave;
  endmodule
endinstance

//////////////////////////////
// AXI Read Address Channel //
////////////////////////////////////////////////////////////////////////////////

// NullSource / NullSink instances

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

instance NullSink#(ARSlave#(a, b, c));
  module mkNullSink(ARSlave#(a, b, c));
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

instance NullSink#(ARLiteSlave#(a));
  module mkNullSink(ARLiteSlave#(a));
    method araddr  (_) = noAction;
    method arprot  (_) = noAction;
    method arvalid (_) = noAction;
    method arready = True;
  endmodule
endinstance

// Shim instances

instance MkSinkAXI#(t, ARMaster#(id_, addr_, user_))
  provisos (Bits#(t, t_sz), ToAXIARFlit#(t, id_, addr_, user_));
  module mkSinkAXI (SinkAXI#(t, ARMaster#(id_, addr_, user_)));
    let ff     <- mkBypassFIFOF;
    let master <- toAXIARMaster(ff);
    interface sink = toSink(ff);
    interface axi  = master;
  endmodule
endinstance

instance MkSinkAXI#(t, ARLiteMaster#(addr_))
  provisos (Bits#(t, t_sz), ToAXIARLiteFlit#(t, addr_));
  module mkSinkAXI (SinkAXI#(t, ARLiteMaster#(addr_)));
    let ff     <- mkBypassFIFOF;
    let master <- toAXIARLiteMaster(ff);
    interface sink = toSink(ff);
    interface axi  = master;
  endmodule
endinstance

instance MkSourceAXI#(t, ARSlave#(id_, addr_, user_))
  provisos (Bits#(t, t_sz), FromAXIARFlit#(t, id_, addr_, user_));
  module mkSourceAXI (SourceAXI#(t, ARSlave#(id_, addr_, user_)));
    let ff    <- mkBypassFIFOF;
    let slave <- toAXIARSlave(ff);
    interface source = toSource(ff);
    interface axi    = slave;
  endmodule
endinstance

instance MkSourceAXI#(t, ARLiteSlave#(addr_))
  provisos (Bits#(t, t_sz), FromAXIARLiteFlit#(t, addr_));
  module mkSourceAXI (SourceAXI#(t, ARLiteSlave#(addr_)));
    let ff    <- mkBypassFIFOF;
    let slave <- toAXIARLiteSlave(ff);
    interface source = toSource(ff);
    interface axi    = slave;
  endmodule
endinstance

///////////////////////////
// AXI Read Data Channel //
////////////////////////////////////////////////////////////////////////////////

// NullSource / NullSink instances

instance NullSink#(RMaster#(a, b, c));
  module mkNullSink(RMaster#(a, b, c));
    method rid   (_) = noAction;
    method rdata (_) = noAction;
    method rresp (_) = noAction;
    method rlast (_) = noAction;
    method ruser (_) = noAction;
    method rvalid(_) = noAction;
    method rready = True;
  endmodule
endinstance

instance NullSink#(RLiteMaster#(a));
  module mkNullSink(RLiteMaster#(a));
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

// Shim instances

instance MkSourceAXI#(t, RMaster#(id_, data_, user_))
  provisos (Bits#(t, t_sz), FromAXIRFlit#(t, id_, data_, user_));
  module mkSourceAXI (SourceAXI#(t, RMaster#(id_, data_, user_)));
    let ff     <- mkBypassFIFOF;
    let master <- toAXIRMaster(ff);
    interface source = toSource(ff);
    interface axi    = master;
  endmodule
endinstance

instance MkSourceAXI#(t, RLiteMaster#(data_))
  provisos (Bits#(t, t_sz), FromAXIRLiteFlit#(t, data_));
  module mkSourceAXI (SourceAXI#(t, RLiteMaster#(data_)));
    let ff     <- mkBypassFIFOF;
    let master <- toAXIRLiteMaster(ff);
    interface source = toSource(ff);
    interface axi    = master;
  endmodule
endinstance

instance MkSinkAXI#(t, RSlave#(id_, data_, user_))
  provisos (Bits#(t, t_sz), ToAXIRFlit#(t, id_, data_, user_));
  module mkSinkAXI (SinkAXI#(t, RSlave#(id_, data_, user_)));
    let ff    <- mkBypassFIFOF;
    let slave <- toAXIRSlave(ff);
    interface sink = toSink(ff);
    interface axi  = slave;
  endmodule
endinstance

instance MkSinkAXI#(t, RLiteSlave#(data_))
  provisos (Bits#(t, t_sz), ToAXIRLiteFlit#(t, data_));
  module mkSinkAXI (SinkAXI#(t, RLiteSlave#(data_)));
    let ff    <- mkBypassFIFOF;
    let slave <- toAXIRLiteSlave(ff);
    interface sink = toSink(ff);
    interface axi  = slave;
  endmodule
endinstance

////////////////
// AXI Master //
////////////////////////////////////////////////////////////////////////////////

// Shim for AXIMaster

interface AXIMasterShim#(
  numeric type id_,
  numeric type addr_,
  numeric type data_,
  numeric type user_);
  interface AXIMaster#(id_, addr_, data_, user_) master;
  interface Sink#(AWFlit#(id_, addr_, user_))    awSink;
  interface Sink#(WFlit#(data_, user_))          wSink;
  interface Source#(BFlit#(id_, user_))          bSource;
  interface Sink#(ARFlit#(id_, addr_, user_))    arSink;
  interface Source#(RFlit#(id_, data_, user_))   rSource;
endinterface

module mkAXIMasterShim (AXIMasterShim#(id_, addr_, data_, user_))
  provisos (
    MkSinkAXI#(AWFlit#(id_, addr_, user_), AWMaster#(id_, addr_, user_)),
    MkSinkAXI#(WFlit#(data_, user_), WMaster#(data_, user_)),
    MkSourceAXI#(BFlit#(id_, user_), BMaster#(id_, user_)),
    MkSinkAXI#(ARFlit#(id_, addr_, user_), ARMaster#(id_, addr_, user_)),
    MkSourceAXI#(RFlit#(id_, data_, user_), RMaster#(id_, data_, user_))
  );
  let awshim <- mkSinkAXI;
  let  wshim <- mkSinkAXI;
  let  bshim <- mkSourceAXI;
  let arshim <- mkSinkAXI;
  let  rshim <- mkSourceAXI;
  interface master = interface AXIMaster;
    interface aw = awshim.axi;
    interface  w = wshim.axi;
    interface  b = bshim.axi;
    interface ar = arshim.axi;
    interface  r = rshim.axi;
  endinterface;
  interface  awSink = awshim.sink;
  interface   wSink = wshim.sink;
  interface bSource = bshim.source;
  interface  arSink = arshim.sink;
  interface rSource = rshim.source;
endmodule

interface AXILiteMasterShim#(numeric type addr_, numeric type data_);
  interface AXILiteMaster#(addr_, data_) master;
  interface Sink#(AWLiteFlit#(addr_))    awSink;
  interface Sink#(WLiteFlit#(data_))     wSink;
  interface Source#(BLiteFlit)           bSource;
  interface Sink#(ARLiteFlit#(addr_))    arSink;
  interface Source#(RLiteFlit#(data_))   rSource;
endinterface

module mkAXILiteMasterShim (AXILiteMasterShim#(addr_, data_))
  provisos (
    MkSinkAXI#(AWLiteFlit#(addr_), AWLiteMaster#(addr_)),
    MkSinkAXI#(WLiteFlit#(data_), WLiteMaster#(data_)),
    MkSourceAXI#(BLiteFlit, BLiteMaster),
    MkSinkAXI#(ARLiteFlit#(addr_), ARLiteMaster#(addr_)),
    MkSourceAXI#(RLiteFlit#(data_), RLiteMaster#(data_))
  );
  let awshim <- mkSinkAXI;
  let  wshim <- mkSinkAXI;
  let  bshim <- mkSourceAXI;
  let arshim <- mkSinkAXI;
  let  rshim <- mkSourceAXI;
  interface master = interface AXILiteMaster;
    interface aw = awshim.axi;
    interface  w = wshim.axi;
    interface  b = bshim.axi;
    interface ar = arshim.axi;
    interface  r = rshim.axi;
  endinterface;
  interface  awSink = awshim.sink;
  interface   wSink = wshim.sink;
  interface bSource = bshim.source;
  interface  arSink = arshim.sink;
  interface rSource = rshim.source;
endmodule

///////////////
// AXI Slave //
////////////////////////////////////////////////////////////////////////////////

// Shim for AXISlave

interface AXISlaveShim#(
  numeric type id_,
  numeric type addr_,
  numeric type data_,
  numeric type user_);
  interface AXISlave#(id_, addr_, data_, user_) slave;
  interface Source#(AWFlit#(id_, addr_, user_)) awSource;
  interface Source#(WFlit#(data_, user_))       wSource;
  interface Sink#(BFlit#(id_, user_))           bSink;
  interface Source#(ARFlit#(id_, addr_, user_)) arSource;
  interface Sink#(RFlit#(id_, data_, user_))    rSink;
endinterface

module mkAXISlaveShim (AXISlaveShim#(id_, addr_, data_, user_))
  provisos (
    MkSourceAXI#(AWFlit#(id_, addr_, user_), AWSlave#(id_, addr_, user_)),
    MkSourceAXI#(WFlit#(data_, user_), WSlave#(data_, user_)),
    MkSinkAXI#(BFlit#(id_, user_), BSlave#(id_, user_)),
    MkSourceAXI#(ARFlit#(id_, addr_, user_), ARSlave#(id_, addr_, user_)),
    MkSinkAXI#(RFlit#(id_, data_, user_), RSlave#(id_, data_, user_))
  );
  let awshim <- mkSourceAXI;
  let  wshim <- mkSourceAXI;
  let  bshim <- mkSinkAXI;
  let arshim <- mkSourceAXI;
  let  rshim <- mkSinkAXI;
  interface slave = interface AXISlave;
    interface aw = awshim.axi;
    interface  w = wshim.axi;
    interface  b = bshim.axi;
    interface ar = arshim.axi;
    interface  r = rshim.axi;
  endinterface;
  interface awSource = awshim.source;
  interface  wSource = wshim.source;
  interface    bSink = bshim.sink;
  interface arSource = arshim.source;
  interface    rSink = rshim.sink;
endmodule

interface AXILiteSlaveShim#(numeric type addr_, numeric type data_);
  interface AXILiteSlave#(addr_, data_) slave;
  interface Source#(AWLiteFlit#(addr_)) awSource;
  interface Source#(WLiteFlit#(data_))  wSource;
  interface Sink#(BLiteFlit)            bSink;
  interface Source#(ARLiteFlit#(addr_)) arSource;
  interface Sink#(RLiteFlit#(data_))    rSink;
endinterface

module mkAXILiteSlaveShim (AXILiteSlaveShim#(addr_, data_))
  provisos (
    MkSourceAXI#(AWLiteFlit#(addr_), AWLiteSlave#(addr_)),
    MkSourceAXI#(WLiteFlit#(data_), WLiteSlave#(data_)),
    MkSinkAXI#(BLiteFlit, BLiteSlave),
    MkSourceAXI#(ARLiteFlit#(addr_), ARLiteSlave#(addr_)),
    MkSinkAXI#(RLiteFlit#(data_), RLiteSlave#(data_))
  );
  let awshim <- mkSourceAXI;
  let  wshim <- mkSourceAXI;
  let  bshim <- mkSinkAXI;
  let arshim <- mkSourceAXI;
  let  rshim <- mkSinkAXI;
  interface slave = interface AXILiteSlave;
    interface aw = awshim.axi;
    interface  w = wshim.axi;
    interface  b = bshim.axi;
    interface ar = arshim.axi;
    interface  r = rshim.axi;
  endinterface;
  interface awSource = awshim.source;
  interface  wSource = wshim.source;
  interface    bSink = bshim.sink;
  interface arSource = arshim.source;
  interface    rSink = rshim.sink;
endmodule

///////////////////////////////
// AXI NullSource / NullSink //
////////////////////////////////////////////////////////////////////////////////

instance NullSource#(AXIMaster#(a, b, c, d));
  module mkNullSource(AXIMaster#(a, b, c, d));
    let aw_source <- mkNullSource;
    let w_source  <- mkNullSource;
    let b_sink    <- mkNullSink;
    let ar_source <- mkNullSource;
    let r_sink    <- mkNullSink;
    interface aw = aw_source;
    interface w  = w_source;
    interface b  = b_sink;
    interface ar = ar_source;
    interface r  = r_sink;
  endmodule
endinstance

instance NullSink#(AXISlave#(a, b, c, d));
  module mkNullSink(AXISlave#(a, b, c, d));
    let aw_sink  <- mkNullSink;
    let w_sink   <- mkNullSink;
    let b_source <- mkNullSource;
    let ar_sink  <- mkNullSink;
    let r_source <- mkNullSource;
    interface aw = aw_sink;
    interface w  = w_sink;
    interface b  = b_source;
    interface ar = ar_sink;
    interface r  = r_source;
  endmodule
endinstance
