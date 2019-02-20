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

import Connectable :: *;
import DefaultValue :: *;

// BlueStuff import
import Routable :: *;
// BlueBasics import
import SourceSink :: *;

import AXI4_AXI4Lite_Types :: *;

//////////////////
// helper types //
////////////////////////////////////////////////////////////////////////////////

// return an interface acting as a dead end
typeclass CulDeSac#(type t);
  function t culDeSac;
endtypeclass

////////////////////////////////
// AXI4 Address Write Channel //
////////////////////////////////////////////////////////////////////////////////

// Flit type
typedef struct {
  Bit#(id_)   awid;
  Bit#(addr_) awaddr;
  AXI4_Len    awlen;
  AXI4_Size   awsize;
  AXI4_Burst  awburst;
  AXI4_Lock   awlock;
  AXI4_Cache  awcache;
  AXI4_Prot   awprot;
  AXI4_QoS    awqos;
  AXI4_Region awregion;
  Bit#(user_) awuser;
} AXI4_AWFlit#(numeric type id_, numeric type addr_, numeric type user_)
deriving (Bits, FShow);
instance DefaultValue#(AXI4_AWFlit#(id_, addr_, user_));
  function defaultValue = AXI4_AWFlit {
    awid: 0, awaddr: ?, awlen: 0, awsize: 0,
    awburst: INCR, awlock: NORMAL, awcache: 0,
    awprot: 0, awqos: 0, awregion: 0, awuser: ?
  };
endinstance
instance Routable#(
  AXI4_AWFlit#(id_, addr_, awuser_),
  AXI4_BFlit#(id_, buser_),
  Bit#(addr_));
  function routingField(x) = x.awaddr;
  function noRouteFound(x) = AXI4_BFlit {
    bid: x.awid, bresp: DECERR, buser: ?
  };
endinstance
instance DetectLast#(AXI4_AWFlit#(id_, addr_, user_));
  function detectLast(x) = True;
endinstance

// Master interfaces
(* always_ready, always_enabled *)
interface AXI4_AW_Master_Synth#(numeric type id_,
                                numeric type addr_,
                                numeric type user_);
  method Bit#(id_)   awid;
  method Bit#(addr_) awaddr;
  method AXI4_Len    awlen;
  method AXI4_Size   awsize;
  method AXI4_Burst  awburst;
  method AXI4_Lock   awlock;
  method AXI4_Cache  awcache;
  method AXI4_Prot   awprot;
  method AXI4_QoS    awqos;
  method AXI4_Region awregion;
  method Bit#(user_) awuser;
  method Bool        awvalid;
  (* prefix="" *) method Action awready(Bool awready);
endinterface

instance CulDeSac#(AXI4_AW_Master_Synth#(id_, addr_, user_));
  function culDeSac = interface AXI4_AW_Master_Synth;
    method awid       = ?;
    method awaddr     = ?;
    method awlen      = ?;
    method awsize     = ?;
    method awburst    = ?;
    method awlock     = ?;
    method awcache    = ?;
    method awprot     = ?;
    method awqos      = ?;
    method awregion   = ?;
    method awuser     = ?;
    method awvalid    = False;
    method awready(_) = noAction;
  endinterface;
endinstance

/*
interface AXI4_AW_Master_Xactor#(numeric type id_,
                                 numeric type addr_,
                                 numeric type user_);
  method Action reset;
  interface Sink#(AXI4_AWFlit#(id_, addr_, user_))   sink;
  interface AXI4_AW_Master_Synth#(id_, addr_, user_) masterSynth;
endinterface
*/

// Slave interfaces
(* always_ready, always_enabled *)
interface AXI4_AW_Slave_Synth#(numeric type id_,
                               numeric type addr_,
                               numeric type user_);
  (* prefix="" *) method Action awid    (Bit#(id_)   awid);
  (* prefix="" *) method Action awaddr  (Bit#(addr_) awaddr);
  (* prefix="" *) method Action awlen   (AXI4_Len    awlen);
  (* prefix="" *) method Action awsize  (AXI4_Size   awsize);
  (* prefix="" *) method Action awburst (AXI4_Burst  awburst);
  (* prefix="" *) method Action awlock  (AXI4_Lock   awlock);
  (* prefix="" *) method Action awcache (AXI4_Cache  awcache);
  (* prefix="" *) method Action awprot  (AXI4_Prot   awprot);
  (* prefix="" *) method Action awqos   (AXI4_QoS    awqos);
  (* prefix="" *) method Action awregion(AXI4_Region awregion);
  (* prefix="" *) method Action awuser  (Bit#(user_) awuser);
  (* prefix="" *) method Action awvalid (Bool        awvalid);
  method Bool awready;
endinterface

instance CulDeSac#(AXI4_AW_Slave_Synth#(id_, addr_, user_));
  function culDeSac = interface AXI4_AW_Slave_Synth;
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
    method awready     = False;
  endinterface;
endinstance

/*
interface AXI4_AW_Slave_Xactor#(numeric type id_,
                                numeric type addr_,
                                numeric type user_);
  method Action reset;
  interface Source#(AXI4_AWFlit#(id_, addr_, user_)) source;
  interface AXI4_AW_Slave_Synth#(id_, addr_, user_)  slaveSynth;
endinterface
*/

// connectable instances
instance Connectable#(AXI4_AW_Master_Synth#(a, b, c),
                      AXI4_AW_Slave_Synth#(a, b, c));
  module mkConnection#(AXI4_AW_Master_Synth#(a, b, c) m,
                       AXI4_AW_Slave_Synth#(a, b, c) s) (Empty);
    rule connect_awid;     s.awid(m.awid);         endrule
    rule connect_awaddr;   s.awaddr(m.awaddr);     endrule
    rule connect_awlen;    s.awlen(m.awlen);       endrule
    rule connect_awsize;   s.awsize(m.awsize);     endrule
    rule connect_awburst;  s.awburst(m.awburst);   endrule
    rule connect_awlock;   s.awlock(m.awlock);     endrule
    rule connect_awcache;  s.awcache(m.awcache);   endrule
    rule connect_awprot;   s.awprot(m.awprot);     endrule
    rule connect_awqos;    s.awqos(m.awqos);       endrule
    rule connect_awregion; s.awregion(m.awregion); endrule
    rule connect_awuser;   s.awuser(m.awuser);     endrule
    rule connect_awvalid;  s.awvalid(m.awvalid);   endrule
    rule connect_awready;  m.awready(s.awready);   endrule
  endmodule
endinstance
instance Connectable#(AXI4_AW_Slave_Synth#(a, b, c),
                      AXI4_AW_Master_Synth#(a, b, c));
  module mkConnection#(AXI4_AW_Slave_Synth#(a, b, c) s,
                       AXI4_AW_Master_Synth#(a, b, c) m) (Empty);
    mkConnection(m, s);
  endmodule
endinstance

/////////////////////////////
// AXI4 Write Data Channel //
////////////////////////////////////////////////////////////////////////////////

// Flit type
typedef struct {
  Bit#(data_)           wdata;
  Bit#(TDiv#(data_, 8)) wstrb;
  Bool                  wlast;
  Bit#(user_)           wuser;
} AXI4_WFlit#(numeric type data_, numeric type user_) deriving (Bits, FShow);
instance DefaultValue#(AXI4_WFlit#(data_, user_));
  function defaultValue = AXI4_WFlit {
    wdata: ?, wstrb: ~0, wlast: True, wuser: ?
  };
endinstance
instance DetectLast#(AXI4_WFlit#(data_, user_));
  function detectLast(x) = x.wlast;
endinstance

// Master interfaces
(* always_ready, always_enabled *)
interface AXI4_W_Master_Synth#(numeric type data_, numeric type user_);
  method Bit#(data_)           wdata;
  method Bit#(TDiv#(data_, 8)) wstrb;
  method Bool                  wlast;
  method Bit#(user_)           wuser;
  method Bool                  wvalid;
  (* prefix="" *) method Action wready(Bool wready);
endinterface

instance CulDeSac#(AXI4_W_Master_Synth#(data_, user_));
  function culDeSac = interface AXI4_W_Master_Synth;
    method wdata     = ?;
    method wstrb     = ?;
    method wlast     = ?;
    method wuser     = ?;
    method wvalid    = False;
    method wready(_) = noAction;
  endinterface;
endinstance

/*
interface AXI4_W_Master_Xactor#(numeric type data_,
                                numeric type user_);
  method Action reset;
  interface Sink#(AXI4_WFlit#(data_, user_))   sink;
  interface AXI4_W_Master_Synth#(data_, user_) masterSynth;
endinterface
*/

// Slave interfaces
(* always_ready, always_enabled *)
interface AXI4_W_Slave_Synth#(numeric type data_, numeric type user_);
  (* prefix="" *) method Action wdata (Bit#(data_)            wdata);
  (* prefix="" *) method Action wstrb (Bit#(TDiv#(data_,  8)) wstrb);
  (* prefix="" *) method Action wlast (Bool                   wlast);
  (* prefix="" *) method Action wuser (Bit#(user_)            wuser);
  (* prefix="" *) method Action wvalid(Bool                   wvalid);
  method Bool wready;
endinterface

instance CulDeSac#(AXI4_W_Slave_Synth#(data_, user_));
  function culDeSac = interface AXI4_W_Slave_Synth;
    method wdata (_) = noAction;
    method wstrb (_) = noAction;
    method wlast (_) = noAction;
    method wuser (_) = noAction;
    method wvalid(_) = noAction;
    method wready    = False;
  endinterface;
endinstance

/*
interface AXI4_W_Slave_Xactor#(numeric type data_,
                               numeric type user_);
  method Action reset;
  interface Source#(AXI4_WFlit#(data_, user_)) source;
  interface AXI4_W_Slave_Synth#(data_, user_)  slaveSynth;
endinterface
*/

// connectable instances
instance Connectable#(AXI4_W_Master_Synth#(a, b), AXI4_W_Slave_Synth#(a, b));
  module mkConnection#(AXI4_W_Master_Synth#(a, b) m,
                       AXI4_W_Slave_Synth#(a, b) s)(Empty);
    rule connect_wdata;  s.wdata(m.wdata);   endrule
    rule connect_wstrb;  s.wstrb(m.wstrb);   endrule
    rule connect_wlast;  s.wlast(m.wlast);   endrule
    rule connect_wuser;  s.wuser(m.wuser);   endrule
    rule connect_wvalid; s.wvalid(m.wvalid); endrule
    rule connect_wready; m.wready(s.wready); endrule
  endmodule
endinstance
instance Connectable#(AXI4_W_Slave_Synth#(a, b), AXI4_W_Master_Synth#(a, b));
  module mkConnection#(AXI4_W_Slave_Synth#(a, b) s,
                       AXI4_W_Master_Synth#(a, b) m)(Empty);
    mkConnection(m, s);
  endmodule
endinstance

/////////////////////////////////
// AXI4 Write Response Channel //
////////////////////////////////////////////////////////////////////////////////

// Flit type
typedef struct {
  Bit#(id_)   bid;
  AXI4_Resp   bresp;
  Bit#(user_) buser;
} AXI4_BFlit#(numeric type id_, numeric type user_) deriving (Bits, FShow);
instance DefaultValue#(AXI4_BFlit#(id_, user_));
  function defaultValue = AXI4_BFlit { bid: 0, bresp: OKAY, buser: ? };
endinstance
instance DetectLast#(AXI4_BFlit#(id_, user_));
  function detectLast(x) = True;
endinstance

// Master interfaces
(* always_ready, always_enabled *)
interface AXI4_B_Master_Synth#(numeric type id_, numeric type user_);
  (* prefix="" *) method Action bid   (Bit#(id_)   bid);
  (* prefix="" *) method Action bresp (AXI4_Resp   bresp);
  (* prefix="" *) method Action buser (Bit#(user_) buser);
  (* prefix="" *) method Action bvalid(Bool        bvalid);
  method Bool bready;
endinterface

instance CulDeSac#(AXI4_B_Master_Synth#(id_, user_));
  function culDeSac = interface AXI4_B_Master_Synth;
    method bid   (_) = noAction;
    method bresp (_) = noAction;
    method buser (_) = noAction;
    method bvalid(_) = noAction;
    method bready    = False;
  endinterface;
endinstance

/*
interface AXI4_B_Master_Xactor#(numeric type id_,
                                numeric type user_);
  method Action reset;
  interface Source#(AXI4_BFlit#(id_, user_)) source;
  interface AXI4_B_Master_Synth#(id_, user_) masterSynth;
endinterface
*/

// Slave interfaces
(* always_ready, always_enabled *)
interface AXI4_B_Slave_Synth#(numeric type id_, numeric type user_);
  method Bit#(id_)   bid;
  method AXI4_Resp   bresp;
  method Bit#(user_) buser;
  method Bool        bvalid;
  (* prefix="" *) method Action bready(Bool bready);
endinterface

instance CulDeSac#(AXI4_B_Slave_Synth#(id_, user_));
  function culDeSac = interface AXI4_B_Slave_Synth;
    method bid       = ?;
    method bresp     = ?;
    method buser     = ?;
    method bvalid    = False;
    method bready(_) = noAction;
  endinterface;
endinstance

/*
interface AXI4_B_Slave_Xactor#(numeric type id_,
                               numeric type user_);
  method Action reset;
  interface Sink#(AXI4_BFlit#(id_, user_))  sink;
  interface AXI4_B_Slave_Synth#(id_, user_) slaveSynth;
endinterface
*/

// connectable instances
instance Connectable#(AXI4_B_Master_Synth#(a, b), AXI4_B_Slave_Synth#(a, b));
  module mkConnection#(AXI4_B_Master_Synth#(a, b) m,
                       AXI4_B_Slave_Synth#(a, b) s)(Empty);
    rule connect_bid;    m.bid(s.bid);       endrule
    rule connect_bresp;  m.bresp(s.bresp);   endrule
    rule connect_buser;  m.buser(s.buser);   endrule
    rule connect_bvalid; m.bvalid(s.bvalid); endrule
    rule connect_bready; s.bready(m.bready); endrule
  endmodule
endinstance
instance Connectable#(AXI4_B_Slave_Synth#(a, b), AXI4_B_Master_Synth#(a, b));
  module mkConnection#(AXI4_B_Slave_Synth#(a, b) s,
                       AXI4_B_Master_Synth#(a, b) m)(Empty);
    mkConnection(m, s);
  endmodule
endinstance

///////////////////////////////
// AXI4 Read Address Channel //
////////////////////////////////////////////////////////////////////////////////

// Flit type
typedef struct {
  Bit#(id_)   arid;
  Bit#(addr_) araddr;
  AXI4_Len    arlen;
  AXI4_Size   arsize;
  AXI4_Burst  arburst;
  AXI4_Lock   arlock;
  AXI4_Cache  arcache;
  AXI4_Prot   arprot;
  AXI4_QoS    arqos;
  AXI4_Region arregion;
  Bit#(user_) aruser;
} AXI4_ARFlit#(numeric type id_, numeric type addr_, numeric type user_)
deriving (Bits, FShow);
instance DefaultValue#(AXI4_ARFlit#(id_, addr_, user_));
  function defaultValue = AXI4_ARFlit {
    arid: 0, araddr: ?, arlen: 0, arsize: 0,
    arburst: INCR, arlock: NORMAL, arcache: 0,
    arprot: 0, arqos: 0, arregion: 0, aruser: ?
  };
endinstance
instance Routable#(
  AXI4_ARFlit#(id_, addr_, aruser_),
  AXI4_RFlit#(id_, data_, ruser_),
  Bit#(addr_));
  function routingField(x) = x.araddr;
  function noRouteFound(x) = AXI4_RFlit {
    rid: x.arid, rdata: ?, rresp: DECERR, rlast: True, ruser: ?
  };
endinstance
instance DetectLast#(AXI4_ARFlit#(id_, addr_, user_));
  function detectLast(x) = True;
endinstance

// Master interfaces
(* always_ready, always_enabled *)
interface AXI4_AR_Master_Synth#(numeric type id_,
                                numeric type addr_,
                                numeric type user_);
  method Bit#(id_)   arid;
  method Bit#(addr_) araddr;
  method AXI4_Len    arlen;
  method AXI4_Size   arsize;
  method AXI4_Burst  arburst;
  method AXI4_Lock   arlock;
  method AXI4_Cache  arcache;
  method AXI4_Prot   arprot;
  method AXI4_QoS    arqos;
  method AXI4_Region arregion;
  method Bit#(user_) aruser;
  method Bool        arvalid;
  (* prefix="" *) method Action arready(Bool arready);
endinterface

instance CulDeSac#(AXI4_AR_Master_Synth#(id_, addr_, user_));
  function culDeSac = interface AXI4_AR_Master_Synth;
    method arid       = ?;
    method araddr     = ?;
    method arlen      = ?;
    method arsize     = ?;
    method arburst    = ?;
    method arlock     = ?;
    method arcache    = ?;
    method arprot     = ?;
    method arqos      = ?;
    method arregion   = ?;
    method aruser     = ?;
    method arvalid    = False;
    method arready(_) = noAction;
  endinterface;
endinstance

/*
interface AXI4_AR_Master_Xactor#(numeric type id_,
                                 numeric type addr_,
                                 numeric type user_);
  method Action reset;
  interface Source#(AXI4_ARFlit#(id_, addr_, user_)) source;
  interface AXI4_AR_Master_Synth#(id_, addr_, user_) masterSynth;
endinterface
*/

// Slave interfaces
(* always_ready, always_enabled *)
interface AXI4_AR_Slave_Synth#(numeric type id_,
                               numeric type addr_,
                               numeric type user_);
  (* prefix="" *) method Action arid    (Bit#(id_)   arid);
  (* prefix="" *) method Action araddr  (Bit#(addr_) araddr);
  (* prefix="" *) method Action arlen   (AXI4_Len    arlen);
  (* prefix="" *) method Action arsize  (AXI4_Size   arsize);
  (* prefix="" *) method Action arburst (AXI4_Burst  arburst);
  (* prefix="" *) method Action arlock  (AXI4_Lock   arlock);
  (* prefix="" *) method Action arcache (AXI4_Cache  arcache);
  (* prefix="" *) method Action arprot  (AXI4_Prot   arprot);
  (* prefix="" *) method Action arqos   (AXI4_QoS    arqos);
  (* prefix="" *) method Action arregion(AXI4_Region arregion);
  (* prefix="" *) method Action aruser  (Bit#(user_) aruser);
  (* prefix="" *) method Action arvalid (Bool        arvalid);
  method Bool arready;
endinterface

instance CulDeSac#(AXI4_AR_Slave_Synth#(id_, addr_, user_));
  function culDeSac = interface AXI4_AR_Slave_Synth;
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
    method arready     = False;
  endinterface;
endinstance

/*
interface AXI4_AR_Slave_Xactor#(numeric type id_,
                                numeric type addr_,
                                numeric type user_);
  method Action reset;
  interface Sink#(AXI4_ARFlit#(id_, addr_, user_))  sink;
  interface AXI4_AR_Slave_Synth#(id_, addr_, user_) slaveSynth;
endinterface
*/

// connectable instances
instance Connectable#(AXI4_AR_Master_Synth#(a, b, c),
                      AXI4_AR_Slave_Synth#(a, b, c));
  module mkConnection#(AXI4_AR_Master_Synth#(a, b, c) m,
                       AXI4_AR_Slave_Synth#(a, b, c) s)(Empty);
    rule connect_arid;     s.arid(m.arid);         endrule
    rule connect_araddr;   s.araddr(m.araddr);     endrule
    rule connect_arlen;    s.arlen(m.arlen);       endrule
    rule connect_arsize;   s.arsize(m.arsize);     endrule
    rule connect_arburst;  s.arburst(m.arburst);   endrule
    rule connect_arlock;   s.arlock(m.arlock);     endrule
    rule connect_arcache;  s.arcache(m.arcache);   endrule
    rule connect_arprot;   s.arprot(m.arprot);     endrule
    rule connect_arqos;    s.arqos(m.arqos);       endrule
    rule connect_arregion; s.arregion(m.arregion); endrule
    rule connect_aruser;   s.aruser(m.aruser);     endrule
    rule connect_arvalid;  s.arvalid(m.arvalid);   endrule
    rule connect_arready;  m.arready(s.arready);   endrule
  endmodule
endinstance
instance Connectable#(AXI4_AR_Slave_Synth#(a, b, c),
                      AXI4_AR_Master_Synth#(a, b, c));
  module mkConnection#(AXI4_AR_Slave_Synth#(a, b, c) s,
                       AXI4_AR_Master_Synth#(a, b, c) m)(Empty);
    mkConnection(m, s);
  endmodule
endinstance

////////////////////////////
// AXI4 Read Data Channel //
////////////////////////////////////////////////////////////////////////////////

// Flit type
typedef struct {
  Bit#(id_)   rid;
  Bit#(data_) rdata;
  AXI4_Resp   rresp;
  Bool        rlast;
  Bit#(user_) ruser;
} AXI4_RFlit#(numeric type id_, numeric type data_, numeric type user_)
deriving (Bits, FShow);
instance DefaultValue#(AXI4_RFlit#(id_, data_, user_));
  function defaultValue = AXI4_RFlit {
    rid: 0, rdata: ?, rresp: OKAY, rlast: True, ruser: ?
  };
endinstance
instance DetectLast#(AXI4_RFlit#(id_, data_, user_));
  function detectLast(x) = x.rlast;
endinstance

// Master interfaces
(* always_ready, always_enabled *)
interface AXI4_R_Master_Synth#(numeric type id_,
                               numeric type data_,
                               numeric type user_);
  (* prefix="" *) method Action rid   (Bit#(id_)   rid);
  (* prefix="" *) method Action rdata (Bit#(data_) rdata);
  (* prefix="" *) method Action rresp (AXI4_Resp   rresp);
  (* prefix="" *) method Action rlast (Bool        rlast);
  (* prefix="" *) method Action ruser (Bit#(user_) ruser);
  (* prefix="" *) method Action rvalid(Bool        rvalid);
  method Bool rready;
endinterface

instance CulDeSac#(AXI4_R_Master_Synth#(id_, data_, user_));
  function culDeSac = interface AXI4_R_Master_Synth;
    method rid   (_) = noAction;
    method rdata (_) = noAction;
    method rresp (_) = noAction;
    method rlast (_) = noAction;
    method ruser (_) = noAction;
    method rvalid(_) = noAction;
    method rready    = False;
  endinterface;
endinstance

/*
interface AXI4_R_Master_Xactor#(numeric type id_,
                                numeric type data_,
                                numeric type user_);
  method Action reset;
  interface Source#(AXI4_RFlit#(id_, data_, user_)) source;
  interface AXI4_R_Master_Synth#(id_, data_, user_) masterSynth;
endinterface
*/

// Slave interfaces
(* always_ready, always_enabled *)
interface AXI4_R_Slave_Synth#(numeric type id_,
                              numeric type data_,
                              numeric type user_);
  method Bit#(id_)   rid;
  method Bit#(data_) rdata;
  method AXI4_Resp   rresp;
  method Bool        rlast;
  method Bit#(user_) ruser;
  method Bool        rvalid;
  (* prefix="" *) method Action rready(Bool rready);
endinterface

instance CulDeSac#(AXI4_R_Slave_Synth#(id_, data_, user_));
  function culDeSac = interface AXI4_R_Slave_Synth;
    method rid       = ?;
    method rdata     = ?;
    method rresp     = ?;
    method rlast     = ?;
    method ruser     = ?;
    method rvalid    = False;
    method rready(_) = noAction;
  endinterface;
endinstance

/*
interface AXI4_R_Slave_Xactor#(numeric type id_,
                               numeric type data_,
                               numeric type user_);
  method Action reset;
  interface Sink#(AXI4_RFlit#(id_, data_, user_))  sink;
  interface AXI4_R_Slave_Synth#(id_, data_, user_) slaveSynth;
endinterface
*/

// connectable instances
instance Connectable#(AXI4_R_Master_Synth#(a, b, c),
                      AXI4_R_Slave_Synth#(a, b, c));
  module mkConnection#(AXI4_R_Master_Synth#(a, b, c) m,
                       AXI4_R_Slave_Synth#(a, b, c) s)(Empty);
    rule connect_rid;    m.rid(s.rid);       endrule
    rule connect_rdata;  m.rdata(s.rdata);   endrule
    rule connect_rresp;  m.rresp(s.rresp);   endrule
    rule connect_rlast;  m.rlast(s.rlast);   endrule
    rule connect_ruser;  m.ruser(s.ruser);   endrule
    rule connect_rvalid; m.rvalid(s.rvalid); endrule
    rule connect_rready; s.rready(m.rready); endrule
  endmodule
endinstance
instance Connectable#(AXI4_R_Slave_Synth#(a, b, c),
                      AXI4_R_Master_Synth#(a, b, c));
  module mkConnection#(AXI4_R_Slave_Synth#(a, b, c) s,
                       AXI4_R_Master_Synth#(a, b, c) m)(Empty);
    mkConnection(m, s);
  endmodule
endinstance

/////////////////
// AXI4 Master //
////////////////////////////////////////////////////////////////////////////////

interface AXI4_Master#(
  numeric type id_,
  numeric type addr_,
  numeric type data_,
  numeric type awuser_,
  numeric type wuser_,
  numeric type buser_,
  numeric type aruser_,
  numeric type ruser_);
  interface Source#(AXI4_AWFlit#(id_, addr_, awuser_)) aw;
  interface Source#(AXI4_WFlit#(data_, wuser_))        w;
  interface Sink#(AXI4_BFlit#(id_, buser_))            b;
  interface Source#(AXI4_ARFlit#(id_, addr_, aruser_)) ar;
  interface Sink#(AXI4_RFlit#(id_, data_, ruser_))     r;
endinterface

instance CulDeSac#(AXI4_Master#(id_,
                                addr_,
                                data_,
                                awuser_,
                                wuser_,
                                buser_,
                                aruser_,
                                ruser_));
  function culDeSac = interface AXI4_Master;
    interface aw = nullSource;
    interface  w = nullSource;
    interface  b = nullSink;
    interface ar = nullSource;
    interface  r = nullSink;
  endinterface;
endinstance

interface AXI4_Master_Synth#(
  numeric type id_,
  numeric type addr_,
  numeric type data_,
  numeric type awuser_,
  numeric type wuser_,
  numeric type buser_,
  numeric type aruser_,
  numeric type ruser_);
  interface AXI4_AW_Master_Synth#(id_, addr_, awuser_) aw;
  interface AXI4_W_Master_Synth#(data_, wuser_)        w;
  interface AXI4_B_Master_Synth#(id_, buser_)          b;
  interface AXI4_AR_Master_Synth#(id_, addr_, aruser_) ar;
  interface AXI4_R_Master_Synth#(id_, data_, ruser_)   r;
endinterface

instance CulDeSac#(AXI4_Master_Synth#(id_,
                                      addr_,
                                      data_,
                                      awuser_,
                                      wuser_,
                                      buser_,
                                      aruser_,
                                      ruser_))
  provisos (
    CulDeSac#(AXI4_AW_Master_Synth#(id_, addr_, awuser_)),
    CulDeSac#(AXI4_W_Master_Synth#(data_, wuser_)),
    CulDeSac#(AXI4_B_Master_Synth#(id_, buser_)),
    CulDeSac#(AXI4_AR_Master_Synth#(id_, addr_, aruser_)),
    CulDeSac#(AXI4_R_Master_Synth#(id_, data_, ruser_))
  );
  function culDeSac = interface AXI4_Master_Synth;
    interface aw = culDeSac;
    interface  w = culDeSac;
    interface  b = culDeSac;
    interface ar = culDeSac;
    interface  r = culDeSac;
  endinterface;
endinstance

interface AXI4_Master_Xactor#(
  numeric type id_,
  numeric type addr_,
  numeric type data_,
  numeric type awuser_,
  numeric type wuser_,
  numeric type buser_,
  numeric type aruser_,
  numeric type ruser_);
  method Action clear;
  interface AXI4_Slave#(id_,
                        addr_,
                        data_,
                        awuser_,
                        wuser_,
                        buser_,
                        aruser_,
                        ruser_)        slave;
  interface AXI4_Master_Synth#(id_,
                               addr_,
                               data_,
                               awuser_,
                               wuser_,
                               buser_,
                               aruser_,
                               ruser_) masterSynth;
endinterface

////////////////
// AXI4 Slave //
////////////////////////////////////////////////////////////////////////////////

interface AXI4_Slave#(
  numeric type id_,
  numeric type addr_,
  numeric type data_,
  numeric type awuser_,
  numeric type wuser_,
  numeric type buser_,
  numeric type aruser_,
  numeric type ruser_);
  interface Sink#(AXI4_AWFlit#(id_, addr_, awuser_)) aw;
  interface Sink#(AXI4_WFlit#(data_, wuser_))        w;
  interface Source#(AXI4_BFlit#(id_, buser_))        b;
  interface Sink#(AXI4_ARFlit#(id_, addr_, aruser_)) ar;
  interface Source#(AXI4_RFlit#(id_, data_, ruser_)) r;
endinterface

instance CulDeSac#(AXI4_Slave#(id_,
                               addr_,
                               data_,
                               awuser_,
                               wuser_,
                               buser_,
                               aruser_,
                               ruser_));
  function culDeSac = interface AXI4_Slave;
    interface aw = nullSink;
    interface  w = nullSink;
    interface  b = nullSource;
    interface ar = nullSink;
    interface  r = nullSource;
  endinterface;
endinstance

interface AXI4_Slave_Synth#(
  numeric type id_,
  numeric type addr_,
  numeric type data_,
  numeric type awuser_,
  numeric type wuser_,
  numeric type buser_,
  numeric type aruser_,
  numeric type ruser_);
  interface AXI4_AW_Slave_Synth#(id_, addr_, awuser_) aw;
  interface AXI4_W_Slave_Synth#(data_, wuser_)        w;
  interface AXI4_B_Slave_Synth#(id_, buser_)          b;
  interface AXI4_AR_Slave_Synth#(id_, addr_, aruser_) ar;
  interface AXI4_R_Slave_Synth#(id_, data_, ruser_)   r;
endinterface

instance CulDeSac#(AXI4_Slave_Synth#(id_,
                                     addr_,
                                     data_,
                                     awuser_,
                                     wuser_,
                                     buser_,
                                     aruser_,
                                     ruser_))
  provisos (
    CulDeSac#(AXI4_AW_Slave_Synth#(id_, addr_, awuser_)),
    CulDeSac#(AXI4_W_Slave_Synth#(data_, wuser_)),
    CulDeSac#(AXI4_B_Slave_Synth#(id_, buser_)),
    CulDeSac#(AXI4_AR_Slave_Synth#(id_, addr_, aruser_)),
    CulDeSac#(AXI4_R_Slave_Synth#(id_, data_, ruser_))
  );
  function culDeSac = interface AXI4_Slave_Synth;
    interface aw = culDeSac;
    interface  w = culDeSac;
    interface  b = culDeSac;
    interface ar = culDeSac;
    interface  r = culDeSac;
  endinterface;
endinstance

interface AXI4_Slave_Xactor#(
  numeric type id_,
  numeric type addr_,
  numeric type data_,
  numeric type awuser_,
  numeric type wuser_,
  numeric type buser_,
  numeric type aruser_,
  numeric type ruser_);
  method Action clear;
  interface AXI4_Master#(id_,
                         addr_,
                         data_,
                         awuser_,
                         wuser_,
                         buser_,
                         aruser_,
                         ruser_)      master;
  interface AXI4_Slave_Synth#(id_,
                              addr_,
                              data_,
                              awuser_,
                              wuser_,
                              buser_,
                              aruser_,
                              ruser_) slaveSynth;
endinterface

////////////////////////////////
// AXI4 Shim Master <-> Slave //
////////////////////////////////////////////////////////////////////////////////

interface AXI4_Shim#(
  numeric type id_,
  numeric type addr_,
  numeric type data_,
  numeric type awuser_,
  numeric type wuser_,
  numeric type buser_,
  numeric type aruser_,
  numeric type ruser_);
  method Action clear;
  interface AXI4_Master#(
    id_, addr_, data_, awuser_, wuser_, buser_, aruser_, ruser_
  ) master;
  interface AXI4_Slave#(
    id_, addr_, data_, awuser_, wuser_, buser_, aruser_, ruser_
  ) slave;
endinterface

////////////////////////////////
// AXI4 Connectable instances //
////////////////////////////////////////////////////////////////////////////////

instance Connectable#(
  AXI4_Master#(a, b, c, d, e, f, g, h),
  AXI4_Slave#(a, b, c, d, e, f, g, h));
  module mkConnection#(
    AXI4_Master#(a, b, c, d, e, f, g, h) m,
    AXI4_Slave#(a, b, c, d, e, f, g, h) s)
    (Empty);
    mkConnection(m.aw, s.aw);
    mkConnection(m.w, s.w);
    mkConnection(m.b, s.b);
    mkConnection(m.ar, s.ar);
    mkConnection(m.r, s.r);
  endmodule
endinstance
instance Connectable#(
  AXI4_Slave#(a, b, c, d, e, f, g, h),
  AXI4_Master#(a, b, c, d, e, f, g, h));
  module mkConnection#(
    AXI4_Slave#(a, b, c, d, e, f, g, h) s,
    AXI4_Master#(a, b, c, d, e, f, g, h) m)
    (Empty);
    mkConnection(m, s);
  endmodule
endinstance

instance Connectable#(
  AXI4_Master_Synth#(a, b, c, d, e, f, g, h),
  AXI4_Slave_Synth#(a, b, c, d, e, f, g, h));
  module mkConnection#(
    AXI4_Master_Synth#(a, b, c, d, e, f, g, h) m,
    AXI4_Slave_Synth#(a, b, c, d, e, f, g, h) s)
    (Empty);
    mkConnection(m.aw, s.aw);
    mkConnection(m.w, s.w);
    mkConnection(m.b, s.b);
    mkConnection(m.ar, s.ar);
    mkConnection(m.r, s.r);
  endmodule
endinstance
instance Connectable#(
  AXI4_Slave_Synth#(a, b, c, d, e, f, g, h),
  AXI4_Master_Synth#(a, b, c, d, e, f, g, h));
  module mkConnection#(
    AXI4_Slave_Synth#(a, b, c, d, e, f, g, h) s,
    AXI4_Master_Synth#(a, b, c, d, e, f, g, h) m)
    (Empty);
    mkConnection(m, s);
  endmodule
endinstance

////////////////////////////////////
// AXI4 write channel helper type //
////////////////////////////////////////////////////////////////////////////////

typedef union tagged {
  Tuple2#(AXI4_AWFlit#(id_, addr_, awuser_),
          AXI4_WFlit#(data_, wuser_)) FirstFlit;
  AXI4_WFlit#(data_, wuser_) OtherFlit;
} AXI4_WriteFlit#(
  numeric type id_,
  numeric type addr_,
  numeric type data_,
  numeric type awuser_,
  numeric type wuser_) deriving (Bits);
instance Routable#(
  AXI4_WriteFlit#(id_, addr_, data_, awuser_, wuser_),
  AXI4_BFlit#(id_, buser_),
  Bit#(addr_)) provisos (
    Routable#(AXI4_AWFlit#(id_, addr_, awuser_),
              AXI4_BFlit#(id_, buser_),
              Bit#(addr_))
  );
  function routingField(x) = case (x) matches
    tagged FirstFlit {.aw, .w}: aw.awaddr; // XXX routingField(aw); XXX THIS SHOULD JUST WORK BUT DOESN'T ?!
    default: ?;
  endcase;
  function noRouteFound(x) = case (x) matches
    tagged FirstFlit {.aw, .w}: noRouteFound(aw);
    default: ?;
  endcase;
endinstance
instance DetectLast#(AXI4_WriteFlit#(id_, addr_, data_, awuser_, wuser_));
  function detectLast(x) = case (x) matches
    tagged FirstFlit {.aw, .w}: detectLast(w);
    tagged OtherFlit .w: detectLast(w);
  endcase;
endinstance

////////////////////////////
// ExpandReqRsp instances //
////////////////////////////////////////////////////////////////////////////////

instance ExpandReqRsp#(
  AXI4_WriteFlit#(id_, addr_, data_, awuser_, wuser_),
  AXI4_WriteFlit#(sid_, addr_, data_, awuser_, wuser_),
  AXI4_BFlit#(sid_, buser_),
  AXI4_BFlit#(id_, buser_),
  Bit#(n)) provisos (Add#(id_, n, sid_));
  function expand(r, x) = case (r) matches
    tagged FirstFlit {.aw, .w}: FirstFlit(tuple2(AXI4_AWFlit {
      awid: {x, aw.awid}, awaddr: aw.awaddr, awlen: aw.awlen, awsize: aw.awsize,
      awburst: aw.awburst, awlock: aw.awlock, awcache: aw.awcache,
      awprot: aw.awprot, awqos: aw.awqos, awregion: aw.awregion,
      awuser: aw.awuser
    }, w));
    tagged OtherFlit .f: OtherFlit(f);
  endcase;
  function shrink(r) = tuple2(AXI4_BFlit {
    bid: truncate(r.bid), bresp: r.bresp, buser: r.buser
  }, truncateLSB(r.bid));
endinstance

instance ExpandReqRsp#(
  AXI4_ARFlit#(id_, addr_, aruser_),
  AXI4_ARFlit#(sid_, addr_, aruser_),
  AXI4_RFlit#(sid_, data_, ruser_),
  AXI4_RFlit#(id_, data_, ruser_),
  Bit#(n)) provisos (Add#(id_, n, sid_));
  function expand(ar, x) = AXI4_ARFlit {
    arid: {x, ar.arid}, araddr: ar.araddr, arlen: ar.arlen, arsize: ar.arsize,
    arburst: ar.arburst, arlock: ar.arlock, arcache: ar.arcache,
    arprot: ar.arprot, arqos: ar.arqos, arregion: ar.arregion,
    aruser: ar.aruser
  };
  function shrink(r) = tuple2(AXI4_RFlit {
    rid: truncate(r.rid), rdata: r.rdata, rresp: r.rresp,
    rlast: r.rlast, ruser: r.ruser
  }, truncateLSB(r.rid));
endinstance
