/*-
 * Copyright (c) 2018-2019 Alexandre Joannou
 * Copyright (c) 2019 Peter Rugg
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
    awid: 0, awaddr: ?, awlen: 0, awsize: 1,
    awburst: INCR, awlock: NORMAL, awcache: 0,
    awprot: 0, awqos: 0, awregion: 0, awuser: ?
  };
endinstance
instance Routable#(
  AXI4_AWFlit#(id_, addr_, awuser_),
  AXI4_BFlit#(id_, buser_),
  Bit#(addr_));
  function routingField(x) = x.awaddr;
  module mkNoRouteFound(NoRouteFoundIfc#(AXI4_AWFlit#(id_, addr_, awuser_),
                                         AXI4_BFlit#(id_, buser_)));
    Reg#(AXI4_AWFlit#(id_, addr_, awuser_)) currentReq[2] <- mkCReg(2, ?);
    Reg#(Bool)                              pendingReq[2] <- mkCReg(2, False);
    method pushReq (req) if (!pendingReq[0]) = action
      currentReq[0] <= req;
      pendingReq[0] <= True;
    endaction;
    method getRsp if (pendingReq[1]) = actionvalue
      pendingReq[1] <= False;
      return tuple2(True, AXI4_BFlit {
                            bid: currentReq[1].awid, bresp: DECERR, buser: ?
                          });
    endactionvalue;
  endmodule
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

// Slave interfaces
interface AXI4_AW_Slave_Synth#(numeric type id_,
                               numeric type addr_,
                               numeric type user_);
  (* always_ready, enable="awvalid", prefix="" *)
  method Action awflit (Bit#(id_)   awid,
                        Bit#(addr_) awaddr,
                        AXI4_Len    awlen,
                        AXI4_Size   awsize,
                        AXI4_Burst  awburst,
                        AXI4_Lock   awlock,
                        AXI4_Cache  awcache,
                        AXI4_Prot   awprot,
                        AXI4_QoS    awqos,
                        AXI4_Region awregion,
                        Bit#(user_) awuser);
  (* always_ready, always_enabled *)
  method Bool awready;
endinterface

instance CulDeSac#(AXI4_AW_Slave_Synth#(id_, addr_, user_));
  function culDeSac = interface AXI4_AW_Slave_Synth;
    method awflit (a,b,c,d,e,f,g,h,i,j,k) = noAction;
    method awready = True;
  endinterface;
endinstance

// connectable instances
instance Connectable#(AXI4_AW_Master_Synth#(a, b, c),
                      AXI4_AW_Slave_Synth#(a, b, c));
  module mkConnection#(AXI4_AW_Master_Synth#(a, b, c) m,
                       AXI4_AW_Slave_Synth#(a, b, c) s) (Empty);
    rule connect_awflit (m.awvalid);
      s.awflit(m.awid,
               m.awaddr,
               m.awlen,
               m.awsize,
               m.awburst,
               m.awlock,
               m.awcache,
               m.awprot,
               m.awqos,
               m.awregion,
               m.awuser);
    endrule
    rule connect_awready; m.awready(s.awready); endrule
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

// Slave interfaces
interface AXI4_W_Slave_Synth#(numeric type data_, numeric type user_);
  (* always_ready, enable="wvalid", prefix="" *)
  method Action wflit (Bit#(data_)           wdata,
                       Bit#(TDiv#(data_, 8)) wstrb,
                       Bool                  wlast,
                       Bit#(user_)           wuser);
  (* always_ready, always_enabled *)
  method Bool wready;
endinterface

instance CulDeSac#(AXI4_W_Slave_Synth#(data_, user_));
  function culDeSac = interface AXI4_W_Slave_Synth;
    method wflit (a,b,c,d) = noAction;
    method wready = False;
  endinterface;
endinstance

// connectable instances
instance Connectable#(AXI4_W_Master_Synth#(a, b), AXI4_W_Slave_Synth#(a, b));
  module mkConnection#(AXI4_W_Master_Synth#(a, b) m,
                       AXI4_W_Slave_Synth#(a, b) s)(Empty);
    rule connect_wflit (m.wvalid); s.wflit(m.wdata, m.wstrb, m.wlast, m.wuser); endrule
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
interface AXI4_B_Master_Synth#(numeric type id_, numeric type user_);
  (* always_ready, enable="bvalid", prefix="" *)
  method Action bflit (Bit#(id_)   bid,
                       AXI4_Resp   bresp,
                       Bit#(user_) buser);
  (* always_ready, always_enabled *)
  method Bool bready;
endinterface

instance CulDeSac#(AXI4_B_Master_Synth#(id_, user_));
  function culDeSac = interface AXI4_B_Master_Synth;
    method bflit (x,y,z) = noAction;
    method bready = False;
  endinterface;
endinstance

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

// connectable instances
instance Connectable#(AXI4_B_Master_Synth#(a, b), AXI4_B_Slave_Synth#(a, b));
  module mkConnection#(AXI4_B_Master_Synth#(a, b) m,
                       AXI4_B_Slave_Synth#(a, b) s)(Empty);
    rule connect_bflit (s.bvalid); m.bflit(s.bid, s.bresp, s.buser); endrule
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
    arid: 0, araddr: ?, arlen: 0, arsize: 1,
    arburst: INCR, arlock: NORMAL, arcache: 0,
    arprot: 0, arqos: 0, arregion: 0, aruser: ?
  };
endinstance
instance Routable#(
  AXI4_ARFlit#(id_, addr_, aruser_),
  AXI4_RFlit#(id_, data_, ruser_),
  Bit#(addr_));
  function routingField(x) = x.araddr;
  module mkNoRouteFound(NoRouteFoundIfc#(AXI4_ARFlit#(id_, addr_, aruser_),
                                         AXI4_RFlit#(id_, data_, ruser_)));
    Reg#(AXI4_ARFlit#(id_, addr_, aruser_)) currentReq[2] <- mkCReg(2, ?);
    Reg#(Bit#(TAdd#(SizeOf#(AXI4_Len), 1))) flitCount[2]  <- mkCReg(2, 0);
    method pushReq (req) if (flitCount[0] == 0) = action
      currentReq[0] <= req;
      flitCount[0]  <= zeroExtend(req.arlen) + 1;
    endaction;
    method getRsp if (flitCount[1] != 0) = actionvalue
      flitCount[1] <= flitCount[1] - 1;
      let isLast = flitCount[1] == 1;
      return tuple2(isLast,
                    AXI4_RFlit{
                      rid: currentReq[1].arid, rdata: ?, rresp: DECERR,
                      rlast: isLast, ruser: ?
                    });
    endactionvalue;
  endmodule
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

// Slave interfaces
interface AXI4_AR_Slave_Synth#(numeric type id_,
                               numeric type addr_,
                               numeric type user_);
  (* always_ready, enable="arvalid", prefix="" *)
  method Action arflit (Bit#(id_)   arid,
                        Bit#(addr_) araddr,
                        AXI4_Len    arlen,
                        AXI4_Size   arsize,
                        AXI4_Burst  arburst,
                        AXI4_Lock   arlock,
                        AXI4_Cache  arcache,
                        AXI4_Prot   arprot,
                        AXI4_QoS    arqos,
                        AXI4_Region arregion,
                        Bit#(user_) aruser);
  (* always_ready, always_enabled *)
  method Bool arready;
endinterface

instance CulDeSac#(AXI4_AR_Slave_Synth#(id_, addr_, user_));
  function culDeSac = interface AXI4_AR_Slave_Synth;
    method arflit (a,b,c,d,e,f,g,h,i,j,k) = noAction;
    method arready = False;
  endinterface;
endinstance

// connectable instances
instance Connectable#(AXI4_AR_Master_Synth#(a, b, c),
                      AXI4_AR_Slave_Synth#(a, b, c));
  module mkConnection#(AXI4_AR_Master_Synth#(a, b, c) m,
                       AXI4_AR_Slave_Synth#(a, b, c) s)(Empty);
   rule connect_arflit (m.arvalid);
      s.arflit(m.arid,
               m.araddr,
               m.arlen,
               m.arsize,
               m.arburst,
               m.arlock,
               m.arcache,
               m.arprot,
               m.arqos,
               m.arregion,
               m.aruser);
    endrule
    rule connect_arready; m.arready(s.arready); endrule
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
interface AXI4_R_Master_Synth#(numeric type id_,
                               numeric type data_,
                               numeric type user_);
  (* always_ready, enable="rvalid", prefix="" *)
  method Action rflit (Bit#(id_)   rid,
                       Bit#(data_) rdata,
                       AXI4_Resp   rresp,
                       Bool        rlast,
                       Bit#(user_) ruser);
  (* always_ready, always_enabled *)
  method Bool rready;
endinterface

instance CulDeSac#(AXI4_R_Master_Synth#(id_, data_, user_));
  function culDeSac = interface AXI4_R_Master_Synth;
    method rflit (a,b,c,d,e) = noAction;
    method rready = False;
  endinterface;
endinstance

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

// connectable instances
instance Connectable#(AXI4_R_Master_Synth#(a, b, c),
                      AXI4_R_Slave_Synth#(a, b, c));
  module mkConnection#(AXI4_R_Master_Synth#(a, b, c) m,
                       AXI4_R_Slave_Synth#(a, b, c) s)(Empty);
    rule connect_rflit (s.rvalid);
      m.rflit(s.rid, s.rdata, s.rresp, s.rlast, s.ruser);
    endrule
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
  (* prefix = "" *) interface Source#(AXI4_AWFlit#(id_, addr_, awuser_)) aw;
  (* prefix = "" *) interface Source#(AXI4_WFlit#(data_, wuser_))        w;
  (* prefix = "" *) interface Sink#(AXI4_BFlit#(id_, buser_))            b;
  (* prefix = "" *) interface Source#(AXI4_ARFlit#(id_, addr_, aruser_)) ar;
  (* prefix = "" *) interface Sink#(AXI4_RFlit#(id_, data_, ruser_))     r;
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
  (* prefix = "" *) interface AXI4_AW_Master_Synth#(id_, addr_, awuser_) aw;
  (* prefix = "" *) interface AXI4_W_Master_Synth#(data_, wuser_)        w;
  (* prefix = "" *) interface AXI4_B_Master_Synth#(id_, buser_)          b;
  (* prefix = "" *) interface AXI4_AR_Master_Synth#(id_, addr_, aruser_) ar;
  (* prefix = "" *) interface AXI4_R_Master_Synth#(id_, data_, ruser_)   r;
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
                        ruser_) slave;
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
  (* prefix = "" *) interface AXI4_AW_Slave_Synth#(id_, addr_, awuser_) aw;
  (* prefix = "" *) interface AXI4_W_Slave_Synth#(data_, wuser_)        w;
  (* prefix = "" *) interface AXI4_B_Slave_Synth#(id_, buser_)          b;
  (* prefix = "" *) interface AXI4_AR_Slave_Synth#(id_, addr_, aruser_) ar;
  (* prefix = "" *) interface AXI4_R_Slave_Synth#(id_, data_, ruser_)   r;
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
                         ruser_) master;
  interface AXI4_Slave_Synth#(id_,
                              addr_,
                              data_,
                              awuser_,
                              wuser_,
                              buser_,
                              aruser_,
                              ruser_) slaveSynth;
endinterface

interface AXI4_Slave_Width_Xactor#(
  numeric type id_,
  numeric type addr_,
  numeric type mdata_,
  numeric type sdata_,
  numeric type mawuser_,
  numeric type mwuser_,
  numeric type mbuser_,
  numeric type maruser_,
  numeric type mruser_,
  numeric type sawuser_,
  numeric type swuser_,
  numeric type sbuser_,
  numeric type saruser_,
  numeric type sruser_);
  method Action clear;
  interface AXI4_Master#(id_,
                         addr_,
                         mdata_,
                         mawuser_,
                         mwuser_,
                         mbuser_,
                         maruser_,
                         mruser_
) master;
  interface AXI4_Slave_Synth#(id_,
                              addr_,
                              sdata_,
                              sawuser_,
                              swuser_,
                              sbuser_,
                              saruser_,
                              sruser_) slaveSynth;
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
  module mkNoRouteFound(NoRouteFoundIfc#(
                          AXI4_WriteFlit#(id_, addr_, data_, awuser_, wuser_),
                          AXI4_BFlit#(id_, buser_)));
    NoRouteFoundIfc#(AXI4_AWFlit#(id_, addr_, awuser_),
                     AXI4_BFlit#(id_, buser_)) inner <- mkNoRouteFound;
    method pushReq (req) = case (req) matches
      tagged FirstFlit {.aw, .w}: inner.pushReq(aw);
      default: noAction;
    endcase;
    method getRsp = inner.getRsp;
  endmodule
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
