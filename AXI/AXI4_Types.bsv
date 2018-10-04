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

import Connectable :: *;
import DefaultValue :: *;

// BlueStuff import
import Routable :: *;
// BlueBasics import
import SourceSink :: *;

import AXI4_AXI4Lite_Types :: *;

///////////////////////////////
// AXI Address Write Channel //
////////////////////////////////////////////////////////////////////////////////

// Flit type
typedef struct {
  Bit#(id_)   awid;
  Bit#(addr_) awaddr;
  Bit#(8)     awlen;
  Bit#(3)     awsize;
  AXIBurst    awburst;
  Bool        awlock;
  Bit#(4)     awcache;
  Bit#(3)     awprot;
  Bit#(4)     awqos;
  Bit#(4)     awregion;
  Bit#(user_) awuser;
} AWFlit#(numeric type id_, numeric type addr_, numeric type user_)
deriving (Bits, FShow);
instance DefaultValue#(AWFlit#(id_, addr_, user_));
  function defaultValue = AWFlit {
    awid: 0, awaddr: ?, awlen: 0, awsize: 0,
    awburst: FIXED, awlock: False, awcache: 0,
    awprot: 0, awqos: 0, awregion: 0, awuser: ?
  };
endinstance
instance Routable#(
  AWFlit#(id_, addr_, user_),
  BFlit#(id_, user_),
  Bit#(addr_));
  function routingField(x) = x.awaddr;
  function noRouteFound(x) = BFlit {
    bid: x.awid, bresp: DECERR, buser: x.awuser
  };
endinstance
instance DetectLast#(AWFlit#(id_, addr_, user_));
  function detectLast(x) = True;
endinstance

// Master interface
(* always_ready, always_enabled *)
interface AWMaster#(numeric type id_, numeric type addr_, numeric type user_);
  method Bit#(id_)   awid;
  method Bit#(addr_) awaddr;
  method Bit#(8)     awlen;
  method Bit#(3)     awsize;
  method AXIBurst    awburst;
  method Bool        awlock;
  method Bit#(4)     awcache;
  method Bit#(3)     awprot;
  method Bit#(4)     awqos;
  method Bit#(4)     awregion;
  method Bit#(user_) awuser;
  method Bool        awvalid;
  (* prefix="" *) method Action awready(Bool awready);
endinterface

// Slave interface
(* always_ready, always_enabled *)
interface AWSlave#(numeric type id_, numeric type addr_, numeric type user_);
  (* prefix="" *) method Action awid    (Bit#(id_)   awid);
  (* prefix="" *) method Action awaddr  (Bit#(addr_) awaddr);
  (* prefix="" *) method Action awlen   (Bit#(8)     awlen);
  (* prefix="" *) method Action awsize  (Bit#(3)     awsize);
  (* prefix="" *) method Action awburst (AXIBurst    awburst);
  (* prefix="" *) method Action awlock  (Bool        awlock);
  (* prefix="" *) method Action awcache (Bit#(4)     awcache);
  (* prefix="" *) method Action awprot  (Bit#(3)     awprot);
  (* prefix="" *) method Action awqos   (Bit#(4)     awqos);
  (* prefix="" *) method Action awregion(Bit#(4)     awregion);
  (* prefix="" *) method Action awuser  (Bit#(user_) awuser);
  (* prefix="" *) method Action awvalid (Bool        awvalid);
  method Bool awready;
endinterface

// connectable instances
instance Connectable#(AWMaster#(a, b, c), AWSlave#(a, b, c));
  module mkConnection#(AWMaster#(a, b, c) m, AWSlave#(a, b, c) s)(Empty);
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
instance Connectable#(AWSlave#(a, b, c), AWMaster#(a, b, c));
  module mkConnection#(AWSlave#(a, b, c) s, AWMaster#(a, b, c) m)(Empty);
    mkConnection(m, s);
  endmodule
endinstance

////////////////////////////
// AXI Write Data Channel //
////////////////////////////////////////////////////////////////////////////////

// Flit type
typedef struct {
  Bit#(data_)           wdata;
  Bit#(TDiv#(data_, 8)) wstrb;
  Bool                  wlast;
  Bit#(user_)           wuser;
} WFlit#(numeric type data_, numeric type user_) deriving (Bits, FShow);
instance DefaultValue#(WFlit#(data_, user_));
  function defaultValue = WFlit { wdata: ?, wstrb: ~0, wlast: True, wuser: ? };
endinstance
instance DetectLast#(WFlit#(data_, user_));
  function detectLast(x) = x.wlast;
endinstance

// Master interface
(* always_ready, always_enabled *)
interface WMaster#(numeric type data_, numeric type user_);
  method Bit#(data_)           wdata;
  method Bit#(TDiv#(data_, 8)) wstrb;
  method Bool                  wlast;
  method Bit#(user_)           wuser;
  method Bool                  wvalid;
  (* prefix="" *) method Action wready(Bool wready);
endinterface

// Slave interface
(* always_ready, always_enabled *)
interface WSlave#(numeric type data_, numeric type user_);
  (* prefix="" *) method Action wdata (Bit#(data_)            wdata);
  (* prefix="" *) method Action wstrb (Bit#(TDiv#(data_,  8)) wstrb);
  (* prefix="" *) method Action wlast (Bool                   wlast);
  (* prefix="" *) method Action wuser (Bit#(user_)            wuser);
  (* prefix="" *) method Action wvalid(Bool                   wvalid);
  method Bool wready;
endinterface

// connectable instances
instance Connectable#(WMaster#(a, b), WSlave#(a, b));
  module mkConnection#(WMaster#(a, b) m, WSlave#(a, b) s)(Empty);
    rule connect_wdata;  s.wdata(m.wdata);   endrule
    rule connect_wstrb;  s.wstrb(m.wstrb);   endrule
    rule connect_wlast;  s.wlast(m.wlast);   endrule
    rule connect_wuser;  s.wuser(m.wuser);   endrule
    rule connect_wvalid; s.wvalid(m.wvalid); endrule
    rule connect_wready; m.wready(s.wready); endrule
  endmodule
endinstance
instance Connectable#(WSlave#(a, b), WMaster#(a, b));
  module mkConnection#(WSlave#(a, b) s, WMaster#(a, b) m)(Empty);
    mkConnection(m, s);
  endmodule
endinstance

////////////////////////////////
// AXI Write Response Channel //
////////////////////////////////////////////////////////////////////////////////

// Flit type
typedef struct {
  Bit#(id_)   bid;
  AXIResp     bresp;
  Bit#(user_) buser;
} BFlit#(numeric type id_, numeric type user_) deriving (Bits, FShow);
instance DefaultValue#(BFlit#(id_, user_));
  function defaultValue = BFlit { bid: 0, bresp: OKAY, buser: ? };
endinstance
instance DetectLast#(BFlit#(id_, user_));
  function detectLast(x) = True;
endinstance

// Master interface
(* always_ready, always_enabled *)
interface BMaster#(numeric type id_, numeric type user_);
  (* prefix="" *) method Action bid   (Bit#(id_)   bid);
  (* prefix="" *) method Action bresp (AXIResp     bresp);
  (* prefix="" *) method Action buser (Bit#(user_) buser);
  (* prefix="" *) method Action bvalid(Bool        bvalid);
  method Bool bready;
endinterface

// Slave interface
(* always_ready, always_enabled *)
interface BSlave#(numeric type id_, numeric type user_);
  method Bit#(id_)   bid;
  method AXIResp     bresp;
  method Bit#(user_) buser;
  method Bool        bvalid;
  (* prefix="" *) method Action bready(Bool bready);
endinterface

// connectable instances
instance Connectable#(BMaster#(a, b), BSlave#(a, b));
  module mkConnection#(BMaster#(a, b) m, BSlave#(a, b) s)(Empty);
    rule connect_bid;    m.bid(s.bid);       endrule
    rule connect_bresp;  m.bresp(s.bresp);   endrule
    rule connect_buser;  m.buser(s.buser);   endrule
    rule connect_bvalid; m.bvalid(s.bvalid); endrule
    rule connect_bready; s.bready(m.bready); endrule
  endmodule
endinstance
instance Connectable#(BSlave#(a, b), BMaster#(a, b));
  module mkConnection#(BSlave#(a, b) s, BMaster#(a, b) m)(Empty);
    mkConnection(m, s);
  endmodule
endinstance

//////////////////////////////
// AXI Read Address Channel //
////////////////////////////////////////////////////////////////////////////////

// Flit type
typedef struct {
  Bit#(id_)   arid;
  Bit#(addr_) araddr;
  Bit#(8)     arlen;
  Bit#(3)     arsize;
  AXIBurst    arburst;
  Bool        arlock;
  Bit#(4)     arcache;
  Bit#(3)     arprot;
  Bit#(4)     arqos;
  Bit#(4)     arregion;
  Bit#(user_) aruser;
} ARFlit#(numeric type id_, numeric type addr_, numeric type user_)
deriving (Bits, FShow);
instance DefaultValue#(ARFlit#(id_, addr_, user_));
  function defaultValue = ARFlit {
    arid: 0, araddr: ?, arlen: 0, arsize: 0,
    arburst: FIXED, arlock: False, arcache: 0,
    arprot: 0, arqos: 0, arregion: 0, aruser: ?
  };
endinstance
instance Routable#(
  ARFlit#(id_, addr_, user_),
  RFlit#(id_, data_, user_),
  Bit#(addr_));
  function routingField(x) = x.araddr;
  function noRouteFound(x) = RFlit {
    rid: x.arid, rdata: ?, rresp: DECERR, rlast: True, ruser: x.aruser
  };
endinstance
instance DetectLast#(ARFlit#(id_, addr_, user_));
  function detectLast(x) = True;
endinstance

// Master interface
(* always_ready, always_enabled *)
interface ARMaster#(numeric type id_, numeric type addr_, numeric type user_);
  method Bit#(id_)   arid;
  method Bit#(addr_) araddr;
  method Bit#(8)     arlen;
  method Bit#(3)     arsize;
  method AXIBurst    arburst;
  method Bool        arlock;
  method Bit#(4)     arcache;
  method Bit#(3)     arprot;
  method Bit#(4)     arqos;
  method Bit#(4)     arregion;
  method Bit#(user_) aruser;
  method Bool        arvalid;
  (* prefix="" *) method Action arready(Bool arready);
endinterface

// Slave interface
(* always_ready, always_enabled *)
interface ARSlave#(numeric type id_, numeric type addr_, numeric type user_);
  (* prefix="" *) method Action arid    (Bit#(id_)   arid);
  (* prefix="" *) method Action araddr  (Bit#(addr_) araddr);
  (* prefix="" *) method Action arlen   (Bit#(8)     arlen);
  (* prefix="" *) method Action arsize  (Bit#(3)     arsize);
  (* prefix="" *) method Action arburst (AXIBurst    arburst);
  (* prefix="" *) method Action arlock  (Bool        arlock);
  (* prefix="" *) method Action arcache (Bit#(4)     arcache);
  (* prefix="" *) method Action arprot  (Bit#(3)     arprot);
  (* prefix="" *) method Action arqos   (Bit#(4)     arqos);
  (* prefix="" *) method Action arregion(Bit#(4)     arregion);
  (* prefix="" *) method Action aruser  (Bit#(user_) aruser);
  (* prefix="" *) method Action arvalid (Bool        arvalid);
  method Bool arready;
endinterface

// connectable instances
instance Connectable#(ARMaster#(a, b, c), ARSlave#(a, b, c));
  module mkConnection#(ARMaster#(a, b, c) m, ARSlave#(a, b, c) s)(Empty);
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
instance Connectable#(ARSlave#(a, b, c), ARMaster#(a, b, c));
  module mkConnection#(ARSlave#(a, b, c) s, ARMaster#(a, b, c) m)(Empty);
    mkConnection(m, s);
  endmodule
endinstance

///////////////////////////
// AXI Read Data Channel //
////////////////////////////////////////////////////////////////////////////////

// Flit type
typedef struct {
  Bit#(id_)   rid;
  Bit#(data_) rdata;
  AXIResp     rresp;
  Bool        rlast;
  Bit#(user_) ruser;
} RFlit#(numeric type id_, numeric type data_, numeric type user_)
deriving (Bits, FShow);
instance DefaultValue#(RFlit#(id_, data_, user_));
  function defaultValue = RFlit {
    rid: 0, rdata: ?, rresp: OKAY, rlast: True, ruser: ?
  };
endinstance
instance DetectLast#(RFlit#(id_, data_, user_));
  function detectLast(x) = x.rlast;
endinstance

// Master interface
(* always_ready, always_enabled *)
interface RMaster#(numeric type id_, numeric type data_, numeric type user_);
  (* prefix="" *) method Action rid   (Bit#(id_)   rid);
  (* prefix="" *) method Action rdata (Bit#(data_) rdata);
  (* prefix="" *) method Action rresp (AXIResp     rresp);
  (* prefix="" *) method Action rlast (Bool        rlast);
  (* prefix="" *) method Action ruser (Bit#(user_) ruser);
  (* prefix="" *) method Action rvalid(Bool        rvalid);
  method Bool rready;
endinterface

// Slave interface
(* always_ready, always_enabled *)
interface RSlave#(numeric type id_, numeric type data_, numeric type user_);
  method Bit#(id_)   rid;
  method Bit#(data_) rdata;
  method AXIResp     rresp;
  method Bool        rlast;
  method Bit#(user_) ruser;
  method Bool        rvalid;
  (* prefix="" *) method Action rready(Bool rready);
endinterface

// connectable instances
instance Connectable#(RMaster#(a, b, c), RSlave#(a, b, c));
  module mkConnection#(RMaster#(a, b, c) m, RSlave#(a, b, c) s)(Empty);
    rule connect_rid;    m.rid(s.rid);       endrule
    rule connect_rdata;  m.rdata(s.rdata);   endrule
    rule connect_rresp;  m.rresp(s.rresp);   endrule
    rule connect_rlast;  m.rlast(s.rlast);   endrule
    rule connect_ruser;  m.ruser(s.ruser);   endrule
    rule connect_rvalid; m.rvalid(s.rvalid); endrule
    rule connect_rready; s.rready(m.rready); endrule
  endmodule
endinstance
instance Connectable#(RSlave#(a, b, c), RMaster#(a, b, c));
  module mkConnection#(RSlave#(a, b, c) s, RMaster#(a, b, c) m)(Empty);
    mkConnection(m, s);
  endmodule
endinstance

////////////////
// AXI Master //
////////////////////////////////////////////////////////////////////////////////

interface AXIMaster#(
  numeric type id_,
  numeric type addr_,
  numeric type data_,
  numeric type user_);
  interface Source#(AWFlit#(id_, addr_, user_)) aw;
  interface Source#(WFlit#(data_, user_))       w;
  interface Sink#(BFlit#(id_, user_))           b;
  interface Source#(ARFlit#(id_, addr_, user_)) ar;
  interface Sink#(RFlit#(id_, data_, user_))    r;
endinterface

interface AXIMasterSynth#(
  numeric type id_,
  numeric type addr_,
  numeric type data_,
  numeric type user_);
  interface AWMaster#(id_, addr_, user_) aw;
  interface WMaster#(data_, user_)       w;
  interface BMaster#(id_, user_)         b;
  interface ARMaster#(id_, addr_, user_) ar;
  interface RMaster#(id_, data_, user_)  r;
endinterface

///////////////
// AXI Slave //
////////////////////////////////////////////////////////////////////////////////

interface AXISlave#(
  numeric type id_,
  numeric type addr_,
  numeric type data_,
  numeric type user_);
  interface Sink#(AWFlit#(id_, addr_, user_))  aw;
  interface Sink#(WFlit#(data_, user_))        w;
  interface Source#(BFlit#(id_, user_))        b;
  interface Sink#(ARFlit#(id_, addr_, user_))  ar;
  interface Source#(RFlit#(id_, data_, user_)) r;
endinterface

interface AXISlaveSynth#(
  numeric type id_,
  numeric type addr_,
  numeric type data_,
  numeric type user_);
  interface AWSlave#(id_, addr_, user_) aw;
  interface WSlave#(data_, user_)       w;
  interface BSlave#(id_, user_)         b;
  interface ARSlave#(id_, addr_, user_) ar;
  interface RSlave#(id_, data_, user_)  r;
endinterface

///////////////////////////////
// AXI Shim Master <-> Slave //
////////////////////////////////////////////////////////////////////////////////

interface AXIShim#(
  numeric type id_,
  numeric type addr_,
  numeric type data_,
  numeric type user_);
  interface AXIMaster#(id_, addr_, data_, user_) master;
  interface AXISlave#(id_, addr_, data_, user_) slave;
endinterface

///////////////////////////////
// AXI Connectable instances //
////////////////////////////////////////////////////////////////////////////////

instance Connectable#(AXIMaster#(a, b, c, d), AXISlave#(a, b, c, d));
  module mkConnection#(AXIMaster#(a, b, c, d) m, AXISlave#(a, b, c, d) s)
  (Empty);
    mkConnection(m.aw, s.aw);
    mkConnection(m.w, s.w);
    mkConnection(m.b, s.b);
    mkConnection(m.ar, s.ar);
    mkConnection(m.r, s.r);
  endmodule
endinstance
instance Connectable#(AXISlave#(a, b, c, d), AXIMaster#(a, b, c, d));
  module mkConnection#(AXISlave#(a, b, c, d) s, AXIMaster#(a, b, c, d) m)
  (Empty);
    mkConnection(m, s);
  endmodule
endinstance

///////////////////////////////////
// AXI write channel helper type //
////////////////////////////////////////////////////////////////////////////////

typedef union tagged {
  Tuple2#(AWFlit#(id_, addr_, user_), WFlit#(data_, user_)) FirstFlit;
  WFlit#(data_, user_) OtherFlit;
} AXIWriteFlit#(
  numeric type id_,
  numeric type addr_,
  numeric type data_,
  numeric type user_) deriving (Bits);
instance Routable#(AXIWriteFlit#(id_, addr_, data_, user_),
                   BFlit#(id_, user_),
                   Bit#(addr_));
  function routingField(x) = case (x) matches
    tagged FirstFlit {.aw, .w}: routingField(aw);
    default: ?;
  endcase;
  function noRouteFound(x) = case (x) matches
    tagged FirstFlit {.aw, .w}: noRouteFound(aw);
    default: ?;
  endcase;
endinstance
instance DetectLast#(AXIWriteFlit#(id_, addr_, data_, user_));
  function detectLast(x) = case (x) matches
    tagged FirstFlit {.aw, .w}: detectLast(w);
    tagged OtherFlit .w: detectLast(w);
  endcase;
endinstance

////////////////////////////
// ExpandReqRsp instances //
////////////////////////////////////////////////////////////////////////////////

instance ExpandReqRsp#(
  AXIWriteFlit#(id_, addr_, data_, user_),
  AXIWriteFlit#(sid_, addr_, data_, user_),
  BFlit#(sid_, user_),
  BFlit#(id_, user_),
  Bit#(n)) provisos (Add#(id_, n, sid_));
  function expand(r, x) = case (r) matches
    tagged FirstFlit {.aw, .w}: FirstFlit(tuple2(AWFlit{
      awid: {x, aw.awid}, awaddr: aw.awaddr, awlen: aw.awlen, awsize: aw.awsize,
      awburst: aw.awburst, awlock: aw.awlock, awcache: aw.awcache,
      awprot: aw.awprot, awqos: aw.awqos, awregion: aw.awregion,
      awuser: aw.awuser
    }, w));
    tagged OtherFlit .f: OtherFlit(f);
  endcase;
  function shrink(r) = tuple2(BFlit{
    bid: truncate(r.bid), bresp: r.bresp, buser: r.buser
  }, truncateLSB(r.bid));
endinstance

instance ExpandReqRsp#(
  ARFlit#(id_, addr_, user_),
  ARFlit#(sid_, addr_, user_),
  RFlit#(sid_, data_, user_),
  RFlit#(id_, data_, user_),
  Bit#(n)) provisos (Add#(id_, n, sid_));
  function expand(ar, x) = ARFlit{
    arid: {x, ar.arid}, araddr: ar.araddr, arlen: ar.arlen, arsize: ar.arsize,
    arburst: ar.arburst, arlock: ar.arlock, arcache: ar.arcache,
    arprot: ar.arprot, arqos: ar.arqos, arregion: ar.arregion,
    aruser: ar.aruser
  };
  function shrink(r) = tuple2(RFlit{
    rid: truncate(r.rid), rdata: r.rdata, rresp: r.rresp,
    rlast: r.rlast, ruser: r.ruser
  }, truncateLSB(r.rid));
endinstance
