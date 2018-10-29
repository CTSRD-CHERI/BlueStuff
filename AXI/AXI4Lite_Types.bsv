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
  Bit#(addr_) awaddr;
  Bit#(3)     awprot;
  Bit#(user_) awuser;
} AWLiteFlit#(numeric type addr_, numeric type user_) deriving (Bits, FShow);
instance DefaultValue#(AWLiteFlit#(addr_, user_));
  function defaultValue = AWLiteFlit { awaddr: ?, awprot: 0, awuser: ? };
endinstance
instance Routable#(AWLiteFlit#(addr_, user_), BLiteFlit#(user_), Bit#(addr_));
  function routingField(x) = x.awaddr;
  function noRouteFound(x) = BLiteFlit { bresp: DECERR, buser: ? };
endinstance
instance DetectLast#(AWLiteFlit#(addr_, user_));
  function detectLast(x) = True;
endinstance

// Master interface
(* always_ready, always_enabled *)
interface AWLiteMaster#(numeric type addr_, numeric type user_);
  method Bit#(addr_) awaddr;
  method Bit#(3)     awprot;
  method Bit#(user_) awuser;
  method Bool        awvalid;
  (* prefix="" *) method Action awready(Bool awready);
endinterface

// Slave interface
(* always_ready, always_enabled *)
interface AWLiteSlave#(numeric type addr_, numeric type user_);
  (* prefix="" *) method Action awaddr (Bit#(addr_)  awaddr);
  (* prefix="" *) method Action awprot (Bit#(3)      awprot);
  (* prefix="" *) method Action awuser  (Bit#(user_) awuser);
  (* prefix="" *) method Action awvalid(Bool         awvalid);
  method Bool awready;
endinterface

// connectable instances
instance Connectable#(AWLiteMaster#(a, b), AWLiteSlave#(a, b));
  module mkConnection#(AWLiteMaster#(a,b ) m, AWLiteSlave#(a, b) s)(Empty);
    rule connect_awaddr;  s.awaddr(m.awaddr);   endrule
    rule connect_awprot;  s.awprot(m.awprot);   endrule
    rule connect_awuser;  s.awuser(m.awuser);   endrule
    rule connect_awvalid; s.awvalid(m.awvalid); endrule
    rule connect_awready; m.awready(s.awready); endrule
  endmodule
endinstance
instance Connectable#(AWLiteSlave#(a, b), AWLiteMaster#(a, b));
  module mkConnection#(AWLiteSlave#(a, b) s, AWLiteMaster#(a, b) m)(Empty);
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
  Bit#(user_)           wuser;
} WLiteFlit#(numeric type data_, numeric type user_) deriving (Bits, FShow);
instance DefaultValue#(WLiteFlit#(data_, user_));
  function defaultValue = WLiteFlit { wdata: ?, wstrb: ~0, wuser: ? };
endinstance
instance DetectLast#(WLiteFlit#(data_, user_));
  function detectLast(x) = True;
endinstance

// Master interface
(* always_ready, always_enabled *)
interface WLiteMaster#(numeric type data_, numeric type user_);
  method Bit#(data_)           wdata;
  method Bit#(TDiv#(data_, 8)) wstrb;
  method Bit#(user_)           wuser;
  method Bool                  wvalid;
  (* prefix="" *) method Action wready(Bool wready);
endinterface

// Slave interface
(* always_ready, always_enabled *)
interface WLiteSlave#(numeric type data_, numeric type user_);
  (* prefix="" *) method Action wdata (Bit#(data_)            wdata);
  (* prefix="" *) method Action wstrb (Bit#(TDiv#(data_,  8)) wstrb);
  (* prefix="" *) method Action wuser (Bit#(user_)            wuser);
  (* prefix="" *) method Action wvalid(Bool                   wvalid);
  method Bool wready;
endinterface

// connectable instances
instance Connectable#(WLiteMaster#(a, b), WLiteSlave#(a, b));
  module mkConnection#(WLiteMaster#(a, b) m, WLiteSlave#(a, b) s)(Empty);
    rule connect_wdata;  s.wdata(m.wdata);   endrule
    rule connect_wstrb;  s.wstrb(m.wstrb);   endrule
    rule connect_wuser;  s.wuser(m.wuser);   endrule
    rule connect_wvalid; s.wvalid(m.wvalid); endrule
    rule connect_wready; m.wready(s.wready); endrule
  endmodule
endinstance
instance Connectable#(WLiteSlave#(a, b), WLiteMaster#(a, b));
  module mkConnection#(WLiteSlave#(a, b) s, WLiteMaster#(a, b) m)(Empty);
    mkConnection(m, s);
  endmodule
endinstance

////////////////////////////////
// AXI Write Response Channel //
////////////////////////////////////////////////////////////////////////////////

// Flit type
typedef struct {
  AXIResp     bresp;
  Bit#(user_) buser;
} BLiteFlit#(numeric type user_) deriving (Bits, FShow);
instance DefaultValue#(BLiteFlit#(user_));
  function defaultValue = BLiteFlit { bresp: OKAY, buser: ? };
endinstance
instance DetectLast#(BLiteFlit#(user_));
  function detectLast(x) = True;
endinstance

// Master interface
(* always_ready, always_enabled *)
interface BLiteMaster#(numeric type user_);
  (* prefix="" *) method Action bresp (AXIResp     bresp);
  (* prefix="" *) method Action buser (Bit#(user_) buser);
  (* prefix="" *) method Action bvalid(Bool        bvalid);
  method Bool bready;
endinterface

// Slave interface
(* always_ready, always_enabled *)
interface BLiteSlave#(numeric type user_);
  method AXIResp     bresp;
  method Bit#(user_) buser;
  method Bool        bvalid;
  (* prefix="" *) method Action bready(Bool bready);
endinterface

// connectable instances
instance Connectable#(BLiteMaster#(a), BLiteSlave#(a));
  module mkConnection#(BLiteMaster#(a) m, BLiteSlave#(a) s)(Empty);
    rule connect_bresp;  m.bresp(s.bresp);   endrule
    rule connect_buser;  m.buser(s.buser);   endrule
    rule connect_bvalid; m.bvalid(s.bvalid); endrule
    rule connect_bready; s.bready(m.bready); endrule
  endmodule
endinstance
instance Connectable#(BLiteSlave#(a), BLiteMaster#(a));
  module mkConnection#(BLiteSlave#(a) s, BLiteMaster#(a) m)(Empty);
    mkConnection(m, s);
  endmodule
endinstance

//////////////////////////////
// AXI Read Address Channel //
////////////////////////////////////////////////////////////////////////////////

// Flit type
typedef struct {
  Bit#(addr_) araddr;
  Bit#(3)     arprot;
  Bit#(user_) aruser;
} ARLiteFlit#(numeric type addr_, numeric type user_) deriving (Bits, FShow);
instance DefaultValue#(ARLiteFlit#(addr_, user_));
  function defaultValue = ARLiteFlit { araddr: ?, arprot: 0, aruser: ? };
endinstance
instance Routable#(
  ARLiteFlit#(addr_, user_),
  RLiteFlit#(data_, user_),
  Bit#(addr_));
  function routingField(x) = x.araddr;
  function noRouteFound(x) = RLiteFlit {
    rdata: ?, rresp: DECERR, ruser: ?
  };
endinstance
instance DetectLast#(ARLiteFlit#(addr_, user_));
  function detectLast(x) = True;
endinstance

// Master interface
(* always_ready, always_enabled *)
interface ARLiteMaster#(numeric type addr_, numeric type user_);
  method Bit#(addr_) araddr;
  method Bit#(3)     arprot;
  method Bit#(user_) aruser;
  method Bool        arvalid;
  (* prefix="" *) method Action arready(Bool arready);
endinterface

// Slave interface
(* always_ready, always_enabled *)
interface ARLiteSlave#(numeric type addr_, numeric type user_);
  (* prefix="" *) method Action araddr (Bit#(addr_) araddr);
  (* prefix="" *) method Action arprot (Bit#(3)     arprot);
  (* prefix="" *) method Action aruser (Bit#(user_) aruser);
  (* prefix="" *) method Action arvalid(Bool        arvalid);
  method Bool arready;
endinterface

// connectable instances
instance Connectable#(ARLiteMaster#(a, b), ARLiteSlave#(a, b));
  module mkConnection#(ARLiteMaster#(a, b) m, ARLiteSlave#(a, b) s)(Empty);
    rule connect_araddr;  s.araddr(m.araddr);   endrule
    rule connect_arprot;  s.arprot(m.arprot);   endrule
    rule connect_aruser;  s.aruser(m.aruser);   endrule
    rule connect_arvalid; s.arvalid(m.arvalid); endrule
    rule connect_arready; m.arready(s.arready); endrule
  endmodule
endinstance
instance Connectable#(ARLiteSlave#(a, b), ARLiteMaster#(a, b));
  module mkConnection#(ARLiteSlave#(a, b) s, ARLiteMaster#(a, b) m)(Empty);
    mkConnection(m, s);
  endmodule
endinstance

///////////////////////////
// AXI Read Data Channel //
////////////////////////////////////////////////////////////////////////////////

// Flit type
typedef struct {
  Bit#(data_) rdata;
  AXIResp     rresp;
  Bit#(user_) ruser;
} RLiteFlit#(numeric type data_, numeric type user_) deriving (Bits, FShow);
instance DefaultValue#(RLiteFlit#(data_, user_));
  function defaultValue = RLiteFlit { rdata: ?, rresp: OKAY, ruser: ? };
endinstance
instance DetectLast#(RLiteFlit#(data_, user_));
  function detectLast(x) = True;
endinstance

// Master interface
(* always_ready, always_enabled *)
interface RLiteMaster#(numeric type data_, numeric type user_);
  (* prefix="" *) method Action rdata (Bit#(data_) rdata);
  (* prefix="" *) method Action rresp (AXIResp     rresp);
  (* prefix="" *) method Action ruser (Bit#(user_) ruser);
  (* prefix="" *) method Action rvalid(Bool        rvalid);
  method Bool rready;
endinterface

// Slave interface
(* always_ready, always_enabled *)
interface RLiteSlave#(numeric type data_, numeric type user_);
  method Bit#(data_) rdata;
  method AXIResp     rresp;
  method Bit#(user_) ruser;
  method Bool        rvalid;
  (* prefix="" *) method Action rready(Bool rready);
endinterface

// connectable instances
instance Connectable#(RLiteMaster#(a, b), RLiteSlave#(a, b));
  module mkConnection#(RLiteMaster#(a, b) m, RLiteSlave#(a, b) s)(Empty);
    rule connect_rdata;  m.rdata(s.rdata);   endrule
    rule connect_rresp;  m.rresp(s.rresp);   endrule
    rule connect_ruser;  m.ruser(s.ruser);   endrule
    rule connect_rvalid; m.rvalid(s.rvalid); endrule
    rule connect_rready; s.rready(m.rready); endrule
  endmodule
endinstance
instance Connectable#(RLiteSlave#(a, b), RLiteMaster#(a, b));
  module mkConnection#(RLiteSlave#(a, b) s, RLiteMaster#(a, b) m)(Empty);
    mkConnection(m, s);
  endmodule
endinstance

////////////////
// AXI Master //
////////////////////////////////////////////////////////////////////////////////

interface AXILiteMaster#(
  numeric type addr_,
  numeric type data_,
  numeric type awuser_,
  numeric type wuser_,
  numeric type buser_,
  numeric type aruser_,
  numeric type ruser_);
  interface Source#(AWLiteFlit#(addr_, awuser_)) aw;
  interface Source#(WLiteFlit#(data_, wuser_))   w;
  interface Sink#(BLiteFlit#(buser_))            b;
  interface Source#(ARLiteFlit#(addr_, aruser_)) ar;
  interface Sink#(RLiteFlit#(data_, ruser_))     r;
endinterface

interface AXILiteMasterSynth#(
  numeric type addr_,
  numeric type data_,
  numeric type awuser_,
  numeric type wuser_,
  numeric type buser_,
  numeric type aruser_,
  numeric type ruser_);
  interface AWLiteMaster#(addr_, awuser_) aw;
  interface WLiteMaster#(data_, wuser_)   w;
  interface BLiteMaster#(buser_)          b;
  interface ARLiteMaster#(addr_, aruser_) ar;
  interface RLiteMaster#(data_, ruser_)   r;
endinterface

///////////////
// AXI Slave //
////////////////////////////////////////////////////////////////////////////////

interface AXILiteSlave#(
  numeric type addr_,
  numeric type data_,
  numeric type awuser_,
  numeric type wuser_,
  numeric type buser_,
  numeric type aruser_,
  numeric type ruser_);
  interface Sink#(AWLiteFlit#(addr_, awuser_)) aw;
  interface Sink#(WLiteFlit#(data_, wuser_))   w;
  interface Source#(BLiteFlit#(buser_))        b;
  interface Sink#(ARLiteFlit#(addr_, aruser_)) ar;
  interface Source#(RLiteFlit#(data_, ruser_)) r;
endinterface

interface AXILiteSlaveSynth#(
  numeric type addr_,
  numeric type data_,
  numeric type awuser_,
  numeric type wuser_,
  numeric type buser_,
  numeric type aruser_,
  numeric type ruser_);
  interface AWLiteSlave#(addr_, awuser_) aw;
  interface WLiteSlave#(data_, wuser_)   w;
  interface BLiteSlave#(buser_)          b;
  interface ARLiteSlave#(addr_, aruser_) ar;
  interface RLiteSlave#(data_, ruser_)   r;
endinterface

///////////////////////////////
// AXI Shim Master <-> Slave //
////////////////////////////////////////////////////////////////////////////////

interface AXILiteShim#(
  numeric type addr_,
  numeric type data_,
  numeric type awuser_,
  numeric type wuser_,
  numeric type buser_,
  numeric type aruser_,
  numeric type ruser_);
  interface AXILiteMaster#(
    addr_, data_, awuser_, wuser_, buser_, aruser_, ruser_
  ) master;
  interface AXILiteSlave#(
    addr_, data_, awuser_, wuser_, buser_, aruser_, ruser_
  ) slave;
endinterface

///////////////////////////////
// AXI Connectable instances //
////////////////////////////////////////////////////////////////////////////////

instance Connectable#(
  AXILiteMaster#(a, b, c, d, e, f, g),
  AXILiteSlave#(a, b, c, d, e, f, g));
  module mkConnection#(
    AXILiteMaster#(a, b, c, d, e, f, g) m,
    AXILiteSlave#(a, b, c, d, e, f, g) s)
    (Empty);
    mkConnection(m.aw, s.aw);
    mkConnection(m.w, s.w);
    mkConnection(m.b, s.b);
    mkConnection(m.ar, s.ar);
    mkConnection(m.r, s.r);
  endmodule
endinstance
instance Connectable#(
  AXILiteSlave#(a, b, c, d, e, f, g),
  AXILiteMaster#(a, b, c, d, e, f, g));
  module mkConnection#(
    AXILiteSlave#(a, b, c, d, e, f, g) s,
    AXILiteMaster#(a, b, c, d, e, f, g) m)
    (Empty);
    mkConnection(m, s);
  endmodule
endinstance

///////////////////////
// AXI write channel //
////////////////////////////////////////////////////////////////////////////////

typedef struct {
  AWLiteFlit#(addr_, awuser_) aw;
  WLiteFlit#(data_, wuser_)  w;
} AXILiteWriteFlit#(
  numeric type addr_,
  numeric type data_,
  numeric type awuser_,
  numeric type wuser_) deriving (Bits, FShow);
instance Routable#(
  AXILiteWriteFlit#(addr_, data_, awuser_, wuser_),
  BLiteFlit#(buser_),
  Bit#(addr_)) provisos (
    Routable#(AWLiteFlit#(addr_, awuser_), BLiteFlit#(buser_), Bit#(addr_))
  );
  function routingField(x) = routingField(x.aw);
  function noRouteFound(x) = noRouteFound(x.aw);
endinstance
instance DetectLast#(AXILiteWriteFlit#(addr_, data_, awuser_, wuser_));
  function detectLast(x) = detectLast(x.w);
endinstance
