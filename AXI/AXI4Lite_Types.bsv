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
} AWLiteFlit#(numeric type addr_) deriving (Bits, FShow);
instance DefaultValue#(AWLiteFlit#(addr_));
  function defaultValue = AWLiteFlit { awaddr: ?, awprot: 0};
endinstance
instance Routable#(AWLiteFlit#(addr_), BLiteFlit, Bit#(addr_));
  function routingField(x) = x.awaddr;
  function noRouteFound(x) = BLiteFlit { bresp: DECERR };
endinstance
instance DetectLast#(AWLiteFlit#(addr_));
  function detectLast(x) = True;
endinstance

// Master interface
(* always_ready, always_enabled *)
interface AWLiteMaster#(numeric type addr_);
  method Bit#(addr_) awaddr;
  method Bit#(3)     awprot;
  method Bool        awvalid;
  (* prefix="" *) method Action awready(Bool awready);
endinterface

// Slave interface
(* always_ready, always_enabled *)
interface AWLiteSlave#(numeric type addr_);
  (* prefix="" *) method Action awaddr (Bit#(addr_) awaddr);
  (* prefix="" *) method Action awprot (Bit#(3)     awprot);
  (* prefix="" *) method Action awvalid(Bool        awvalid);
  method Bool awready;
endinterface

// connectable instances
instance Connectable#(AWLiteMaster#(a), AWLiteSlave#(a));
  module mkConnection#(AWLiteMaster#(a) m, AWLiteSlave#(a) s)(Empty);
    rule connect_awaddr;  s.awaddr(m.awaddr);   endrule
    rule connect_awprot;  s.awprot(m.awprot);   endrule
    rule connect_awvalid; s.awvalid(m.awvalid); endrule
    rule connect_awready; m.awready(s.awready); endrule
  endmodule
endinstance
instance Connectable#(AWLiteSlave#(a), AWLiteMaster#(a));
  module mkConnection#(AWLiteSlave#(a) s, AWLiteMaster#(a) m)(Empty);
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
} WLiteFlit#(numeric type data_) deriving (Bits, FShow);
instance DefaultValue#(WLiteFlit#(data_));
  function defaultValue = WLiteFlit { wdata: ?, wstrb: ~0};
endinstance
instance DetectLast#(WLiteFlit#(data_));
  function detectLast(x) = True;
endinstance

// Master interface
(* always_ready, always_enabled *)
interface WLiteMaster#(numeric type data_);
  method Bit#(data_)           wdata;
  method Bit#(TDiv#(data_, 8)) wstrb;
  method Bool                  wvalid;
  (* prefix="" *) method Action wready(Bool wready);
endinterface

// Slave interface
(* always_ready, always_enabled *)
interface WLiteSlave#(numeric type data_);
  (* prefix="" *) method Action wdata (Bit#(data_)            wdata);
  (* prefix="" *) method Action wstrb (Bit#(TDiv#(data_,  8)) wstrb);
  (* prefix="" *) method Action wvalid(Bool                   wvalid);
  method Bool wready;
endinterface

// connectable instances
instance Connectable#(WLiteMaster#(a), WLiteSlave#(a));
  module mkConnection#(WLiteMaster#(a) m, WLiteSlave#(a) s)(Empty);
    rule connect_wdata;  s.wdata(m.wdata);   endrule
    rule connect_wstrb;  s.wstrb(m.wstrb);   endrule
    rule connect_wvalid; s.wvalid(m.wvalid); endrule
    rule connect_wready; m.wready(s.wready); endrule
  endmodule
endinstance
instance Connectable#(WLiteSlave#(a), WLiteMaster#(a));
  module mkConnection#(WLiteSlave#(a) s, WLiteMaster#(a) m)(Empty);
    mkConnection(m, s);
  endmodule
endinstance

////////////////////////////////
// AXI Write Response Channel //
////////////////////////////////////////////////////////////////////////////////

// Flit type
typedef struct {
  AXIResp bresp;
} BLiteFlit deriving (Bits, FShow);
instance DefaultValue#(BLiteFlit);
  function defaultValue = BLiteFlit { bresp: OKAY };
endinstance
instance DetectLast#(BLiteFlit);
  function detectLast(x) = True;
endinstance

// Master interface
(* always_ready, always_enabled *)
interface BLiteMaster;
  (* prefix="" *) method Action bresp (AXIResp bresp);
  (* prefix="" *) method Action bvalid(Bool    bvalid);
  method Bool bready;
endinterface

// Slave interface
(* always_ready, always_enabled *)
interface BLiteSlave;
  method AXIResp bresp;
  method Bool bvalid;
  (* prefix="" *) method Action bready(Bool bready);
endinterface

// connectable instances
instance Connectable#(BLiteMaster, BLiteSlave);
  module mkConnection#(BLiteMaster m, BLiteSlave s)(Empty);
    rule connect_bresp;  m.bresp(s.bresp);   endrule
    rule connect_bvalid; m.bvalid(s.bvalid); endrule
    rule connect_bready; s.bready(m.bready); endrule
  endmodule
endinstance
instance Connectable#(BLiteSlave, BLiteMaster);
  module mkConnection#(BLiteSlave s, BLiteMaster m)(Empty);
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
} ARLiteFlit#(numeric type addr_) deriving (Bits, FShow);
instance DefaultValue#(ARLiteFlit#(addr_));
  function defaultValue = ARLiteFlit { araddr: ?, arprot: 0};
endinstance
instance Routable#(ARLiteFlit#(addr_), RLiteFlit#(data_), Bit#(addr_));
  function routingField(x) = x.araddr;
  function noRouteFound(x) = RLiteFlit { rdata: ?, rresp: DECERR };
endinstance
instance DetectLast#(ARLiteFlit#(addr_));
  function detectLast(x) = True;
endinstance

// Master interface
(* always_ready, always_enabled *)
interface ARLiteMaster#(numeric type addr_);
  method Bit#(addr_) araddr;
  method Bit#(3)     arprot;
  method Bool        arvalid;
  (* prefix="" *) method Action arready(Bool arready);
endinterface

// Slave interface
(* always_ready, always_enabled *)
interface ARLiteSlave#(numeric type addr_);
  (* prefix="" *) method Action araddr (Bit#(addr_) araddr);
  (* prefix="" *) method Action arprot (Bit#(3)     arprot);
  (* prefix="" *) method Action arvalid(Bool        arvalid);
  method Bool arready;
endinterface

// connectable instances
instance Connectable#(ARLiteMaster#(a), ARLiteSlave#(a));
  module mkConnection#(ARLiteMaster#(a) m, ARLiteSlave#(a) s)(Empty);
    rule connect_araddr;  s.araddr(m.araddr);   endrule
    rule connect_arprot;  s.arprot(m.arprot);   endrule
    rule connect_arvalid; s.arvalid(m.arvalid); endrule
    rule connect_arready; m.arready(s.arready); endrule
  endmodule
endinstance
instance Connectable#(ARLiteSlave#(a), ARLiteMaster#(a));
  module mkConnection#(ARLiteSlave#(a) s, ARLiteMaster#(a) m)(Empty);
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
} RLiteFlit#(numeric type data_) deriving (Bits, FShow);
instance DefaultValue#(RLiteFlit#(data_));
  function defaultValue = RLiteFlit { rdata: ?, rresp: OKAY };
endinstance
instance DetectLast#(RLiteFlit#(data_));
  function detectLast(x) = True;
endinstance

// Master interface
(* always_ready, always_enabled *)
interface RLiteMaster#(numeric type data_);
  (* prefix="" *) method Action rdata (Bit#(data_) rdata);
  (* prefix="" *) method Action rresp (AXIResp     rresp);
  (* prefix="" *) method Action rvalid(Bool        rvalid);
  method Bool rready;
endinterface

// Slave interface
(* always_ready, always_enabled *)
interface RLiteSlave#(numeric type data_);
  method Bit#(data_) rdata;
  method AXIResp     rresp;
  method Bool        rvalid;
  (* prefix="" *) method Action rready(Bool rready);
endinterface

// connectable instances
instance Connectable#(RLiteMaster#(a), RLiteSlave#(a));
  module mkConnection#(RLiteMaster#(a) m, RLiteSlave#(a) s)(Empty);
    rule connect_rdata;  m.rdata(s.rdata);   endrule
    rule connect_rresp;  m.rresp(s.rresp);   endrule
    rule connect_rvalid; m.rvalid(s.rvalid); endrule
    rule connect_rready; s.rready(m.rready); endrule
  endmodule
endinstance
instance Connectable#(RLiteSlave#(a), RLiteMaster#(a));
  module mkConnection#(RLiteSlave#(a) s, RLiteMaster#(a) m)(Empty);
    mkConnection(m, s);
  endmodule
endinstance

////////////////
// AXI Master //
////////////////////////////////////////////////////////////////////////////////

interface AXILiteMaster#(numeric type addr_, numeric type data_);
  interface Source#(AWLiteFlit#(addr_)) aw;
  interface Source#(WLiteFlit#(data_))  w;
  interface Sink#(BLiteFlit)            b;
  interface Source#(ARLiteFlit#(addr_)) ar;
  interface Sink#(RLiteFlit#(data_))    r;
endinterface

interface AXILiteMasterSynth#(numeric type addr_, numeric type data_);
  interface AWLiteMaster#(addr_) aw;
  interface WLiteMaster#(data_)  w;
  interface BLiteMaster          b;
  interface ARLiteMaster#(addr_) ar;
  interface RLiteMaster#(data_)  r;
endinterface

///////////////
// AXI Slave //
////////////////////////////////////////////////////////////////////////////////

interface AXILiteSlave#(numeric type addr_, numeric type data_);
  interface Sink#(AWLiteFlit#(addr_))  aw;
  interface Sink#(WLiteFlit#(data_))   w;
  interface Source#(BLiteFlit)         b;
  interface Sink#(ARLiteFlit#(addr_))  ar;
  interface Source#(RLiteFlit#(data_)) r;
endinterface

interface AXILiteSlaveSynth#(numeric type addr_, numeric type data_);
  interface AWLiteSlave#(addr_) aw;
  interface WLiteSlave#(data_)  w;
  interface BLiteSlave          b;
  interface ARLiteSlave#(addr_) ar;
  interface RLiteSlave#(data_)  r;
endinterface

///////////////////////////////
// AXI Shim Master <-> Slave //
////////////////////////////////////////////////////////////////////////////////

interface AXILiteShim#(
  numeric type addr_,
  numeric type data_);
  interface AXILiteMaster#(addr_, data_) master;
  interface AXILiteSlave#(addr_, data_) slave;
endinterface

///////////////////////////////
// AXI Connectable instances //
////////////////////////////////////////////////////////////////////////////////

instance Connectable#(AXILiteMaster#(a, b), AXILiteSlave#(a, b));
  module mkConnection#(AXILiteMaster#(a, b) m, AXILiteSlave#(a, b) s)
  (Empty);
    mkConnection(m.aw, s.aw);
    mkConnection(m.w, s.w);
    mkConnection(m.b, s.b);
    mkConnection(m.ar, s.ar);
    mkConnection(m.r, s.r);
  endmodule
endinstance
instance Connectable#(AXILiteSlave#(a, b), AXILiteMaster#(a, b));
  module mkConnection#(AXILiteSlave#(a, b) s, AXILiteMaster#(a, b) m)
  (Empty);
    mkConnection(m, s);
  endmodule
endinstance
