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

////////////////////////////////////
// AXI4Lite Address Write Channel //
////////////////////////////////////////////////////////////////////////////////

// Flit type
typedef struct {
  Bit#(addr_) awaddr;
  AXI4_Prot   awprot;
  Bit#(user_) awuser;
} AXI4Lite_AWFlit#(numeric type addr_,
                   numeric type user_) deriving (Bits, FShow);
instance DefaultValue#(AXI4Lite_AWFlit#(addr_, user_));
  function defaultValue = AXI4Lite_AWFlit { awaddr: ?, awprot: 0, awuser: ? };
endinstance
instance Routable#(AXI4Lite_AWFlit#(addr_, user_),
                   AXI4Lite_BFlit#(user_),
                   Bit#(addr_));
  function routingField(x) = x.awaddr;
  function noRouteFound(x) = AXI4Lite_BFlit { bresp: DECERR, buser: ? };
endinstance
instance DetectLast#(AXI4Lite_AWFlit#(addr_, user_));
  function detectLast(x) = True;
endinstance

// Master interfaces
(* always_ready, always_enabled *)
interface AXI4Lite_AW_Master_Synth#(numeric type addr_, numeric type user_);
  method Bit#(addr_) awaddr;
  method AXI4_Prot   awprot;
  method Bit#(user_) awuser;
  method Bool        awvalid;
  (* prefix="" *) method Action awready(Bool awready);
endinterface

interface AXI4Lite_AW_Master_Xactor#(numeric type addr_, numeric type user_);
  method Action reset;
  interface Sink#(AXI4Lite_AWFlit#(addr_, user_))   sink;
  interface AXI4Lite_AW_Master_Synth#(addr_, user_) masterSynth;
endinterface

// Slave interfaces
(* always_ready, always_enabled *)
interface AXI4Lite_AW_Slave_Synth#(numeric type addr_, numeric type user_);
  (* prefix="" *) method Action awaddr (Bit#(addr_) awaddr);
  (* prefix="" *) method Action awprot (AXI4_Prot   awprot);
  (* prefix="" *) method Action awuser (Bit#(user_) awuser);
  (* prefix="" *) method Action awvalid(Bool        awvalid);
  method Bool awready;
endinterface

interface AXI4Lite_AW_Slave_Xactor#(numeric type addr_, numeric type user_);
  method Action reset;
  interface Source#(AXI4Lite_AWFlit#(addr_, user_)) source;
  interface AXI4Lite_AW_Slave_Synth#(addr_, user_)  slaveSynth;
endinterface

// connectable instances
instance Connectable#(AXI4Lite_AW_Master_Synth#(a, b),
                      AXI4Lite_AW_Slave_Synth#(a, b));
  module mkConnection#(AXI4Lite_AW_Master_Synth#(a,b ) m,
                       AXI4Lite_AW_Slave_Synth#(a, b) s)(Empty);
    rule connect_awaddr;  s.awaddr(m.awaddr);   endrule
    rule connect_awprot;  s.awprot(m.awprot);   endrule
    rule connect_awuser;  s.awuser(m.awuser);   endrule
    rule connect_awvalid; s.awvalid(m.awvalid); endrule
    rule connect_awready; m.awready(s.awready); endrule
  endmodule
endinstance
instance Connectable#(AXI4Lite_AW_Slave_Synth#(a, b),
                      AXI4Lite_AW_Master_Synth#(a, b));
  module mkConnection#(AXI4Lite_AW_Slave_Synth#(a, b) s,
                       AXI4Lite_AW_Master_Synth#(a, b) m)(Empty);
    mkConnection(m, s);
  endmodule
endinstance

/////////////////////////////////
// AXI4Lite Write Data Channel //
////////////////////////////////////////////////////////////////////////////////

// Flit type
typedef struct {
  Bit#(data_)           wdata;
  Bit#(TDiv#(data_, 8)) wstrb;
  Bit#(user_)           wuser;
} AXI4Lite_WFlit#(numeric type data_,
                  numeric type user_) deriving (Bits, FShow);
instance DefaultValue#(AXI4Lite_WFlit#(data_, user_));
  function defaultValue = AXI4Lite_WFlit { wdata: ?, wstrb: ~0, wuser: ? };
endinstance
instance DetectLast#(AXI4Lite_WFlit#(data_, user_));
  function detectLast(x) = True;
endinstance

// Master interfaces
(* always_ready, always_enabled *)
interface AXI4Lite_W_Master_Synth#(numeric type data_, numeric type user_);
  method Bit#(data_)           wdata;
  method Bit#(TDiv#(data_, 8)) wstrb;
  method Bit#(user_)           wuser;
  method Bool                  wvalid;
  (* prefix="" *) method Action wready(Bool wready);
endinterface

interface AXI4Lite_W_Master_Xactor#(numeric type data_, numeric type user_);
  method Action reset;
  interface Sink#(AXI4Lite_WFlit#(data_, user_))   sink;
  interface AXI4Lite_W_Master_Synth#(data_, user_) masterSynth;
endinterface

// Slave interfaces
(* always_ready, always_enabled *)
interface AXI4Lite_W_Slave_Synth#(numeric type data_, numeric type user_);
  (* prefix="" *) method Action wdata (Bit#(data_)            wdata);
  (* prefix="" *) method Action wstrb (Bit#(TDiv#(data_,  8)) wstrb);
  (* prefix="" *) method Action wuser (Bit#(user_)            wuser);
  (* prefix="" *) method Action wvalid(Bool                   wvalid);
  method Bool wready;
endinterface

interface AXI4Lite_W_Slave_Xactor#(numeric type data_, numeric type user_);
  method Action reset;
  interface Source#(AXI4Lite_WFlit#(data_, user_)) source;
  interface AXI4Lite_W_Slave_Synth#(data_, user_)  slaveSynth;
endinterface

// connectable instances
instance Connectable#(AXI4Lite_W_Master_Synth#(a, b),
                      AXI4Lite_W_Slave_Synth#(a, b));
  module mkConnection#(AXI4Lite_W_Master_Synth#(a, b) m,
                       AXI4Lite_W_Slave_Synth#(a, b) s)(Empty);
    rule connect_wdata;  s.wdata(m.wdata);   endrule
    rule connect_wstrb;  s.wstrb(m.wstrb);   endrule
    rule connect_wuser;  s.wuser(m.wuser);   endrule
    rule connect_wvalid; s.wvalid(m.wvalid); endrule
    rule connect_wready; m.wready(s.wready); endrule
  endmodule
endinstance
instance Connectable#(AXI4Lite_W_Slave_Synth#(a, b),
                      AXI4Lite_W_Master_Synth#(a, b));
  module mkConnection#(AXI4Lite_W_Slave_Synth#(a, b) s,
                       AXI4Lite_W_Master_Synth#(a, b) m)(Empty);
    mkConnection(m, s);
  endmodule
endinstance

/////////////////////////////////////
// AXI4Lite Write Response Channel //
////////////////////////////////////////////////////////////////////////////////

// Flit type
typedef struct {
  AXI4_Resp   bresp;
  Bit#(user_) buser;
} AXI4Lite_BFlit#(numeric type user_) deriving (Bits, FShow);
instance DefaultValue#(AXI4Lite_BFlit#(user_));
  function defaultValue = AXI4Lite_BFlit { bresp: OKAY, buser: ? };
endinstance
instance DetectLast#(AXI4Lite_BFlit#(user_));
  function detectLast(x) = True;
endinstance

// Master interfaces
(* always_ready, always_enabled *)
interface AXI4Lite_B_Master_Synth#(numeric type user_);
  (* prefix="" *) method Action bresp (AXI4_Resp   bresp);
  (* prefix="" *) method Action buser (Bit#(user_) buser);
  (* prefix="" *) method Action bvalid(Bool        bvalid);
  method Bool bready;
endinterface

interface AXI4Lite_B_Master_Xactor#(numeric type user_);
  method Action reset;
  interface Source#(AXI4Lite_BFlit#(user_)) source;
  interface AXI4Lite_B_Master_Synth#(user_) masterSynth;
endinterface

// Slave interfaces
(* always_ready, always_enabled *)
interface AXI4Lite_B_Slave_Synth#(numeric type user_);
  method AXI4_Resp   bresp;
  method Bit#(user_) buser;
  method Bool        bvalid;
  (* prefix="" *) method Action bready(Bool bready);
endinterface

interface AXI4Lite_B_Slave_Xactor#(numeric type user_);
  method Action reset;
  interface Sink#(AXI4Lite_BFlit#(user_))  sink;
  interface AXI4Lite_B_Slave_Synth#(user_) slaveSynth;
endinterface

// connectable instances
instance Connectable#(AXI4Lite_B_Master_Synth#(a), AXI4Lite_B_Slave_Synth#(a));
  module mkConnection#(AXI4Lite_B_Master_Synth#(a) m,
                       AXI4Lite_B_Slave_Synth#(a) s)(Empty);
    rule connect_bresp;  m.bresp(s.bresp);   endrule
    rule connect_buser;  m.buser(s.buser);   endrule
    rule connect_bvalid; m.bvalid(s.bvalid); endrule
    rule connect_bready; s.bready(m.bready); endrule
  endmodule
endinstance
instance Connectable#(AXI4Lite_B_Slave_Synth#(a), AXI4Lite_B_Master_Synth#(a));
  module mkConnection#(AXI4Lite_B_Slave_Synth#(a) s,
                       AXI4Lite_B_Master_Synth#(a) m)(Empty);
    mkConnection(m, s);
  endmodule
endinstance

///////////////////////////////////
// AXI4Lite Read Address Channel //
////////////////////////////////////////////////////////////////////////////////

// Flit type
typedef struct {
  Bit#(addr_) araddr;
  AXI4_Prot   arprot;
  Bit#(user_) aruser;
} AXI4Lite_ARFlit#(numeric type addr_,
                   numeric type user_) deriving (Bits, FShow);
instance DefaultValue#(AXI4Lite_ARFlit#(addr_, user_));
  function defaultValue = AXI4Lite_ARFlit { araddr: ?, arprot: 0, aruser: ? };
endinstance
instance Routable#(
  AXI4Lite_ARFlit#(addr_, user_),
  AXI4Lite_RFlit#(data_, user_),
  Bit#(addr_));
  function routingField(x) = x.araddr;
  function noRouteFound(x) = AXI4Lite_RFlit {
    rdata: ?, rresp: DECERR, ruser: ?
  };
endinstance
instance DetectLast#(AXI4Lite_ARFlit#(addr_, user_));
  function detectLast(x) = True;
endinstance

// Master interfaces
(* always_ready, always_enabled *)
interface AXI4Lite_AR_Master_Synth#(numeric type addr_, numeric type user_);
  method Bit#(addr_) araddr;
  method AXI4_Prot   arprot;
  method Bit#(user_) aruser;
  method Bool        arvalid;
  (* prefix="" *) method Action arready(Bool arready);
endinterface

interface AXI4Lite_AR_Master_Xactor#(numeric type addr_, numeric type user_);
  method Action reset;
  interface Source#(AXI4Lite_ARFlit#(addr_, user_)) source;
  interface AXI4Lite_AR_Master_Synth#(addr_, user_) masterSynth;
endinterface

// Slave interfaces
(* always_ready, always_enabled *)
interface AXI4Lite_AR_Slave_Synth#(numeric type addr_, numeric type user_);
  (* prefix="" *) method Action araddr (Bit#(addr_) araddr);
  (* prefix="" *) method Action arprot (AXI4_Prot   arprot);
  (* prefix="" *) method Action aruser (Bit#(user_) aruser);
  (* prefix="" *) method Action arvalid(Bool        arvalid);
  method Bool arready;
endinterface

interface AXI4Lite_AR_Slave_Xactor#(numeric type addr_, numeric type user_);
  method Action reset;
  interface Sink#(AXI4Lite_ARFlit#(addr_, user_))  sink;
  interface AXI4Lite_AR_Slave_Synth#(addr_, user_) slaveSynth;
endinterface

// connectable instances
instance Connectable#(AXI4Lite_AR_Master_Synth#(a, b),
                      AXI4Lite_AR_Slave_Synth#(a, b));
  module mkConnection#(AXI4Lite_AR_Master_Synth#(a, b) m,
                       AXI4Lite_AR_Slave_Synth#(a, b) s)(Empty);
    rule connect_araddr;  s.araddr(m.araddr);   endrule
    rule connect_arprot;  s.arprot(m.arprot);   endrule
    rule connect_aruser;  s.aruser(m.aruser);   endrule
    rule connect_arvalid; s.arvalid(m.arvalid); endrule
    rule connect_arready; m.arready(s.arready); endrule
  endmodule
endinstance
instance Connectable#(AXI4Lite_AR_Slave_Synth#(a, b),
                      AXI4Lite_AR_Master_Synth#(a, b));
  module mkConnection#(AXI4Lite_AR_Slave_Synth#(a, b) s,
                       AXI4Lite_AR_Master_Synth#(a, b) m)(Empty);
    mkConnection(m, s);
  endmodule
endinstance

////////////////////////////////
// AXI4Lite Read Data Channel //
////////////////////////////////////////////////////////////////////////////////

// Flit type
typedef struct {
  Bit#(data_) rdata;
  AXI4_Resp   rresp;
  Bit#(user_) ruser;
} AXI4Lite_RFlit#(numeric type data_,
                  numeric type user_) deriving (Bits, FShow);
instance DefaultValue#(AXI4Lite_RFlit#(data_, user_));
  function defaultValue = AXI4Lite_RFlit { rdata: ?, rresp: OKAY, ruser: ? };
endinstance
instance DetectLast#(AXI4Lite_RFlit#(data_, user_));
  function detectLast(x) = True;
endinstance

// Master interfaces
(* always_ready, always_enabled *)
interface AXI4Lite_R_Master_Synth#(numeric type data_, numeric type user_);
  (* prefix="" *) method Action rdata (Bit#(data_) rdata);
  (* prefix="" *) method Action rresp (AXI4_Resp   rresp);
  (* prefix="" *) method Action ruser (Bit#(user_) ruser);
  (* prefix="" *) method Action rvalid(Bool        rvalid);
  method Bool rready;
endinterface

interface AXI4Lite_R_Master_Xactor#(numeric type data_, numeric type user_);
  method Action reset;
  interface Source#(AXI4Lite_RFlit#(data_, user_)) source;
  interface AXI4Lite_R_Master_Synth#(data_, user_) masterSynth;
endinterface

// Slave interfaces
(* always_ready, always_enabled *)
interface AXI4Lite_R_Slave_Synth#(numeric type data_, numeric type user_);
  method Bit#(data_) rdata;
  method AXI4_Resp   rresp;
  method Bit#(user_) ruser;
  method Bool        rvalid;
  (* prefix="" *) method Action rready(Bool rready);
endinterface

interface AXI4Lite_R_Slave_Xactor#(numeric type data_, numeric type user_);
  method Action reset;
  interface Sink#(AXI4Lite_RFlit#(data_, user_))  sink;
  interface AXI4Lite_R_Slave_Synth#(data_, user_) slaveSynth;
endinterface

// connectable instances
instance Connectable#(AXI4Lite_R_Master_Synth#(a, b),
                      AXI4Lite_R_Slave_Synth#(a, b));
  module mkConnection#(AXI4Lite_R_Master_Synth#(a, b) m,
                       AXI4Lite_R_Slave_Synth#(a, b) s)(Empty);
    rule connect_rdata;  m.rdata(s.rdata);   endrule
    rule connect_rresp;  m.rresp(s.rresp);   endrule
    rule connect_ruser;  m.ruser(s.ruser);   endrule
    rule connect_rvalid; m.rvalid(s.rvalid); endrule
    rule connect_rready; s.rready(m.rready); endrule
  endmodule
endinstance
instance Connectable#(AXI4Lite_R_Slave_Synth#(a, b),
                      AXI4Lite_R_Master_Synth#(a, b));
  module mkConnection#(AXI4Lite_R_Slave_Synth#(a, b) s,
                       AXI4Lite_R_Master_Synth#(a, b) m)(Empty);
    mkConnection(m, s);
  endmodule
endinstance

/////////////////////
// AXI4Lite Master //
////////////////////////////////////////////////////////////////////////////////

interface AXI4Lite_Master#(
  numeric type addr_,
  numeric type data_,
  numeric type awuser_,
  numeric type wuser_,
  numeric type buser_,
  numeric type aruser_,
  numeric type ruser_);
  interface Source#(AXI4Lite_AWFlit#(addr_, awuser_)) aw;
  interface Source#(AXI4Lite_WFlit#(data_, wuser_))   w;
  interface Sink#(AXI4Lite_BFlit#(buser_))            b;
  interface Source#(AXI4Lite_ARFlit#(addr_, aruser_)) ar;
  interface Sink#(AXI4Lite_RFlit#(data_, ruser_))     r;
endinterface

interface AXI4Lite_Master_Synth#(
  numeric type addr_,
  numeric type data_,
  numeric type awuser_,
  numeric type wuser_,
  numeric type buser_,
  numeric type aruser_,
  numeric type ruser_);
  interface AXI4Lite_AW_Master_Synth#(addr_, awuser_) aw;
  interface AXI4Lite_W_Master_Synth#(data_, wuser_)   w;
  interface AXI4Lite_B_Master_Synth#(buser_)          b;
  interface AXI4Lite_AR_Master_Synth#(addr_, aruser_) ar;
  interface AXI4Lite_R_Master_Synth#(data_, ruser_)   r;
endinterface

interface AXI4Lite_Master_Xactor#(
  numeric type addr_,
  numeric type data_,
  numeric type awuser_,
  numeric type wuser_,
  numeric type buser_,
  numeric type aruser_,
  numeric type ruser_);
  method Action reset;
  interface AXI4Lite_Master#(addr_,
                             data_,
                             awuser_,
                             wuser_,
                             buser_,
                             aruser_,
                             ruser_)       master;
  interface AXI4Lite_Master_Synth#(addr_,
                                   data_,
                                   awuser_,
                                   wuser_,
                                   buser_,
                                   aruser_,
                                   ruser_) masterSynth;
endinterface

////////////////////
// AXI4Lite Slave //
////////////////////////////////////////////////////////////////////////////////

interface AXI4Lite_Slave#(
  numeric type addr_,
  numeric type data_,
  numeric type awuser_,
  numeric type wuser_,
  numeric type buser_,
  numeric type aruser_,
  numeric type ruser_);
  interface Sink#(AXI4Lite_AWFlit#(addr_, awuser_)) aw;
  interface Sink#(AXI4Lite_WFlit#(data_, wuser_))   w;
  interface Source#(AXI4Lite_BFlit#(buser_))        b;
  interface Sink#(AXI4Lite_ARFlit#(addr_, aruser_)) ar;
  interface Source#(AXI4Lite_RFlit#(data_, ruser_)) r;
endinterface

interface AXI4Lite_Slave_Synth#(
  numeric type addr_,
  numeric type data_,
  numeric type awuser_,
  numeric type wuser_,
  numeric type buser_,
  numeric type aruser_,
  numeric type ruser_);
  interface AXI4Lite_AW_Slave_Synth#(addr_, awuser_) aw;
  interface AXI4Lite_W_Slave_Synth#(data_, wuser_)   w;
  interface AXI4Lite_B_Slave_Synth#(buser_)          b;
  interface AXI4Lite_AR_Slave_Synth#(addr_, aruser_) ar;
  interface AXI4Lite_R_Slave_Synth#(data_, ruser_)   r;
endinterface

interface AXI4Lite_Slave_Xactor#(
  numeric type addr_,
  numeric type data_,
  numeric type awuser_,
  numeric type wuser_,
  numeric type buser_,
  numeric type aruser_,
  numeric type ruser_);
  method Action reset;
  interface AXI4Lite_Slave#(addr_,
                            data_,
                            awuser_,
                            wuser_,
                            buser_,
                            aruser_,
                            ruser_)       slave;
  interface AXI4Lite_Slave_Synth#(addr_,
                                  data_,
                                  awuser_,
                                  wuser_,
                                  buser_,
                                  aruser_,
                                  ruser_) slaveSynth;
endinterface

////////////////////////////////////
// AXI4Lite Shim Master <-> Slave //
////////////////////////////////////////////////////////////////////////////////

interface AXI4Lite_Shim#(
  numeric type addr_,
  numeric type data_,
  numeric type awuser_,
  numeric type wuser_,
  numeric type buser_,
  numeric type aruser_,
  numeric type ruser_);
  interface AXI4Lite_Master#(
    addr_, data_, awuser_, wuser_, buser_, aruser_, ruser_
  ) master;
  interface AXI4Lite_Slave#(
    addr_, data_, awuser_, wuser_, buser_, aruser_, ruser_
  ) slave;
endinterface

////////////////////////////////////
// AXI4Lite Connectable instances //
////////////////////////////////////////////////////////////////////////////////

instance Connectable#(
  AXI4Lite_Master#(a, b, c, d, e, f, g),
  AXI4Lite_Slave#(a, b, c, d, e, f, g));
  module mkConnection#(
    AXI4Lite_Master#(a, b, c, d, e, f, g) m,
    AXI4Lite_Slave#(a, b, c, d, e, f, g) s)
    (Empty);
    mkConnection(m.aw, s.aw);
    mkConnection(m.w, s.w);
    mkConnection(m.b, s.b);
    mkConnection(m.ar, s.ar);
    mkConnection(m.r, s.r);
  endmodule
endinstance
instance Connectable#(
  AXI4Lite_Slave#(a, b, c, d, e, f, g),
  AXI4Lite_Master#(a, b, c, d, e, f, g));
  module mkConnection#(
    AXI4Lite_Slave#(a, b, c, d, e, f, g) s,
    AXI4Lite_Master#(a, b, c, d, e, f, g) m)
    (Empty);
    mkConnection(m, s);
  endmodule
endinstance

instance Connectable#(
  AXI4Lite_Master_Synth#(a, b, c, d, e, f, g),
  AXI4Lite_Slave_Synth#(a, b, c, d, e, f, g));
  module mkConnection#(
    AXI4Lite_Master_Synth#(a, b, c, d, e, f, g) m,
    AXI4Lite_Slave_Synth#(a, b, c, d, e, f, g) s)
    (Empty);
    mkConnection(m.aw, s.aw);
    mkConnection(m.w, s.w);
    mkConnection(m.b, s.b);
    mkConnection(m.ar, s.ar);
    mkConnection(m.r, s.r);
  endmodule
endinstance
instance Connectable#(
  AXI4Lite_Slave_Synth#(a, b, c, d, e, f, g),
  AXI4Lite_Master_Synth#(a, b, c, d, e, f, g));
  module mkConnection#(
    AXI4Lite_Slave_Synth#(a, b, c, d, e, f, g) s,
    AXI4Lite_Master_Synth#(a, b, c, d, e, f, g) m)
    (Empty);
    mkConnection(m, s);
  endmodule
endinstance

////////////////////////////////////////
// AXI4Lite write channel helper type //
////////////////////////////////////////////////////////////////////////////////

typedef struct {
  AXI4Lite_AWFlit#(addr_, awuser_) aw;
  AXI4Lite_WFlit#(data_, wuser_)  w;
} AXI4Lite_WriteFlit#(
  numeric type addr_,
  numeric type data_,
  numeric type awuser_,
  numeric type wuser_) deriving (Bits, FShow);
instance Routable#(
  AXI4Lite_WriteFlit#(addr_, data_, awuser_, wuser_),
  AXI4Lite_BFlit#(buser_),
  Bit#(addr_)) provisos (
    Routable#(AXI4Lite_AWFlit#(addr_, awuser_),
              AXI4Lite_BFlit#(buser_),
              Bit#(addr_))
  );
  function routingField(x) = routingField(x.aw);
  function noRouteFound(x) = noRouteFound(x.aw);
endinstance
instance DetectLast#(AXI4Lite_WriteFlit#(addr_, data_, awuser_, wuser_));
  function detectLast(x) = detectLast(x.w);
endinstance
