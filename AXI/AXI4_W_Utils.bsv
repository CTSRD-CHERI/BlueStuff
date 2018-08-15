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

import FIFOF :: *;
import SpecialFIFOs :: *;

////////////////////////////
// AXI Write Data Channel //
////////////////////////////////////////////////////////////////////////////////

// typeclasses to convert to/from the flit type

typeclass ToAXIWFlit#(type t, numeric type data_, numeric type user_);
  function WFlit#(data_, user_) toAXIWFlit (t x);
endtypeclass

instance ToAXIWFlit#(WFlit#(a, b), a, b);
  function toAXIWFlit = id;
endinstance

typeclass ToAXIWLiteFlit#(type t, numeric type data_);
  function WLiteFlit#(data_) toAXIWLiteFlit (t x);
endtypeclass

instance ToAXIWLiteFlit#(WLiteFlit#(a), a);
  function toAXIWLiteFlit = id;
endinstance

typeclass FromAXIWFlit#(type t,
numeric type data_, numeric type user_);
  function t fromAXIWFlit (WFlit#(data_, user_) x);
endtypeclass

instance FromAXIWFlit#(WFlit#(a, b), a, b);
  function fromAXIWFlit = id;
endinstance

typeclass FromAXIWLiteFlit#(type t, numeric type data_);
  function t fromAXIWLiteFlit (WLiteFlit#(data_) x);
endtypeclass

instance FromAXIWLiteFlit#(WLiteFlit#(a), a);
  function fromAXIWLiteFlit = id;
endinstance

// typeclass to turn an interface to the Master interface

typeclass ToAXIWMaster#(type t);
  module toAXIWMaster#(t#(x) ifc) (WMaster#(data_, user_))
  provisos (ToAXIWFlit#(x, data_, user_));
endtypeclass

instance ToAXIWMaster#(FIFOF);
  module toAXIWMaster#(FIFOF#(t) ff)
  (WMaster#(data_, user_)) provisos (ToAXIWFlit#(t, data_, user_));

    Wire#(WFlit#(data_, user_)) flit <- mkDWire(?);
    rule getFlit (ff.notEmpty); flit <= toAXIWFlit(ff.first); endrule
    PulseWire deqWire <- mkPulseWire;
    rule doDeq (deqWire && ff.notEmpty); ff.deq; endrule

    method wdata  = flit.wdata;
    method wstrb  = flit.wstrb;
    method wlast  = flit.wlast;
    method wuser  = flit.wuser;
    method wvalid = ff.notEmpty;
    method wready(rdy) = action if (rdy) deqWire.send; endaction;

  endmodule
endinstance

typeclass ToAXIWLiteMaster#(type t);
  module toAXIWLiteMaster#(t#(x) ifc) (WLiteMaster#(data_))
  provisos (ToAXIWLiteFlit#(x, data_));
endtypeclass

instance ToAXIWLiteMaster#(FIFOF);
  module toAXIWLiteMaster#(FIFOF#(t) ff)
  (WLiteMaster#(data_)) provisos (ToAXIWLiteFlit#(t, data_));

    Wire#(WLiteFlit#(data_)) flit <- mkDWire(?);
    rule getFlit (ff.notEmpty); flit <= toAXIWLiteFlit(ff.first); endrule
    PulseWire deqWire <- mkPulseWire;
    rule doDeq (deqWire && ff.notEmpty); ff.deq; endrule

    method wdata  = flit.wdata;
    method wstrb  = flit.wstrb;
    method wvalid = ff.notEmpty;
    method wready(rdy) = action if (rdy) deqWire.send; endaction;

  endmodule
endinstance

// typeclass to turn an interface to the Slave interface

typeclass ToAXIWSlave#(type t);
  module toAXIWSlave#(t#(x) ifc) (WSlave#(data_, user_))
  provisos (FromAXIWFlit#(x, data_, user_));
endtypeclass

instance ToAXIWSlave#(FIFOF);
  module toAXIWSlave#(FIFOF#(t) ff)
  (WSlave#(data_, user_)) provisos (FromAXIWFlit#(t, data_, user_));

    let w_wdata <- mkDWire(?);
    let w_wstrb <- mkDWire(?);
    let w_wlast <- mkDWire(?);
    let w_wuser <- mkDWire(?);
    PulseWire enqWire <- mkPulseWire;
    rule doEnq (enqWire && ff.notFull);
      ff.enq(fromAXIWFlit(WFlit{
        wdata: w_wdata, wstrb: w_wstrb, wlast: w_wlast, wuser: w_wuser
      }));
    endrule

    method wdata(data)   = action w_wdata <= data; endaction;
    method wstrb(strb)   = action w_wstrb <= strb; endaction;
    method wlast(last)   = action w_wlast <= last; endaction;
    method wuser(user)   = action w_wuser <= user; endaction;
    method wvalid(valid) = action if (valid) enqWire.send; endaction;
    method wready        = ff.notFull;

  endmodule
endinstance

typeclass ToAXIWLiteSlave#(type t);
  module toAXIWLiteSlave#(t#(x) ifc) (WLiteSlave#(data_))
  provisos (FromAXIWLiteFlit#(x, data_));
endtypeclass

instance ToAXIWLiteSlave#(FIFOF);
  module toAXIWLiteSlave#(FIFOF#(t) ff)
  (WLiteSlave#(data_)) provisos (FromAXIWLiteFlit#(t, data_));

    let w_wdata <- mkDWire(?);
    let w_wstrb <- mkDWire(?);
    PulseWire enqWire <- mkPulseWire;
    rule doEnq (enqWire && ff.notFull);
      ff.enq(fromAXIWLiteFlit(WLiteFlit{wdata: w_wdata, wstrb: w_wstrb}));
    endrule

    method wdata(data)   = action w_wdata <= data; endaction;
    method wstrb(strb)   = action w_wstrb <= strb; endaction;
    method wvalid(valid) = action if (valid) enqWire.send; endaction;
    method wready        = ff.notFull;

  endmodule
endinstance

// Shim for WMaster to FIFOF#(WFlit)

interface WMasterShim#(numeric type data_, numeric type user_);
  interface WMaster#(data_, user_) master;
  interface FIFOF#(WFlit#(data_, user_)) fifof;
endinterface

module mkWMasterShim (WMasterShim#(data_, user_));
  let ff  <- mkBypassFIFOF;
  let ifc <- toAXIWMaster(ff);
  interface master = ifc;
  interface fifof  = ff;
endmodule

interface WLiteMasterShim#(numeric type data_);
  interface WLiteMaster#(data_) master;
  interface FIFOF#(WLiteFlit#(data_)) fifof;
endinterface

module mkWLiteMasterShim (WLiteMasterShim#(data_));
  let ff  <- mkBypassFIFOF;
  let ifc <- toAXIWLiteMaster(ff);
  interface master = ifc;
  interface fifof  = ff;
endmodule

// Shim for FIFOF#(WFlit) to WSlave

interface WSlaveShim#(numeric type data_, numeric type user_);
  interface WSlave#(data_, user_) slave;
  interface FIFOF#(WFlit#(data_, user_)) fifof;
endinterface

module mkWSlaveShim (WSlaveShim#(data_, user_));
  let ff  <- mkBypassFIFOF;
  let ifc <- toAXIWSlave(ff);
  interface slave = ifc;
  interface fifof = ff;
endmodule

interface WLiteSlaveShim#(numeric type data_);
  interface WLiteSlave#(data_) slave;
  interface FIFOF#(WLiteFlit#(data_)) fifof;
endinterface

module mkWLiteSlaveShim (WLiteSlaveShim#(data_));
  let ff  <- mkBypassFIFOF;
  let ifc <- toAXIWLiteSlave(ff);
  interface slave = ifc;
  interface fifof = ff;
endmodule
