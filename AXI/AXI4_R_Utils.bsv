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

///////////////////////////
// AXI Read Data Channel //
////////////////////////////////////////////////////////////////////////////////

// typeclasses to convert to/from the flit type

typeclass ToAXIRFlit#(type t,
numeric type id_, numeric type data_, numeric type user_);
  function RFlit#(id_, data_, user_) toAXIRFlit (t x);
endtypeclass

instance ToAXIRFlit#(RFlit#(a, b, c), a, b, c);
  function toAXIRFlit = id;
endinstance

typeclass ToAXIRLiteFlit#(type t, numeric type data_);
  function RLiteFlit#(data_) toAXIRLiteFlit (t x);
endtypeclass

instance ToAXIRLiteFlit#(RLiteFlit#(a), a);
  function toAXIRLiteFlit = id;
endinstance

typeclass FromAXIRFlit#(type t,
numeric type id_, numeric type data_, numeric type user_);
  function t fromAXIRFlit (RFlit#(id_, data_, user_) x);
endtypeclass

instance FromAXIRFlit#(RFlit#(a, b, c), a, b, c);
  function fromAXIRFlit = id;
endinstance

typeclass FromAXIRLiteFlit#(type t, numeric type data_);
  function t fromAXIRLiteFlit (RLiteFlit#(data_) x);
endtypeclass

instance FromAXIRLiteFlit#(RLiteFlit#(a), a);
  function fromAXIRLiteFlit = id;
endinstance

// typeclass to turn an interface to the Master interface

typeclass ToAXIRMaster#(type t);
  module toAXIRMaster#(t#(x) ifc) (RMaster#(id_, data_, user_))
  provisos (FromAXIRFlit#(x, id_, data_, user_));
endtypeclass

instance ToAXIRMaster#(FIFOF);
  module toAXIRMaster#(FIFOF#(t) ff)
  (RMaster#(id_, data_, user_)) provisos (FromAXIRFlit#(t, id_, data_, user_));

    let w_rid   <- mkDWire(?);
    let w_rdata <- mkDWire(?);
    let w_rresp <- mkDWire(?);
    let w_rlast <- mkDWire(?);
    let w_ruser <- mkDWire(?);
    PulseWire enqWire <- mkPulseWire;
    rule doEnq (enqWire && ff.notFull);
      ff.enq(fromAXIRFlit(RFlit{
        rid:   w_rid,
        rdata: w_rdata,
        rresp: w_rresp,
        rlast: w_rlast,
        ruser: w_ruser
      }));
    endrule

    method rid(id)       = action w_rid   <= id; endaction;
    method rdata(data)   = action w_rdata <= data; endaction;
    method rresp(resp)   = action w_rresp <= resp; endaction;
    method rlast(last)   = action w_rlast <= last; endaction;
    method ruser(user)   = action w_ruser <= user; endaction;
    method rvalid(valid) = action if (valid) enqWire.send; endaction;
    method rready        = ff.notFull;

  endmodule
endinstance

typeclass ToAXIRLiteMaster#(type t);
  module toAXIRLiteMaster#(t#(x) ifc) (RLiteMaster#(data_))
  provisos (FromAXIRLiteFlit#(x, data_));
endtypeclass

instance ToAXIRLiteMaster#(FIFOF);
  module toAXIRLiteMaster#(FIFOF#(t) ff)
  (RLiteMaster#(data_)) provisos (FromAXIRLiteFlit#(t, data_));

    let w_rdata <- mkDWire(?);
    let w_rresp <- mkDWire(?);
    PulseWire enqWire <- mkPulseWire;
    rule doEnq (enqWire && ff.notFull);
      ff.enq(fromAXIRLiteFlit(RLiteFlit{rdata: w_rdata, rresp: w_rresp}));
    endrule

    method rdata(data)   = action w_rdata <= data; endaction;
    method rresp(resp)   = action w_rresp <= resp; endaction;
    method rvalid(valid) = action if (valid) enqWire.send; endaction;
    method rready        = ff.notFull;

  endmodule
endinstance

// typeclass to turn an interface to the Slave interface

typeclass ToAXIRSlave#(type t);
  module toAXIRSlave#(t#(x) ifc) (RSlave#(id_, data_, user_))
  provisos (ToAXIRFlit#(x, id_, data_, user_));
endtypeclass

instance ToAXIRSlave#(FIFOF);
  module toAXIRSlave#(FIFOF#(t) ff)
  (RSlave#(id_, data_, user_)) provisos (ToAXIRFlit#(t, id_, data_, user_));

    Wire#(RFlit#(id_, data_, user_)) flit <- mkDWire(?);
    rule getFlit (ff.notEmpty); flit <= toAXIRFlit(ff.first); endrule
    PulseWire deqWire <- mkPulseWire;
    rule doDeq (deqWire && ff.notEmpty); ff.deq; endrule

    method rid    = flit.rid;
    method rdata  = flit.rdata;
    method rresp  = flit.rresp;
    method rlast  = flit.rlast;
    method ruser  = flit.ruser;
    method rvalid = ff.notEmpty;
    method rready(rdy) = action if (rdy) deqWire.send; endaction;

  endmodule
endinstance

typeclass ToAXIRLiteSlave#(type t);
  module toAXIRLiteSlave#(t#(x) ifc) (RLiteSlave#(data_))
  provisos (ToAXIRLiteFlit#(x, data_));
endtypeclass

instance ToAXIRLiteSlave#(FIFOF);
  module toAXIRLiteSlave#(FIFOF#(t) ff)
  (RLiteSlave#(data_)) provisos (ToAXIRLiteFlit#(t, data_));

    Wire#(RLiteFlit#(data_)) flit <- mkDWire(?);
    rule getFlit (ff.notEmpty); flit <= toAXIRLiteFlit(ff.first); endrule
    PulseWire deqWire <- mkPulseWire;
    rule doDeq (deqWire && ff.notEmpty); ff.deq; endrule

    method rdata  = flit.rdata;
    method rresp  = flit.rresp;
    method rvalid = ff.notEmpty;
    method rready(rdy) = action if (rdy) deqWire.send; endaction;

  endmodule
endinstance

// Shim for RMaster to FIFOF#(RFlit)

interface RMasterShim#(numeric type id_, numeric type data_, numeric type user_);
  interface RMaster#(id_, data_, user_) master;
  interface FIFOF#(RFlit#(id_, data_, user_)) fifof;
endinterface

module mkRMasterShim (RMasterShim#(id_, data_, user_));
  let ff  <- mkBypassFIFOF;
  let ifc <- toAXIRMaster(ff);
  interface master = ifc;
  interface fifof  = ff;
endmodule

interface RLiteMasterShim#(numeric type data_);
  interface RLiteMaster#(data_) master;
  interface FIFOF#(RLiteFlit#(data_)) fifof;
endinterface

module mkRLiteMasterShim (RLiteMasterShim#(data_));
  let ff  <- mkBypassFIFOF;
  let ifc <- toAXIRLiteMaster(ff);
  interface master = ifc;
  interface fifof  = ff;
endmodule

// Shim for FIFOF#(RFlit) to RSlave

interface RSlaveShim#(numeric type id_, numeric type data_, numeric type user_);
  interface RSlave#(id_, data_, user_) slave;
  interface FIFOF#(RFlit#(id_, data_, user_)) fifof;
endinterface

module mkRSlaveShim (RSlaveShim#(id_, data_, user_));
  let ff  <- mkBypassFIFOF;
  let ifc <- toAXIRSlave(ff);
  interface slave = ifc;
  interface fifof = ff;
endmodule

interface RLiteSlaveShim#(numeric type data_);
  interface RLiteSlave#(data_) slave;
  interface FIFOF#(RLiteFlit#(data_)) fifof;
endinterface

module mkRLiteSlaveShim (RLiteSlaveShim#(data_));
  let ff  <- mkBypassFIFOF;
  let ifc <- toAXIRLiteSlave(ff);
  interface slave = ifc;
  interface fifof = ff;
endmodule
