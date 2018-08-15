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

////////////////////////////////
// AXI Write Response Channel //
////////////////////////////////////////////////////////////////////////////////

// typeclasses to convert to/from the flit type

typeclass ToAXIBFlit#(type t, numeric type id_, numeric type user_);
  function BFlit#(id_, user_) toAXIBFlit (t x);
endtypeclass

instance ToAXIBFlit#(BFlit#(a, b), a, b);
  function toAXIBFlit = id;
endinstance

typeclass ToAXIBLiteFlit#(type t);
  function BLiteFlit toAXIBLiteFlit (t x);
endtypeclass

instance ToAXIBLiteFlit#(BLiteFlit);
  function toAXIBLiteFlit = id;
endinstance

typeclass FromAXIBFlit#(type t, numeric type id_, numeric type user_);
  function t fromAXIBFlit (BFlit#(id_, user_) x);
endtypeclass

instance FromAXIBFlit#(BFlit#(a, b), a, b);
  function fromAXIBFlit = id;
endinstance

typeclass FromAXIBLiteFlit#(type t);
  function t fromAXIBLiteFlit (BLiteFlit x);
endtypeclass

instance FromAXIBLiteFlit#(BLiteFlit);
  function fromAXIBLiteFlit = id;
endinstance

// typeclass to turn an interface to the Master interface

typeclass ToAXIBMaster#(type t);
  module toAXIBMaster#(t#(x) ifc) (BMaster#(id_, user_))
  provisos (FromAXIBFlit#(x, id_, user_));
endtypeclass

instance ToAXIBMaster#(FIFOF);
  module toAXIBMaster#(FIFOF#(t) ff)
  (BMaster#(id_, user_)) provisos (FromAXIBFlit#(t, id_, user_));

    let w_bid   <- mkDWire(?);
    let w_bresp <- mkDWire(?);
    let w_buser <- mkDWire(?);
    PulseWire enqWire <- mkPulseWire;
    rule doEnq (enqWire && ff.notFull);
      ff.enq(fromAXIBFlit(BFlit{bid: w_bid, bresp: w_bresp, buser: w_buser}));
    endrule

    method bid(id)       = action w_bid   <= id; endaction;
    method bresp(resp)   = action w_bresp <= resp; endaction;
    method buser(user)   = action w_buser <= user; endaction;
    method bvalid(valid) = action if (valid) enqWire.send; endaction;
    method bready        = ff.notFull;

  endmodule
endinstance

typeclass ToAXIBLiteMaster#(type t);
  module toAXIBLiteMaster#(t#(x) ifc) (BLiteMaster)
  provisos (FromAXIBLiteFlit#(x));
endtypeclass

instance ToAXIBLiteMaster#(FIFOF);
  module toAXIBLiteMaster#(FIFOF#(t) ff)
  (BLiteMaster) provisos (FromAXIBLiteFlit#(t));

    let w_bresp <- mkDWire(?);
    PulseWire enqWire <- mkPulseWire;
    rule doEnq (enqWire && ff.notFull);
      ff.enq(fromAXIBLiteFlit(BLiteFlit{bresp: w_bresp}));
    endrule

    method bresp(resp)   = action w_bresp <= resp; endaction;
    method bvalid(valid) = action if (valid) enqWire.send; endaction;
    method bready        = ff.notFull;

  endmodule
endinstance

// typeclass to turn an interface to the Slave interface

typeclass ToAXIBSlave#(type t);
  module toAXIBSlave#(t#(x) ifc) (BSlave#(id_, user_))
  provisos (ToAXIBFlit#(x, id_, user_));
endtypeclass

instance ToAXIBSlave#(FIFOF);
  module toAXIBSlave#(FIFOF#(t) ff)
  (BSlave#(id_, user_)) provisos (ToAXIBFlit#(t, id_, user_));

    Wire#(BFlit#(id_, user_)) flit <- mkDWire(?);
    rule getFlit (ff.notEmpty); flit <= toAXIBFlit(ff.first); endrule
    PulseWire deqWire <- mkPulseWire;
    rule doDeq (deqWire && ff.notEmpty); ff.deq; endrule

    method bid    = flit.bid;
    method bresp  = flit.bresp;
    method buser  = flit.buser;
    method bvalid = ff.notEmpty;
    method bready(rdy) = action if (rdy) deqWire.send; endaction;

  endmodule
endinstance

typeclass ToAXIBLiteSlave#(type t);
  module toAXIBLiteSlave#(t#(x) ifc) (BLiteSlave)
  provisos (ToAXIBLiteFlit#(x));
endtypeclass

instance ToAXIBLiteSlave#(FIFOF);
  module toAXIBLiteSlave#(FIFOF#(t) ff)
  (BLiteSlave) provisos (ToAXIBLiteFlit#(t));

    Wire#(BLiteFlit) flit <- mkDWire(?);
    rule getFlit (ff.notEmpty); flit <= toAXIBLiteFlit(ff.first); endrule
    PulseWire deqWire <- mkPulseWire;
    rule doDeq (deqWire && ff.notEmpty); ff.deq; endrule

    method bresp  = flit.bresp;
    method bvalid = ff.notEmpty;
    method bready(rdy) = action if (rdy) deqWire.send; endaction;

  endmodule
endinstance
