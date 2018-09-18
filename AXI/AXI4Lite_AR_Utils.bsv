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

import SourceSink :: *;

import AXI4Lite_Types :: *;

import FIFOF :: *;
import SpecialFIFOs :: *;

//////////////////////////////
// AXI Read Address Channel //
////////////////////////////////////////////////////////////////////////////////

// typeclasses to convert to/from the flit type

typeclass ToAXIARLiteFlit#(type t, numeric type addr_);
  function ARLiteFlit#(addr_) toAXIARLiteFlit (t x);
endtypeclass

instance ToAXIARLiteFlit#(ARLiteFlit#(a), a);
  function toAXIARLiteFlit = id;
endinstance

typeclass FromAXIARLiteFlit#(type t, numeric type addr_);
  function t fromAXIARLiteFlit (ARLiteFlit#(addr_) x);
endtypeclass

instance FromAXIARLiteFlit#(ARLiteFlit#(a), a);
  function fromAXIARLiteFlit = id;
endinstance

// typeclass to turn an interface to the Master interface

typeclass ToAXIARLiteMaster#(type t);
  module toAXIARLiteMaster#(t#(x) ifc) (ARLiteMaster#(addr_))
  provisos (ToAXIARLiteFlit#(x, addr_));
endtypeclass

instance ToAXIARLiteMaster#(Source);
  module toAXIARLiteMaster#(Source#(t) src)
  (ARLiteMaster#(addr_)) provisos (ToAXIARLiteFlit#(t, addr_));

    Wire#(ARLiteFlit#(addr_)) flit <- mkDWire(?);
    rule getFlit (src.canGet); flit <= toAXIARLiteFlit(src.peek); endrule
    PulseWire getWire <- mkPulseWire;
    rule doGet (getWire && src.canGet); let _ <- src.get; endrule

    method araddr   = flit.araddr;
    method arprot   = flit.arprot;
    method arvalid  = src.canGet;
    method arready(rdy) = action if (rdy) getWire.send; endaction;

  endmodule
endinstance

instance ToAXIARLiteMaster#(FIFOF);
  module toAXIARLiteMaster#(FIFOF#(t) ff)
  (ARLiteMaster#(addr_)) provisos (ToAXIARLiteFlit#(t, addr_));

    Wire#(ARLiteFlit#(addr_)) flit <- mkDWire(?);
    rule getFlit (ff.notEmpty); flit <= toAXIARLiteFlit(ff.first); endrule
    PulseWire deqWire <- mkPulseWire;
    rule doDeq (deqWire && ff.notEmpty); ff.deq; endrule

    method araddr   = flit.araddr;
    method arprot   = flit.arprot;
    method arvalid  = ff.notEmpty;
    method arready(rdy) = action if (rdy) deqWire.send; endaction;

  endmodule
endinstance

// typeclass to turn an interface to the Slave interface

typeclass ToAXIARLiteSlave#(type t);
  module toAXIARLiteSlave#(t#(x) ifc) (ARLiteSlave#(addr_))
  provisos (FromAXIARLiteFlit#(x, addr_));
endtypeclass

instance ToAXIARLiteSlave#(Sink);
  module toAXIARLiteSlave#(Sink#(t) snk)
  (ARLiteSlave#(addr_)) provisos (FromAXIARLiteFlit#(t, addr_));

    let w_araddr   <- mkDWire(?);
    let w_arprot   <- mkDWire(?);
    PulseWire putWire <- mkPulseWire;
    rule doPut (putWire && snk.canPut);
      snk.put(fromAXIARLiteFlit(ARLiteFlit{araddr: w_araddr, arprot: w_arprot}));
    endrule

    method araddr(addr)   = action w_araddr <= addr; endaction;
    method arprot(prot)   = action w_arprot <= prot; endaction;
    method arvalid(valid) = action if (valid) putWire.send; endaction;
    method arready        = snk.canPut;

  endmodule
endinstance

instance ToAXIARLiteSlave#(FIFOF);
  module toAXIARLiteSlave#(FIFOF#(t) ff)
  (ARLiteSlave#(addr_)) provisos (FromAXIARLiteFlit#(t, addr_));

    let w_araddr   <- mkDWire(?);
    let w_arprot   <- mkDWire(?);
    PulseWire enqWire <- mkPulseWire;
    rule doEnq (enqWire && ff.notFull);
      ff.enq(fromAXIARLiteFlit(ARLiteFlit{araddr: w_araddr, arprot: w_arprot}));
    endrule

    method araddr(addr)   = action w_araddr <= addr; endaction;
    method arprot(prot)   = action w_arprot <= prot; endaction;
    method arvalid(valid) = action if (valid) enqWire.send; endaction;
    method arready        = ff.notFull;

  endmodule
endinstance
