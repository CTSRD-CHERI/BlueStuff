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

import SourceSink :: *;

import AXI4Lite_Types :: *;

import FIFOF :: *;
import SpecialFIFOs :: *;

///////////////////////////////
// AXI Address Write Channel //
////////////////////////////////////////////////////////////////////////////////

// typeclasses to convert to/from the flit type

typeclass ToAXIAWLiteFlit#(type t, numeric type addr_, numeric type user_);
  function AWLiteFlit#(addr_, user_) toAXIAWLiteFlit (t x);
endtypeclass

instance ToAXIAWLiteFlit#(AWLiteFlit#(a, b), a, b);
  function toAXIAWLiteFlit = id;
endinstance

typeclass FromAXIAWLiteFlit#(type t, numeric type addr_, numeric type user_);
  function t fromAXIAWLiteFlit (AWLiteFlit#(addr_, user_) x);
endtypeclass

instance FromAXIAWLiteFlit#(AWLiteFlit#(a, b), a, b);
  function fromAXIAWLiteFlit = id;
endinstance

// typeclass to turn an interface to the Master interface

typeclass ToAXIAWLiteMaster#(type t);
  module toAXIAWLiteMaster#(t#(x) ifc) (AWLiteMaster#(addr_, user_))
  provisos (ToAXIAWLiteFlit#(x, addr_, user_));
endtypeclass

instance ToAXIAWLiteMaster#(Source);
  module toAXIAWLiteMaster#(Source#(t) src)
  (AWLiteMaster#(addr_, user_)) provisos (ToAXIAWLiteFlit#(t, addr_, user_));

    Wire#(AWLiteFlit#(addr_, user_)) flit <- mkDWire(?);
    rule peekFlit (src.canPeek); flit <= toAXIAWLiteFlit(src.peek); endrule
    PulseWire dropWire <- mkPulseWire;
    rule doDrop (dropWire && src.canPeek); src.drop; endrule

    method awaddr  = flit.awaddr;
    method awprot  = flit.awprot;
    method awuser  = flit.awuser;
    method awvalid = src.canPeek;
    method awready(rdy) = action if (rdy) dropWire.send; endaction;

  endmodule
endinstance

instance ToAXIAWLiteMaster#(FIFOF);
  module toAXIAWLiteMaster#(FIFOF#(t) ff)
  (AWLiteMaster#(addr_, user_)) provisos (ToAXIAWLiteFlit#(t, addr_, user_));

    Wire#(AWLiteFlit#(addr_, user_)) flit <- mkDWire(?);
    rule peekFlit (ff.notEmpty); flit <= toAXIAWLiteFlit(ff.first); endrule
    PulseWire deqWire <- mkPulseWire;
    rule doDeq (deqWire && ff.notEmpty); ff.deq; endrule

    method awaddr  = flit.awaddr;
    method awprot  = flit.awprot;
    method awuser  = flit.awuser;
    method awvalid = ff.notEmpty;
    method awready(rdy) = action if (rdy) deqWire.send; endaction;

  endmodule
endinstance

// typeclass to turn an interface to the Slave interface

typeclass ToAXIAWLiteSlave#(type t);
  module toAXIAWLiteSlave#(t#(x) ifc) (AWLiteSlave#(addr_, user_))
  provisos (FromAXIAWLiteFlit#(x, addr_, user_));
endtypeclass

instance ToAXIAWLiteSlave#(Sink);
  module toAXIAWLiteSlave#(Sink#(t) snk)
  (AWLiteSlave#(addr_, user_)) provisos (FromAXIAWLiteFlit#(t, addr_, user_));

    let w_awaddr <- mkDWire(?);
    let w_awprot <- mkDWire(?);
    let w_awuser <- mkDWire(?);
    PulseWire putWire <- mkPulseWire;
    rule doPut (putWire && snk.canPut);
      snk.put(fromAXIAWLiteFlit(AWLiteFlit{
        awaddr:   w_awaddr,
        awprot:   w_awprot,
        awuser:   w_awuser
      }));
    endrule

    method awaddr(addr)   = action w_awaddr <= addr; endaction;
    method awprot(prot)   = action w_awprot <= prot; endaction;
    method awuser(user)   = action w_awuser <= user; endaction;
    method awvalid(valid) = action if (valid) putWire.send; endaction;
    method awready        = snk.canPut;

  endmodule
endinstance

instance ToAXIAWLiteSlave#(FIFOF);
  module toAXIAWLiteSlave#(FIFOF#(t) ff)
  (AWLiteSlave#(addr_, user_)) provisos (FromAXIAWLiteFlit#(t, addr_, user_));

    let w_awaddr <- mkDWire(?);
    let w_awprot <- mkDWire(?);
    let w_awuser <- mkDWire(?);
    PulseWire enqWire <- mkPulseWire;
    rule doEnq (enqWire && ff.notFull);
      ff.enq(fromAXIAWLiteFlit(AWLiteFlit{
        awaddr:   w_awaddr,
        awprot:   w_awprot,
        awuser:   w_awuser
      }));
    endrule

    method awaddr(addr)   = action w_awaddr <= addr; endaction;
    method awprot(prot)   = action w_awprot <= prot; endaction;
    method awuser(user)   = action w_awuser <= user; endaction;
    method awvalid(valid) = action if (valid) enqWire.send; endaction;
    method awready        = ff.notFull;

  endmodule
endinstance
