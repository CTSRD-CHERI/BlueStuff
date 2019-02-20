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

import AXI4_Types :: *;

import FIFOF :: *;
import SpecialFIFOs :: *;

////////////////////////////////
// AXI Write Response Channel //
////////////////////////////////////////////////////////////////////////////////

// typeclasses to convert to/from the flit type

typeclass ToAXI4_BFlit#(type t, numeric type id_, numeric type user_);
  function AXI4_BFlit#(id_, user_) toAXI4_BFlit (t x);
endtypeclass

instance ToAXI4_BFlit#(AXI4_BFlit#(a, b), a, b);
  function toAXI4_BFlit = id;
endinstance

typeclass FromAXI4_BFlit#(type t, numeric type id_, numeric type user_);
  function t fromAXI4_BFlit (AXI4_BFlit#(id_, user_) x);
endtypeclass

instance FromAXI4_BFlit#(AXI4_BFlit#(a, b), a, b);
  function fromAXI4_BFlit = id;
endinstance

// convert to/from Synth Master interface
////////////////////////////////////////////////////////////////////////////////

typeclass ToAXI4_B_Master_Synth#(type t);
  module toAXI4_B_Master_Synth#(t#(x) ifc) (AXI4_B_Master_Synth#(id_, user_))
  provisos (FromAXI4_BFlit#(x, id_, user_));
endtypeclass

instance ToAXI4_B_Master_Synth#(Sink);
  module toAXI4_B_Master_Synth#(Sink#(t) snk)
  (AXI4_B_Master_Synth#(id_, user_)) provisos (FromAXI4_BFlit#(t, id_, user_));

    let w_bid   <- mkDWire(?);
    let w_bresp <- mkDWire(?);
    let w_buser <- mkDWire(?);
    PulseWire putWire <- mkPulseWire;
    rule doPut (putWire && snk.canPut);
      snk.put(fromAXI4_BFlit(AXI4_BFlit{
        bid: w_bid, bresp: w_bresp, buser: w_buser
      }));
    endrule

    method bid(id)       = action w_bid   <= id; endaction;
    method bresp(resp)   = action w_bresp <= resp; endaction;
    method buser(user)   = action w_buser <= user; endaction;
    method bvalid(valid) = action if (valid) putWire.send; endaction;
    method bready        = snk.canPut;

  endmodule
endinstance

instance ToAXI4_B_Master_Synth#(FIFOF);
  module toAXI4_B_Master_Synth#(FIFOF#(t) ff)
  (AXI4_B_Master_Synth#(id_, user_)) provisos (FromAXI4_BFlit#(t, id_, user_));

    let w_bid   <- mkDWire(?);
    let w_bresp <- mkDWire(?);
    let w_buser <- mkDWire(?);
    PulseWire enqWire <- mkPulseWire;
    rule doEnq (enqWire && ff.notFull);
      ff.enq(fromAXI4_BFlit(AXI4_BFlit{
        bid: w_bid, bresp: w_bresp, buser: w_buser
      }));
    endrule

    method bid(id)       = action w_bid   <= id; endaction;
    method bresp(resp)   = action w_bresp <= resp; endaction;
    method buser(user)   = action w_buser <= user; endaction;
    method bvalid(valid) = action if (valid) enqWire.send; endaction;
    method bready        = ff.notFull;

  endmodule
endinstance

module fromAXI4_B_Master_Synth#(AXI4_B_Master_Synth#(id_, user_) m)
  (Sink#(AXI4_BFlit#(id_, user_)));

  method canPut = m.bready;
  method put(x) if (m.bready) = action
    m.bid(x.bid);
    m.bresp(x.bresp);
    m.buser(x.buser);
    m.bvalid(True);
  endaction;

endmodule

// convert to/from Synth Slave interface
////////////////////////////////////////////////////////////////////////////////

typeclass ToAXI4_B_Slave_Synth#(type t);
  module toAXI4_B_Slave_Synth#(t#(x) ifc) (AXI4_B_Slave_Synth#(id_, user_))
  provisos (ToAXI4_BFlit#(x, id_, user_));
endtypeclass

instance ToAXI4_B_Slave_Synth#(Source);
  module toAXI4_B_Slave_Synth#(Source#(t) src)
  (AXI4_B_Slave_Synth#(id_, user_)) provisos (ToAXI4_BFlit#(t, id_, user_));

    Wire#(AXI4_BFlit#(id_, user_)) flit <- mkDWire(?);
    rule peekFlit (src.canPeek); flit <= toAXI4_BFlit(src.peek); endrule
    PulseWire dropWire <- mkPulseWire;
    rule doDrop (dropWire && src.canPeek); src.drop; endrule

    method bid    = flit.bid;
    method bresp  = flit.bresp;
    method buser  = flit.buser;
    method bvalid = src.canPeek;
    method bready(rdy) = action if (rdy) dropWire.send; endaction;

  endmodule
endinstance

instance ToAXI4_B_Slave_Synth#(FIFOF);
  module toAXI4_B_Slave_Synth#(FIFOF#(t) ff)
  (AXI4_B_Slave_Synth#(id_, user_)) provisos (ToAXI4_BFlit#(t, id_, user_));

    Wire#(AXI4_BFlit#(id_, user_)) flit <- mkDWire(?);
    rule peekFlit (ff.notEmpty); flit <= toAXI4_BFlit(ff.first); endrule
    PulseWire deqWire <- mkPulseWire;
    rule doDeq (deqWire && ff.notEmpty); ff.deq; endrule

    method bid    = flit.bid;
    method bresp  = flit.bresp;
    method buser  = flit.buser;
    method bvalid = ff.notEmpty;
    method bready(rdy) = action if (rdy) deqWire.send; endaction;

  endmodule
endinstance

module fromAXI4_B_Slave_Synth#(AXI4_B_Slave_Synth#(id_, user_) s)
  (Source#(AXI4_BFlit#(id_, user_)));

  method canPeek = s.bvalid;
  method peek = AXI4_BFlit {
    bid: s.bid, bresp: s.bresp, buser: s.buser
  };
  method drop if (s.bvalid) = s.bready(True);

endmodule
