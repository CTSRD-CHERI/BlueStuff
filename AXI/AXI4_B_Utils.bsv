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

function AXI4_B_Master_Synth#(id_, user_)
  toAXI4_B_Master_Synth(snk_t m)
  provisos (ToSink#(snk_t, t), FromAXI4_BFlit#(t, id_, user_)) =
  interface AXI4_B_Master_Synth;
    method bflit(bid, bresp, buser) = action
      if (toSink(m).canPut) toSink(m).put(fromAXI4_BFlit(AXI4_BFlit{
        bid: bid, bresp: bresp, buser: buser
      }));
    endaction;
    method bready = toSink(m).canPut;
  endinterface;

module fromAXI4_B_Master_Synth#(AXI4_B_Master_Synth#(id_, user_) m) (Sink#(AXI4_BFlit#(id_, user_)));
  //We rely on the buffer being guarded
  FIFOF#(AXI4_BFlit#(id_, user_)) buffer <- mkSizedBypassFIFOF(1);
  rule forwardFlit;
    m.bflit(buffer.first.bid,
            buffer.first.bresp,
            buffer.first.buser);
  endrule
  //dropFlit only fires when both ready and canPeek, i.e. there is something to drop
  rule dropFlit (m.bready);
    buffer.deq;
  endrule
  return toSink(buffer);
endmodule

// convert to/from Synth Slave interface
////////////////////////////////////////////////////////////////////////////////

function AXI4_B_Slave_Synth#(id_, user_)
  toAXI4_B_Slave_Synth(src_t#(t) s)
  provisos (ToSource#(src_t#(t), t), ToAXI4_BFlit#(t, id_, user_));
  let src = toSource(s);
  AXI4_BFlit#(id_, user_) flit = toAXI4_BFlit(src.peek);
  return interface AXI4_B_Slave_Synth;
    method bid    = flit.bid;
    method bresp  = flit.bresp;
    method buser  = flit.buser;
    method bvalid = src.canPeek;
    method bready(rdy) = action if (src.canPeek && rdy) src.drop; endaction;
  endinterface;
endfunction

module fromAXI4_B_Slave_Synth#(AXI4_B_Slave_Synth#(id_, user_) s) (Source#(AXI4_BFlit#(id_, user_)));
  let dwReady <- mkDWire(False);
  rule forwardReady;
    s.bready(dwReady);
  endrule
  return interface Source;
    method canPeek = s.bvalid;
    method peek if (s.bvalid) = AXI4_BFlit {
      bid:     s.bid,
      bresp:   s.bresp,
      buser:   s.buser
    };
    method drop if (s.bvalid) = dwReady._write(True);
  endinterface;
endmodule
