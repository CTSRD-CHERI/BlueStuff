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

///////////////////////////
// AXI Read Data Channel //
////////////////////////////////////////////////////////////////////////////////

// typeclasses to convert to/from the flit type

typeclass ToAXI4_RFlit#(type t,
numeric type id_, numeric type data_, numeric type user_);
  function AXI4_RFlit#(id_, data_, user_) toAXI4_RFlit (t x);
endtypeclass

instance ToAXI4_RFlit#(AXI4_RFlit#(a, b, c), a, b, c);
  function toAXI4_RFlit = id;
endinstance

typeclass FromAXI4_RFlit#(type t,
numeric type id_, numeric type data_, numeric type user_);
  function t fromAXI4_RFlit (AXI4_RFlit#(id_, data_, user_) x);
endtypeclass

instance FromAXI4_RFlit#(AXI4_RFlit#(a, b, c), a, b, c);
  function fromAXI4_RFlit = id;
endinstance

// convert to/from Synth Master interface
////////////////////////////////////////////////////////////////////////////////

function AXI4_R_Master_Synth#(id_, data_, user_)
  toAXI4_R_Master_Synth(snk_t m)
  provisos (ToSink#(snk_t, t), FromAXI4_RFlit#(t, id_, data_, user_)) =
  interface AXI4_R_Master_Synth;
    method rflit(rid, rdata, rresp, rlast, ruser) = action
      if (toSink(m).canPut) toSink(m).put(fromAXI4_RFlit(AXI4_RFlit{
        rid: rid, rdata: rdata, rresp: rresp, rlast: rlast, ruser: ruser
      }));
    endaction;
    method rready = toSink(m).canPut;
  endinterface;

module fromAXI4_R_Master_Synth#(AXI4_R_Master_Synth#(id_, data_, user_) m) (Sink#(AXI4_RFlit#(id_, data_, user_)));
  //We rely on the buffer being guarded
  FIFOF#(AXI4_RFlit#(id_, data_, user_)) buffer <- mkSizedBypassFIFOF(1);
  rule forwardFlit;
    m.rflit(buffer.first.rid,
            buffer.first.rdata,
            buffer.first.rresp,
            buffer.first.rlast,
            buffer.first.ruser);
  endrule
  //dropFlit only fires when both ready and canPeek, i.e. there is something to drop
  rule dropFlit (m.rready);
    buffer.deq;
  endrule
  return toSink(buffer);
endmodule

// convert to/from Synth Slave interface
////////////////////////////////////////////////////////////////////////////////

function AXI4_R_Slave_Synth#(id_, data_, user_)
  toAXI4_R_Slave_Synth(src_t#(t) s)
  provisos (ToSource#(src_t#(t), t), ToAXI4_RFlit#(t, id_, data_, user_));
  let src = toSource(s);
  AXI4_RFlit#(id_, data_, user_) flit = toAXI4_RFlit(src.peek);
  return interface AXI4_R_Slave_Synth;
    method rid    = flit.rid;
    method rdata  = flit.rdata;
    method rresp  = flit.rresp;
    method rlast  = flit.rlast;
    method ruser  = flit.ruser;
    method rvalid = src.canPeek;
    method rready(rdy) = action if (src.canPeek && rdy) src.drop; endaction;
  endinterface;
endfunction

module fromAXI4_R_Slave_Synth#(AXI4_R_Slave_Synth#(id_, data_, user_) s) (Source#(AXI4_RFlit#(id_, data_, user_)));
  let dwReady <- mkDWire(False);
  rule forwardReady;
    s.rready(dwReady);
  endrule
  return interface Source;
    method canPeek = s.rvalid;
    method peek if (s.rvalid) = AXI4_RFlit {
      rid:     s.rid,
      rdata:   s.rdata,
      rresp:   s.rresp,
      rlast:   s.rlast,
      ruser:   s.ruser
    };
    method drop if (s.rvalid) = dwReady._write(True);
  endinterface;
endmodule
