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

////////////////////////////////
// AXI Write Response Channel //
////////////////////////////////////////////////////////////////////////////////

// typeclasses to convert to/from the flit type

typeclass ToAXI4Lite_BFlit#(type t, numeric type user_);
  function AXI4Lite_BFlit#(user_) toAXI4Lite_BFlit (t x);
endtypeclass

instance ToAXI4Lite_BFlit#(AXI4Lite_BFlit#(user_), user_);
  function toAXI4Lite_BFlit = id;
endinstance

typeclass FromAXI4Lite_BFlit#(type t, numeric type user_);
  function t fromAXI4Lite_BFlit (AXI4Lite_BFlit#(user_) x);
endtypeclass

instance FromAXI4Lite_BFlit#(AXI4Lite_BFlit#(user_), user_);
  function fromAXI4Lite_BFlit = id;
endinstance

// convert to/from Synth Master interface
////////////////////////////////////////////////////////////////////////////////

function AXI4Lite_B_Master_Synth#(user_)
  toAXI4Lite_B_Master_Synth(snk_t s)
  provisos (ToSink#(snk_t, t), FromAXI4Lite_BFlit#(t, user_)) =
  interface AXI4Lite_B_Master_Synth;
    //method bflit(bid, bresp, buser) if (toSink(s).canPut) =
    method bflit(bresp, buser) = toSink(s).put(fromAXI4Lite_BFlit(
      AXI4Lite_BFlit{ bresp: bresp, buser: buser }
    ));
    method bready = toSink(s).canPut;
  endinterface;

function Sink#(AXI4Lite_BFlit#(user_))
  fromAXI4Lite_B_Master_Synth(AXI4Lite_B_Master_Synth#(user_) m) =
  interface Sink;
    method canPut = m.bready;
    method put(x) = m.bflit(x.bresp, x.buser);
  endinterface;

// convert to/from Synth Slave interface
////////////////////////////////////////////////////////////////////////////////

function AXI4Lite_B_Slave_Synth#(user_)
  toAXI4Lite_B_Slave_Synth(src_t#(t) s)
  provisos (ToSource#(src_t#(t), t), ToAXI4Lite_BFlit#(t, user_));
  let src = toSource(s);
  AXI4Lite_BFlit#(user_) flit = toAXI4Lite_BFlit(src.peek);
  return interface AXI4Lite_B_Slave_Synth;
    method bresp  = flit.bresp;
    method buser  = flit.buser;
    method bvalid = src.canPeek;
    method bready(rdy) = action if (src.canPeek && rdy) src.drop; endaction;
  endinterface;
endfunction

function Source#(AXI4Lite_BFlit#(user_))
  fromAXI4Lite_B_Slave_Synth(AXI4Lite_B_Slave_Synth#(user_) s) =
  interface Source;
    method canPeek = s.bvalid;
    method peek = AXI4Lite_BFlit {bresp: s.bresp, buser: s.buser};
    method drop = action if (s.bvalid) s.bready(True); endaction;
  endinterface;
