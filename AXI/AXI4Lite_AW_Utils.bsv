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

typeclass ToAXI4Lite_AWFlit#(type t, numeric type addr_, numeric type user_);
  function AXI4Lite_AWFlit#(addr_, user_) toAXI4Lite_AWFlit (t x);
endtypeclass

instance ToAXI4Lite_AWFlit#(AXI4Lite_AWFlit#(a, b), a, b);
  function toAXI4Lite_AWFlit = id;
endinstance

typeclass FromAXI4Lite_AWFlit#(type t, numeric type addr_, numeric type user_);
  function t fromAXI4Lite_AWFlit (AXI4Lite_AWFlit#(addr_, user_) x);
endtypeclass

instance FromAXI4Lite_AWFlit#(AXI4Lite_AWFlit#(a, b), a, b);
  function fromAXI4Lite_AWFlit = id;
endinstance

// convert to/from Synth Master interface
////////////////////////////////////////////////////////////////////////////////

function AXI4Lite_AW_Master_Synth#(addr_, user_)
  toAXI4Lite_AW_Master_Synth(src_t#(t) s)
  provisos (ToSource#(src_t#(t), t), ToAXI4Lite_AWFlit#(t, addr_, user_));
  let src = toSource(s);
  AXI4Lite_AWFlit#(addr_, user_) flit = toAXI4Lite_AWFlit(src.peek);
  return interface AXI4Lite_AW_Master_Synth;
    method awaddr   = flit.awaddr;
    method awprot   = flit.awprot;
    method awuser   = flit.awuser;
    method awvalid  = src.canPeek;
    method awready(rdy) = action if (src.canPeek && rdy) src.drop; endaction;
  endinterface;
endfunction

function Source#(AXI4Lite_AWFlit#(addr_, user_))
  fromAXI4Lite_AW_Master_Synth(AXI4Lite_AW_Master_Synth#(addr_, user_) m) =
  interface Source;
    method canPeek = m.awvalid;
    method peek = AXI4Lite_AWFlit {
      awaddr: m.awaddr, awprot: m.awprot, awuser: m.awuser
    };
    method drop = action if (m.awvalid) m.awready(True); endaction;
  endinterface;

// convert to/from Synth Slave interface
////////////////////////////////////////////////////////////////////////////////

function AXI4Lite_AW_Slave_Synth#(addr_, user_)
  toAXI4Lite_AW_Slave_Synth(snk_t s)
  provisos (ToSink#(snk_t, t), FromAXI4Lite_AWFlit#(t, addr_, user_)) =
  interface AXI4Lite_AW_Slave_Synth;
    method awflit(awaddr, awprot, awuser) = toSink(s).put(fromAXI4Lite_AWFlit(
      AXI4Lite_AWFlit{awaddr: awaddr, awprot: awprot, awuser: awuser}
    ));
    method awready = toSink(s).canPut;
  endinterface;

function Sink#(AXI4Lite_AWFlit#(addr_, user_))
  fromAXI4Lite_AW_Slave_Synth(AXI4Lite_AW_Slave_Synth#(addr_, user_) s) =
  interface Sink;
    method canPut = s.awready;
    method put(x) = s.awflit(x.awaddr, x.awprot, x.awuser);
  endinterface;
