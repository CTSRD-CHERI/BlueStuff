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

////////////////////////////
// AXI Write Data Channel //
////////////////////////////////////////////////////////////////////////////////

// typeclasses to convert to/from the flit type

typeclass ToAXI4Lite_WFlit#(type t, numeric type data_, numeric type user_);
  function AXI4Lite_WFlit#(data_, user_) toAXI4Lite_WFlit (t x);
endtypeclass

instance ToAXI4Lite_WFlit#(AXI4Lite_WFlit#(a, b), a, b);
  function toAXI4Lite_WFlit = id;
endinstance

typeclass FromAXI4Lite_WFlit#(type t, numeric type data_, numeric type user_);
  function t fromAXI4Lite_WFlit (AXI4Lite_WFlit#(data_, user_) x);
endtypeclass

instance FromAXI4Lite_WFlit#(AXI4Lite_WFlit#(a, b), a, b);
  function fromAXI4Lite_WFlit = id;
endinstance

// convert to/from Synth Master interface
////////////////////////////////////////////////////////////////////////////////

function AXI4Lite_W_Master_Synth#(data_, user_)
  toAXI4Lite_W_Master_Synth(src_t#(t) s)
  provisos (ToSource#(src_t#(t), t), ToAXI4Lite_WFlit#(t, data_, user_));
  let src = toSource(s);
  AXI4Lite_WFlit#(data_, user_) flit = toAXI4Lite_WFlit(src.peek);
  return interface AXI4Lite_W_Master_Synth;
    method wdata  = flit.wdata;
    method wstrb  = flit.wstrb;
    method wuser  = flit.wuser;
    method wvalid = src.canPeek;
    method wready(rdy) = action if (src.canPeek && rdy) src.drop; endaction;
  endinterface;
endfunction

function Source#(AXI4Lite_WFlit#(data_, user_))
  fromAXI4Lite_W_Master_Synth(AXI4Lite_W_Master_Synth#(data_, user_) m) =
  interface Source;
    method canPeek = m.wvalid;
    method peek = AXI4Lite_WFlit {
      wdata: m.wdata, wstrb: m.wstrb, wuser: m.wuser
    };
    method drop = action if (m.wvalid) m.wready(True); endaction;
  endinterface;

// convert to/from Synth Slave interface
////////////////////////////////////////////////////////////////////////////////

function AXI4Lite_W_Slave_Synth#(data_, user_)
  toAXI4Lite_W_Slave_Synth(snk_t s)
  provisos (ToSink#(snk_t, t), FromAXI4Lite_WFlit#(t, data_, user_)) =
  interface AXI4Lite_W_Slave_Synth;
    method wflit(wdata, wstrb, wuser) = toSink(s).put(fromAXI4Lite_WFlit(
      AXI4Lite_WFlit{ wdata: wdata, wstrb: wstrb, wuser: wuser }
    ));
    method wready = toSink(s).canPut;
  endinterface;

function Sink#(AXI4Lite_WFlit#(data_, user_))
  fromAXI4Lite_W_Slave_Synth(AXI4Lite_W_Slave_Synth#(data_, user_) s) =
  interface Sink;
    method canPut = s.wready;
    method put(x) = s.wflit(x.wdata, x.wstrb, x.wuser);
  endinterface;
