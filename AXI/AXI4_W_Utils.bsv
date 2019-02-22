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

////////////////////////////
// AXI Write Data Channel //
////////////////////////////////////////////////////////////////////////////////

// typeclasses to convert to/from the flit type

typeclass ToAXI4_WFlit#(type t, numeric type data_, numeric type user_);
  function AXI4_WFlit#(data_, user_) toAXI4_WFlit (t x);
endtypeclass

instance ToAXI4_WFlit#(AXI4_WFlit#(a, b), a, b);
  function toAXI4_WFlit = id;
endinstance

typeclass FromAXI4_WFlit#(type t,
numeric type data_, numeric type user_);
  function t fromAXI4_WFlit (AXI4_WFlit#(data_, user_) x);
endtypeclass

instance FromAXI4_WFlit#(AXI4_WFlit#(a, b), a, b);
  function fromAXI4_WFlit = id;
endinstance

// convert to/from Synth Master interface
////////////////////////////////////////////////////////////////////////////////

function AXI4_W_Master_Synth#(data_, user_)
  toAXI4_W_Master_Synth(src_t#(t) s)
  provisos (ToSource#(src_t#(t), t), ToAXI4_WFlit#(t, data_, user_));
  let src = toSource(s);
  AXI4_WFlit#(data_, user_) flit = toAXI4_WFlit(src.peek);
  return interface AXI4_W_Master_Synth;
    method wdata  = flit.wdata;
    method wstrb  = flit.wstrb;
    method wlast  = flit.wlast;
    method wuser  = flit.wuser;
    method wvalid = src.canPeek;
    method wready(rdy) = action if (src.canPeek && rdy) src.drop; endaction;
  endinterface;
endfunction

function Source#(AXI4_WFlit#(data_, user_))
  fromAXI4_W_Master_Synth(AXI4_W_Master_Synth#(data_, user_) m) =
  interface Source;
    method canPeek = m.wvalid;
    method peek = AXI4_WFlit {
      wdata: m.wdata, wstrb: m.wstrb, wlast: m.wlast, wuser: m.wuser
    };
    method drop = action if (m.wvalid) m.wready(True); endaction;
  endinterface;

// convert to/from Synth Slave interface
////////////////////////////////////////////////////////////////////////////////

function AXI4_W_Slave_Synth#(data_, user_)
  toAXI4_W_Slave_Synth(snk_t s)
  provisos (ToSink#(snk_t, t), FromAXI4_WFlit#(t, data_, user_)) =
  interface AXI4_W_Slave_Synth;
    method wflit(wdata, wstrb, wlast, wuser) =
      toSink(s).put(fromAXI4_WFlit(AXI4_WFlit{
        wdata: wdata, wstrb: wstrb, wlast: wlast, wuser: wuser
      }));
    method wready = toSink(s).canPut;
  endinterface;

function Sink#(AXI4_WFlit#(data_, user_))
  fromAXI4_W_Slave_Synth(AXI4_W_Slave_Synth#(data_, user_) s) =
  interface Sink;
    method canPut = s.wready;
    method put(x) = s.wflit(x.wdata, x.wstrb, x.wlast, x.wuser);
  endinterface;
