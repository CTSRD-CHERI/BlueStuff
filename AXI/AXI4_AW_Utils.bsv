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

///////////////////////////////
// AXI Address Write Channel //
////////////////////////////////////////////////////////////////////////////////

// to convert to/from the flit type
////////////////////////////////////////////////////////////////////////////////

typeclass ToAXI4_AWFlit#(type t,
numeric type id_, numeric type addr_, numeric type user_);
  function AXI4_AWFlit#(id_, addr_, user_) toAXI4_AWFlit (t x);
endtypeclass

instance ToAXI4_AWFlit#(AXI4_AWFlit#(a, b, c), a, b, c);
  function toAXI4_AWFlit = id;
endinstance

typeclass FromAXI4_AWFlit#(type t,
numeric type id_, numeric type addr_, numeric type user_);
  function t fromAXI4_AWFlit (AXI4_AWFlit#(id_, addr_, user_) x);
endtypeclass

instance FromAXI4_AWFlit#(AXI4_AWFlit#(a, b, c), a, b, c);
  function fromAXI4_AWFlit = id;
endinstance

// convert to/from Synth Master interface
////////////////////////////////////////////////////////////////////////////////

function AXI4_AW_Master_Synth#(id_, addr_, user_)
  toAXI4_AW_Master_Synth(src_t#(t) s)
  provisos (ToSource#(src_t#(t), t), ToAXI4_AWFlit#(t, id_, addr_, user_));
  let src = toSource(s);
  AXI4_AWFlit#(id_, addr_, user_) flit = toAXI4_AWFlit(src.peek);
  return interface AXI4_AW_Master_Synth;
    method awid     = flit.awid;
    method awaddr   = flit.awaddr;
    method awlen    = flit.awlen;
    method awsize   = flit.awsize;
    method awburst  = flit.awburst;
    method awlock   = flit.awlock;
    method awcache  = flit.awcache;
    method awprot   = flit.awprot;
    method awqos    = flit.awqos;
    method awregion = flit.awregion;
    method awuser   = flit.awuser;
    method awvalid  = src.canPeek;
    method awready(rdy) = action if (src.canPeek && rdy) src.drop; endaction;
  endinterface;
endfunction

module fromAXI4_AW_Master_Synth#(AXI4_AW_Master_Synth#(id_, addr_, user_) m) (Source#(AXI4_AWFlit#(id_, addr_, user_)));
  let dwReady <- mkDWire(False);
  rule forwardReady;
    m.awready(dwReady);
  endrule
  return interface Source;
    method canPeek = m.awvalid;
    method peek if (m.awvalid) = AXI4_AWFlit {
      awid:     m.awid,
      awaddr:   m.awaddr,
      awlen:    m.awlen,
      awsize:   m.awsize,
      awburst:  m.awburst,
      awlock:   m.awlock,
      awcache:  m.awcache,
      awprot:   m.awprot,
      awqos:    m.awqos,
      awregion: m.awregion,
      awuser:   m.awuser
    };
    method drop if (m.awvalid) = dwReady._write(True);
  endinterface;
endmodule

// convert to/from Synth Slave interface
////////////////////////////////////////////////////////////////////////////////

function AXI4_AW_Slave_Synth#(id_, addr_, user_)
  toAXI4_AW_Slave_Synth(snk_t s)
  provisos (ToSink#(snk_t, t), FromAXI4_AWFlit#(t, id_, addr_, user_)) =
  interface AXI4_AW_Slave_Synth;
    method awflit(awid,
                  awaddr,
                  awlen,
                  awsize,
                  awburst,
                  awlock,
                  awcache,
                  awprot,
                  awqos,
                  awregion,
                  awuser) = action if (toSink(s).canPut) toSink(s).put(fromAXI4_AWFlit(AXI4_AWFlit{
        awid:     awid,
        awaddr:   awaddr,
        awlen:    awlen,
        awsize:   awsize,
        awburst:  awburst,
        awlock:   awlock,
        awcache:  awcache,
        awprot:   awprot,
        awqos:    awqos,
        awregion: awregion,
        awuser:   awuser
      }));
    endaction;
    method awready = toSink(s).canPut;
  endinterface;

module fromAXI4_AW_Slave_Synth#(AXI4_AW_Slave_Synth#(id_, addr_, user_) s) (Sink#(AXI4_AWFlit#(id_, addr_, user_)));
  //We rely on the buffer being guarded
  FIFOF#(AXI4_AWFlit#(id_, addr_, user_)) buffer <- mkSizedBypassFIFOF(1);
  rule forwardFlit;
    s.awflit(buffer.first.awid,
             buffer.first.awaddr,
             buffer.first.awlen,
             buffer.first.awsize,
             buffer.first.awburst,
             buffer.first.awlock,
             buffer.first.awcache,
             buffer.first.awprot,
             buffer.first.awqos,
             buffer.first.awregion,
             buffer.first.awuser);
  endrule
  //dropFlit only fires when both ready and canPeek, i.e. there is something to drop
  rule dropFlit (s.awready);
    buffer.deq;
  endrule
  return toSink(buffer);
endmodule
