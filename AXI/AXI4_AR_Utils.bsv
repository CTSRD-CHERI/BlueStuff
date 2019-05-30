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

//////////////////////////////
// AXI Read Address Channel //
////////////////////////////////////////////////////////////////////////////////

// typeclasses to convert to/from the flit type

typeclass ToAXI4_ARFlit#(type t,
numeric type id_, numeric type addr_, numeric type user_);
  function AXI4_ARFlit#(id_, addr_, user_) toAXI4_ARFlit (t x);
endtypeclass

instance ToAXI4_ARFlit#(AXI4_ARFlit#(a, b, c), a, b, c);
  function toAXI4_ARFlit = id;
endinstance

typeclass FromAXI4_ARFlit#(type t,
numeric type id_, numeric type addr_, numeric type user_);
  function t fromAXI4_ARFlit (AXI4_ARFlit#(id_, addr_, user_) x);
endtypeclass

instance FromAXI4_ARFlit#(AXI4_ARFlit#(a, b, c), a, b, c);
  function fromAXI4_ARFlit = id;
endinstance

// convert to/from Synth Master interface
////////////////////////////////////////////////////////////////////////////////

function AXI4_AR_Master_Synth#(id_, addr_, user_)
  toAXI4_AR_Master_Synth(src_t#(t) s)
  provisos (ToSource#(src_t#(t), t), ToAXI4_ARFlit#(t, id_, addr_, user_));
  let src = toSource(s);
  AXI4_ARFlit#(id_, addr_, user_) flit = toAXI4_ARFlit(src.peek);
  return interface AXI4_AR_Master_Synth;
    method arid     = flit.arid;
    method araddr   = flit.araddr;
    method arlen    = flit.arlen;
    method arsize   = flit.arsize;
    method arburst  = flit.arburst;
    method arlock   = flit.arlock;
    method arcache  = flit.arcache;
    method arprot   = flit.arprot;
    method arqos    = flit.arqos;
    method arregion = flit.arregion;
    method aruser   = flit.aruser;
    method arvalid  = src.canPeek;
    method arready(rdy) = action if (src.canPeek && rdy) src.drop; endaction;
  endinterface;
endfunction

module fromAXI4_AR_Master_Synth#(AXI4_AR_Master_Synth#(id_, addr_, user_) m) (Source#(AXI4_ARFlit#(id_, addr_, user_)));
  let dwReady <- mkDWire(False);
  rule forwardReady;
    m.arready(dwReady);
  endrule
  return interface Source;
    method canPeek = m.arvalid;
    method peek if (m.arvalid) = AXI4_ARFlit {
      arid:     m.arid,
      araddr:   m.araddr,
      arlen:    m.arlen,
      arsize:   m.arsize,
      arburst:  m.arburst,
      arlock:   m.arlock,
      arcache:  m.arcache,
      arprot:   m.arprot,
      arqos:    m.arqos,
      arregion: m.arregion,
      aruser:   m.aruser
    };
    method drop if (m.arvalid) = dwReady._write(True);
  endinterface;
endmodule

// convert to/from Synth Slave interface
////////////////////////////////////////////////////////////////////////////////

function AXI4_AR_Slave_Synth#(id_, addr_, user_)
  toAXI4_AR_Slave_Synth(snk_t s)
  provisos (ToSink#(snk_t, t), FromAXI4_ARFlit#(t, id_, addr_, user_)) =
  interface AXI4_AR_Slave_Synth;
    method arflit(arid,
                  araddr,
                  arlen,
                  arsize,
                  arburst,
                  arlock,
                  arcache,
                  arprot,
                  arqos,
                  arregion,
                  aruser) = action if (toSink(s).canPut) toSink(s).put(fromAXI4_ARFlit(AXI4_ARFlit{
        arid:     arid,
        araddr:   araddr,
        arlen:    arlen,
        arsize:   arsize,
        arburst:  arburst,
        arlock:   arlock,
        arcache:  arcache,
        arprot:   arprot,
        arqos:    arqos,
        arregion: arregion,
        aruser:   aruser
      }));
    endaction;
    method arready = toSink(s).canPut;
  endinterface;

module fromAXI4_AR_Slave_Synth#(AXI4_AR_Slave_Synth#(id_, addr_, user_) s) (Sink#(AXI4_ARFlit#(id_, addr_, user_)));
  //We rely on the buffer being guarded
  FIFOF#(AXI4_ARFlit#(id_, addr_, user_)) buffer <- mkSizedBypassFIFOF(1);
  rule forwardFlit;
    s.arflit(buffer.first.arid,
             buffer.first.araddr,
             buffer.first.arlen,
             buffer.first.arsize,
             buffer.first.arburst,
             buffer.first.arlock,
             buffer.first.arcache,
             buffer.first.arprot,
             buffer.first.arqos,
             buffer.first.arregion,
             buffer.first.aruser);
  endrule
  //dropFlit only fires when both ready and canPeek, i.e. there is something to drop
  rule dropFlit (s.arready);
    buffer.deq;
  endrule
  return toSink(buffer);
endmodule
