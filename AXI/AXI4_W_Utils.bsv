/*-
 * Copyright (c) 2018-2020 Alexandre Joannou
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

typeclass FromAXI4_WFlit#(type t, numeric type data_, numeric type user_);
  function t fromAXI4_WFlit (AXI4_WFlit#(data_, user_) x);
endtypeclass

instance FromAXI4_WFlit#(AXI4_WFlit#(a, b), a, b);
  function fromAXI4_WFlit = id;
endinstance

// convert to/from Synth Master interface
////////////////////////////////////////////////////////////////////////////////

module toAXI4_W_Master_Synth #(src_t#(t) s) (AXI4_W_Master_Synth#(data_, user_))
  provisos ( ToSource#(src_t#(t), t)
           , ToAXI4_WFlit#(t, data_, user_)
           , Bits#(t, t_sz));
  let src <- toUnguardedSource(s, ?);
  AXI4_WFlit#(data_, user_) flit = toAXI4_WFlit(src.peek);
  method wdata  = flit.wdata;
  method wstrb  = flit.wstrb;
  method wlast  = flit.wlast;
  method wuser  = flit.wuser;
  method wvalid = src.canPeek;
  method wready(rdy) = action if (src.canPeek && rdy) src.drop; endaction;
endmodule

module fromAXI4_W_Master_Synth #(AXI4_W_Master_Synth#(data_, user_) m)
                                (Source#(AXI4_WFlit#(data_, user_)));
  FIFOF#(AXI4_WFlit#(data_, user_)) buffer <- mkSizedBypassFIFOF(1);
  let snk <- toUnguardedSink(buffer);
  (* fire_when_enabled, no_implicit_conditions *)
  rule forwardFlit (m.wvalid && snk.canPut);
    snk.put (AXI4_WFlit {
      wdata: m.wdata, wstrb: m.wstrb, wlast: m.wlast, wuser: m.wuser
    });
  endrule
  (* fire_when_enabled, no_implicit_conditions *)
  rule forwardReady; m.wready(snk.canPut); endrule
  return toSource(buffer);
endmodule

// convert to/from Synth Slave interface
////////////////////////////////////////////////////////////////////////////////

module toAXI4_W_Slave_Synth #(snk_t s)
                             (AXI4_W_Slave_Synth#(data_, user_))
  provisos ( ToSink#(snk_t, t)
           , FromAXI4_WFlit#(t, data_, user_)
           , Bits#(t, t_sz));
  let snk <- toUnguardedSink(s);
  method wflit(wvalid, wdata, wstrb, wlast, wuser) = action
    if (wvalid && snk.canPut) snk.put(fromAXI4_WFlit(AXI4_WFlit{
      wdata: wdata, wstrb: wstrb, wlast: wlast, wuser: wuser
    }));
  endaction;
  method wready = snk.canPut;
endmodule

module fromAXI4_W_Slave_Synth #(AXI4_W_Slave_Synth#(data_, user_) s)
                               (Sink#(AXI4_WFlit#(data_, user_)));
  // We use a guarded buffer to export as a guarded sink, and use an unguarded
  // source as an internal interface to it for connection to the Synth interface
  FIFOF#(AXI4_WFlit#(data_, user_)) buffer <- mkSizedBypassFIFOF(1);
  let src <- toUnguardedSource(buffer, ?);
  (* fire_when_enabled, no_implicit_conditions *)
  rule forwardFlit;
    s.wflit( src.canPeek
           , src.peek.wdata
           , src.peek.wstrb
           , src.peek.wlast
           , src.peek.wuser);
  endrule
  (* fire_when_enabled, no_implicit_conditions *)
  rule dropFlit (src.canPeek && s.wready); src.drop; endrule
  return toSink(buffer);
endmodule
