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

import AXI4Lite_Types :: *;

import FIFOF :: *;
import SpecialFIFOs :: *;

//////////////////////////////
// AXI Read Address Channel //
////////////////////////////////////////////////////////////////////////////////

// typeclasses to convert to/from the flit type

typeclass ToAXI4Lite_ARFlit#(type t, numeric type addr_, numeric type user_);
  function AXI4Lite_ARFlit#(addr_, user_) toAXI4Lite_ARFlit (t x);
endtypeclass

instance ToAXI4Lite_ARFlit#(AXI4Lite_ARFlit#(a, b), a, b);
  function toAXI4Lite_ARFlit = id;
endinstance

typeclass FromAXI4Lite_ARFlit#(type t, numeric type addr_, numeric type user_);
  function t fromAXI4Lite_ARFlit (AXI4Lite_ARFlit#(addr_, user_) x);
endtypeclass

instance FromAXI4Lite_ARFlit#(AXI4Lite_ARFlit#(a, b), a, b);
  function fromAXI4Lite_ARFlit = id;
endinstance

// convert to/from Synth Master interface
////////////////////////////////////////////////////////////////////////////////

module toAXI4Lite_AR_Master_Synth #(src_t#(t) s)
                                   (AXI4Lite_AR_Master_Synth#(addr_, user_))
  provisos ( ToSource#(src_t#(t), t)
           , ToAXI4Lite_ARFlit#(t, addr_, user_)
           , Bits#(t, t_sz));
  let src <- toUnguardedSource(s, ?);
  AXI4Lite_ARFlit#(addr_, user_) flit = toAXI4Lite_ARFlit(src.peek);
  method araddr   = flit.araddr;
  method arprot   = flit.arprot;
  method aruser   = flit.aruser;
  method arvalid  = src.canPeek;
  method arready(rdy) = action if (src.canPeek && rdy) src.drop; endaction;
endmodule

module fromAXI4Lite_AR_Master_Synth #(AXI4Lite_AR_Master_Synth#(addr_, user_) m)
                                     (Source#(AXI4Lite_ARFlit#(addr_, user_)));
  FIFOF#(AXI4Lite_ARFlit#(addr_, user_)) buffer <- mkSizedBypassFIFOF(1);
  let snk <- toUnguardedSink(buffer);
  (* fire_when_enabled, no_implicit_conditions *)
  rule forwardFlit (m.arvalid && snk.canPut);
    snk.put (AXI4Lite_ARFlit { araddr: m.araddr
                             , arprot: m.arprot
                             , aruser: m.aruser });
  endrule
  (* fire_when_enabled, no_implicit_conditions *)
  rule forwardReady; m.arready(snk.canPut); endrule
  return toSource(buffer);
endmodule

// convert to/from Synth Slave interface
////////////////////////////////////////////////////////////////////////////////

module toAXI4Lite_AR_Slave_Synth #(snk_t s)
                                  (AXI4Lite_AR_Slave_Synth#(addr_, user_))
  provisos ( ToSink#(snk_t, t)
           , FromAXI4Lite_ARFlit#(t, addr_, user_)
           , Bits#(t, t_sz));
  let snk <- toUnguardedSink(s);
  method arflit(arvalid, araddr, arprot, aruser) = action
    if (arvalid && snk.canPut) snk.put(fromAXI4Lite_ARFlit(
      AXI4Lite_ARFlit{ araddr: araddr, arprot: arprot, aruser: aruser }
    ));
  endaction;
  method arready = snk.canPut;
endmodule

module fromAXI4Lite_AR_Slave_Synth #(AXI4Lite_AR_Slave_Synth#(addr_, user_) s)
                                    (Sink#(AXI4Lite_ARFlit#(addr_, user_)));
  // We use a guarded buffer to export as a guarded sink, and use an unguarded
  // source as an internal interface to it for connection to the Synth interface
  FIFOF#(AXI4Lite_ARFlit#(addr_, user_)) buffer <- mkSizedBypassFIFOF(1);
  let src <- toUnguardedSource(buffer, ?);
  (* fire_when_enabled, no_implicit_conditions *)
  rule forwardFlit;
    s.arflit( src.canPeek
            , src.peek.araddr
            , src.peek.arprot
            , src.peek.aruser);
  endrule
  (* fire_when_enabled, no_implicit_conditions *)
  rule dropFlit (src.canPeek && s.arready); src.drop; endrule
  return toSink(buffer);
endmodule
