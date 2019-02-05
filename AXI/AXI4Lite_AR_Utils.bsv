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

// typeclass to turn an interface to the Master interface

typeclass ToAXI4Lite_AR_Master_Synth#(type t);
  module toAXI4Lite_AR_Master_Synth#(t#(x) ifc)
  (AXI4Lite_AR_Master_Synth#(addr_, user_))
  provisos (ToAXI4Lite_ARFlit#(x, addr_, user_));
endtypeclass

instance ToAXI4Lite_AR_Master_Synth#(Source);
  module toAXI4Lite_AR_Master_Synth#(Source#(t) src)
  (AXI4Lite_AR_Master_Synth#(addr_, user_))
  provisos (ToAXI4Lite_ARFlit#(t, addr_, user_));

    Wire#(AXI4Lite_ARFlit#(addr_, user_)) flit <- mkDWire(?);
    rule peekFlit (src.canPeek); flit <= toAXI4Lite_ARFlit(src.peek); endrule
    PulseWire dropWire <- mkPulseWire;
    rule doDrop (dropWire && src.canPeek); src.drop; endrule

    method araddr   = flit.araddr;
    method arprot   = flit.arprot;
    method aruser   = flit.aruser;
    method arvalid  = src.canPeek;
    method arready(rdy) = action if (rdy) dropWire.send; endaction;

  endmodule
endinstance

instance ToAXI4Lite_AR_Master_Synth#(FIFOF);
  module toAXI4Lite_AR_Master_Synth#(FIFOF#(t) ff)
  (AXI4Lite_AR_Master_Synth#(addr_, user_))
  provisos (ToAXI4Lite_ARFlit#(t, addr_, user_));

    Wire#(AXI4Lite_ARFlit#(addr_, user_)) flit <- mkDWire(?);
    rule peekFlit (ff.notEmpty); flit <= toAXI4Lite_ARFlit(ff.first); endrule
    PulseWire deqWire <- mkPulseWire;
    rule doDeq (deqWire && ff.notEmpty); ff.deq; endrule

    method araddr   = flit.araddr;
    method arprot   = flit.arprot;
    method aruser   = flit.aruser;
    method arvalid  = ff.notEmpty;
    method arready(rdy) = action if (rdy) deqWire.send; endaction;

  endmodule
endinstance

// typeclass to turn an interface to the Slave interface

typeclass ToAXI4Lite_AR_Slave_Synth#(type t);
  module toAXI4Lite_AR_Slave_Synth#(t#(x) ifc)
  (AXI4Lite_AR_Slave_Synth#(addr_, user_))
  provisos (FromAXI4Lite_ARFlit#(x, addr_, user_));
endtypeclass

instance ToAXI4Lite_AR_Slave_Synth#(Sink);
  module toAXI4Lite_AR_Slave_Synth#(Sink#(t) snk)
  (AXI4Lite_AR_Slave_Synth#(addr_, user_))
  provisos (FromAXI4Lite_ARFlit#(t, addr_, user_));

    let w_araddr <- mkDWire(?);
    let w_arprot <- mkDWire(?);
    let w_aruser <- mkDWire(?);
    PulseWire putWire <- mkPulseWire;
    rule doPut (putWire && snk.canPut);
      snk.put(fromAXI4Lite_ARFlit(AXI4Lite_ARFlit {
        araddr: w_araddr, arprot: w_arprot, aruser: w_aruser
      }));
    endrule

    method araddr(addr)   = action w_araddr <= addr; endaction;
    method arprot(prot)   = action w_arprot <= prot; endaction;
    method aruser(user)   = action w_aruser <= user; endaction;
    method arvalid(valid) = action if (valid) putWire.send; endaction;
    method arready        = snk.canPut;

  endmodule
endinstance

instance ToAXI4Lite_AR_Slave_Synth#(FIFOF);
  module toAXI4Lite_AR_Slave_Synth#(FIFOF#(t) ff)
  (AXI4Lite_AR_Slave_Synth#(addr_, user_))
  provisos (FromAXI4Lite_ARFlit#(t, addr_, user_));

    let w_araddr <- mkDWire(?);
    let w_arprot <- mkDWire(?);
    let w_aruser <- mkDWire(?);
    PulseWire enqWire <- mkPulseWire;
    rule doEnq (enqWire && ff.notFull);
      ff.enq(fromAXI4Lite_ARFlit(AXI4Lite_ARFlit {
        araddr: w_araddr, arprot: w_arprot, aruser: w_aruser
      }));
    endrule

    method araddr(addr)   = action w_araddr <= addr; endaction;
    method arprot(prot)   = action w_arprot <= prot; endaction;
    method aruser(user)   = action w_aruser <= user; endaction;
    method arvalid(valid) = action if (valid) enqWire.send; endaction;
    method arready        = ff.notFull;

  endmodule
endinstance
