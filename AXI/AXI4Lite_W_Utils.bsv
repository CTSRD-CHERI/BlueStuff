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

typeclass ToAXIWLiteFlit#(type t, numeric type data_, numeric type user_);
  function WLiteFlit#(data_, user_) toAXIWLiteFlit (t x);
endtypeclass

instance ToAXIWLiteFlit#(WLiteFlit#(a, b), a, b);
  function toAXIWLiteFlit = id;
endinstance

typeclass FromAXIWLiteFlit#(type t, numeric type data_, numeric type user_);
  function t fromAXIWLiteFlit (WLiteFlit#(data_, user_) x);
endtypeclass

instance FromAXIWLiteFlit#(WLiteFlit#(a, b), a, b);
  function fromAXIWLiteFlit = id;
endinstance

// typeclass to turn an interface to the Master interface

typeclass ToAXIWLiteMaster#(type t);
  module toAXIWLiteMaster#(t#(x) ifc) (WLiteMaster#(data_, user_))
  provisos (ToAXIWLiteFlit#(x, data_, user_));
endtypeclass

instance ToAXIWLiteMaster#(Source);
  module toAXIWLiteMaster#(Source#(t) src)
  (WLiteMaster#(data_, user_)) provisos (ToAXIWLiteFlit#(t, data_, user_));

    Wire#(WLiteFlit#(data_, user_)) flit <- mkDWire(?);
    rule peekFlit (src.canPeek); flit <= toAXIWLiteFlit(src.peek); endrule
    PulseWire dropWire <- mkPulseWire;
    rule doDrop (dropWire && src.canPeek); src.drop; endrule

    method wdata  = flit.wdata;
    method wstrb  = flit.wstrb;
    method wuser  = flit.wuser;
    method wvalid = src.canPeek;
    method wready(rdy) = action if (rdy) dropWire.send; endaction;

  endmodule
endinstance

instance ToAXIWLiteMaster#(FIFOF);
  module toAXIWLiteMaster#(FIFOF#(t) ff)
  (WLiteMaster#(data_, user_)) provisos (ToAXIWLiteFlit#(t, data_, user_));

    Wire#(WLiteFlit#(data_, user_)) flit <- mkDWire(?);
    rule peekFlit (ff.notEmpty); flit <= toAXIWLiteFlit(ff.first); endrule
    PulseWire deqWire <- mkPulseWire;
    rule doDeq (deqWire && ff.notEmpty); ff.deq; endrule

    method wdata  = flit.wdata;
    method wstrb  = flit.wstrb;
    method wuser  = flit.wuser;
    method wvalid = ff.notEmpty;
    method wready(rdy) = action if (rdy) deqWire.send; endaction;

  endmodule
endinstance

// typeclass to turn an interface to the Slave interface

typeclass ToAXIWLiteSlave#(type t);
  module toAXIWLiteSlave#(t#(x) ifc) (WLiteSlave#(data_, user_))
  provisos (FromAXIWLiteFlit#(x, data_, user_));
endtypeclass

instance ToAXIWLiteSlave#(Sink);
  module toAXIWLiteSlave#(Sink#(t) snk)
  (WLiteSlave#(data_, user_)) provisos (FromAXIWLiteFlit#(t, data_, user_));

    let w_wdata <- mkDWire(?);
    let w_wstrb <- mkDWire(?);
    let w_wuser <- mkDWire(?);
    PulseWire putWire <- mkPulseWire;
    rule doPut (putWire && snk.canPut);
      snk.put(fromAXIWLiteFlit(WLiteFlit {
        wdata: w_wdata, wstrb: w_wstrb, wuser: w_wuser
      }));
    endrule

    method wdata(data)   = action w_wdata <= data; endaction;
    method wstrb(strb)   = action w_wstrb <= strb; endaction;
    method wuser(user)   = action w_wuser <= user; endaction;
    method wvalid(valid) = action if (valid) putWire.send; endaction;
    method wready        = snk.canPut;

  endmodule
endinstance

instance ToAXIWLiteSlave#(FIFOF);
  module toAXIWLiteSlave#(FIFOF#(t) ff)
  (WLiteSlave#(data_, user_)) provisos (FromAXIWLiteFlit#(t, data_, user_));

    let w_wdata <- mkDWire(?);
    let w_wstrb <- mkDWire(?);
    let w_wuser <- mkDWire(?);
    PulseWire enqWire <- mkPulseWire;
    rule doEnq (enqWire && ff.notFull);
      ff.enq(fromAXIWLiteFlit(WLiteFlit {
        wdata: w_wdata, wstrb: w_wstrb, wuser: w_wuser
      }));
    endrule

    method wdata(data)   = action w_wdata <= data; endaction;
    method wstrb(strb)   = action w_wstrb <= strb; endaction;
    method wuser(user)   = action w_wuser <= user; endaction;
    method wvalid(valid) = action if (valid) enqWire.send; endaction;
    method wready        = ff.notFull;

  endmodule
endinstance
