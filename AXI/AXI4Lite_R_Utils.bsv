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

///////////////////////////
// AXI Read Data Channel //
////////////////////////////////////////////////////////////////////////////////

// typeclasses to convert to/from the flit type

typeclass ToAXI4Lite_RFlit#(type t, numeric type data_, numeric type user_);
  function AXI4Lite_RFlit#(data_, user_) toAXI4Lite_RFlit (t x);
endtypeclass

instance ToAXI4Lite_RFlit#(AXI4Lite_RFlit#(a, b), a, b);
  function toAXI4Lite_RFlit = id;
endinstance

typeclass FromAXI4Lite_RFlit#(type t, numeric type data_, numeric type user_);
  function t fromAXI4Lite_RFlit (AXI4Lite_RFlit#(data_, user_) x);
endtypeclass

instance FromAXI4Lite_RFlit#(AXI4Lite_RFlit#(a, b), a, b);
  function fromAXI4Lite_RFlit = id;
endinstance

// typeclass to turn an interface to the Master interface

typeclass ToAXI4Lite_R_Master_Synth#(type t);
  module toAXI4Lite_R_Master_Synth#(t#(x) ifc)
  (AXI4Lite_R_Master_Synth#(data_, user_))
  provisos (FromAXI4Lite_RFlit#(x, data_, user_));
endtypeclass

instance ToAXI4Lite_R_Master_Synth#(Sink);
  module toAXI4Lite_R_Master_Synth#(Sink#(t) snk)
  (AXI4Lite_R_Master_Synth#(data_, user_))
  provisos (FromAXI4Lite_RFlit#(t, data_, user_));

    let w_rdata <- mkDWire(?);
    let w_rresp <- mkDWire(?);
    let w_ruser <- mkDWire(?);
    PulseWire putWire <- mkPulseWire;
    rule doPut (putWire && snk.canPut);
      snk.put(fromAXI4Lite_RFlit(AXI4Lite_RFlit {
        rdata: w_rdata, rresp: w_rresp, ruser: w_ruser
      }));
    endrule

    method rdata(data)   = action w_rdata <= data; endaction;
    method rresp(resp)   = action w_rresp <= resp; endaction;
    method ruser(user)   = action w_ruser <= user; endaction;
    method rvalid(valid) = action if (valid) putWire.send; endaction;
    method rready        = snk.canPut;

  endmodule
endinstance

instance ToAXI4Lite_R_Master_Synth#(FIFOF);
  module toAXI4Lite_R_Master_Synth#(FIFOF#(t) ff)
  (AXI4Lite_R_Master_Synth#(data_, user_))
  provisos (FromAXI4Lite_RFlit#(t, data_, user_));

    let w_rdata <- mkDWire(?);
    let w_rresp <- mkDWire(?);
    let w_ruser <- mkDWire(?);
    PulseWire enqWire <- mkPulseWire;
    rule doEnq (enqWire && ff.notFull);
      ff.enq(fromAXI4Lite_RFlit(AXI4Lite_RFlit {
        rdata: w_rdata, rresp: w_rresp, ruser: w_ruser
      }));
    endrule

    method rdata(data)   = action w_rdata <= data; endaction;
    method rresp(resp)   = action w_rresp <= resp; endaction;
    method ruser(user)   = action w_ruser <= user; endaction;
    method rvalid(valid) = action if (valid) enqWire.send; endaction;
    method rready        = ff.notFull;

  endmodule
endinstance

// typeclass to turn an interface to the Slave interface

typeclass ToAXI4Lite_R_Slave_Synth#(type t);
  module toAXI4Lite_R_Slave_Synth#(t#(x) ifc)
  (AXI4Lite_R_Slave_Synth#(data_, user_))
  provisos (ToAXI4Lite_RFlit#(x, data_, user_));
endtypeclass

instance ToAXI4Lite_R_Slave_Synth#(Source);
  module toAXI4Lite_R_Slave_Synth#(Source#(t) src)
  (AXI4Lite_R_Slave_Synth#(data_, user_))
  provisos (ToAXI4Lite_RFlit#(t, data_, user_));

    Wire#(AXI4Lite_RFlit#(data_, user_)) flit <- mkDWire(?);
    rule peekFlit (src.canPeek); flit <= toAXI4Lite_RFlit(src.peek); endrule
    PulseWire dropWire <- mkPulseWire;
    rule doDrop (dropWire && src.canPeek); src.drop; endrule

    method rdata  = flit.rdata;
    method rresp  = flit.rresp;
    method ruser  = flit.ruser;
    method rvalid = src.canPeek;
    method rready(rdy) = action if (rdy) dropWire.send; endaction;

  endmodule
endinstance

instance ToAXI4Lite_R_Slave_Synth#(FIFOF);
  module toAXI4Lite_R_Slave_Synth#(FIFOF#(t) ff)
  (AXI4Lite_R_Slave_Synth#(data_, user_))
  provisos (ToAXI4Lite_RFlit#(t, data_, user_));

    Wire#(AXI4Lite_RFlit#(data_, user_)) flit <- mkDWire(?);
    rule peekFlit (ff.notEmpty); flit <= toAXI4Lite_RFlit(ff.first); endrule
    PulseWire deqWire <- mkPulseWire;
    rule doDeq (deqWire && ff.notEmpty); ff.deq; endrule

    method rdata  = flit.rdata;
    method rresp  = flit.rresp;
    method ruser  = flit.ruser;
    method rvalid = ff.notEmpty;
    method rready(rdy) = action if (rdy) deqWire.send; endaction;

  endmodule
endinstance
