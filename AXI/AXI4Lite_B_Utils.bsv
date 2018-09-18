/*-
 * Copyright (c) 2018 Alexandre Joannou
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

////////////////////////////////
// AXI Write Response Channel //
////////////////////////////////////////////////////////////////////////////////

// typeclasses to convert to/from the flit type

typeclass ToAXIBLiteFlit#(type t);
  function BLiteFlit toAXIBLiteFlit (t x);
endtypeclass

instance ToAXIBLiteFlit#(BLiteFlit);
  function toAXIBLiteFlit = id;
endinstance

typeclass FromAXIBLiteFlit#(type t);
  function t fromAXIBLiteFlit (BLiteFlit x);
endtypeclass

instance FromAXIBLiteFlit#(BLiteFlit);
  function fromAXIBLiteFlit = id;
endinstance

// typeclass to turn an interface to the Master interface

typeclass ToAXIBLiteMaster#(type t);
  module toAXIBLiteMaster#(t#(x) ifc) (BLiteMaster)
  provisos (FromAXIBLiteFlit#(x));
endtypeclass

instance ToAXIBLiteMaster#(Sink);
  module toAXIBLiteMaster#(Sink#(t) snk)
  (BLiteMaster) provisos (FromAXIBLiteFlit#(t));

    let w_bresp <- mkDWire(?);
    PulseWire putWire <- mkPulseWire;
    rule doPut (putWire && snk.canPut);
      snk.put(fromAXIBLiteFlit(BLiteFlit{bresp: w_bresp}));
    endrule

    method bresp(resp)   = action w_bresp <= resp; endaction;
    method bvalid(valid) = action if (valid) putWire.send; endaction;
    method bready        = snk.canPut;

  endmodule
endinstance

instance ToAXIBLiteMaster#(FIFOF);
  module toAXIBLiteMaster#(FIFOF#(t) ff)
  (BLiteMaster) provisos (FromAXIBLiteFlit#(t));

    let w_bresp <- mkDWire(?);
    PulseWire enqWire <- mkPulseWire;
    rule doEnq (enqWire && ff.notFull);
      ff.enq(fromAXIBLiteFlit(BLiteFlit{bresp: w_bresp}));
    endrule

    method bresp(resp)   = action w_bresp <= resp; endaction;
    method bvalid(valid) = action if (valid) enqWire.send; endaction;
    method bready        = ff.notFull;

  endmodule
endinstance

// typeclass to turn an interface to the Slave interface

typeclass ToAXIBLiteSlave#(type t);
  module toAXIBLiteSlave#(t#(x) ifc) (BLiteSlave)
  provisos (ToAXIBLiteFlit#(x));
endtypeclass

instance ToAXIBLiteSlave#(Source);
  module toAXIBLiteSlave#(Source#(t) src)
  (BLiteSlave) provisos (ToAXIBLiteFlit#(t));

    Wire#(BLiteFlit) flit <- mkDWire(?);
    rule getFlit (src.canGet); flit <= toAXIBLiteFlit(src.peek); endrule
    PulseWire getWire <- mkPulseWire;
    rule doGet (getWire && src.canGet); let _ <- src.get; endrule

    method bresp  = flit.bresp;
    method bvalid = src.canGet;
    method bready(rdy) = action if (rdy) getWire.send; endaction;

  endmodule
endinstance

instance ToAXIBLiteSlave#(FIFOF);
  module toAXIBLiteSlave#(FIFOF#(t) ff)
  (BLiteSlave) provisos (ToAXIBLiteFlit#(t));

    Wire#(BLiteFlit) flit <- mkDWire(?);
    rule getFlit (ff.notEmpty); flit <= toAXIBLiteFlit(ff.first); endrule
    PulseWire deqWire <- mkPulseWire;
    rule doDeq (deqWire && ff.notEmpty); ff.deq; endrule

    method bresp  = flit.bresp;
    method bvalid = ff.notEmpty;
    method bready(rdy) = action if (rdy) deqWire.send; endaction;

  endmodule
endinstance
