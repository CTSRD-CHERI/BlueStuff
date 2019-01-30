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

// typeclasses to convert to/from the flit type

typeclass ToAXIAWFlit#(type t,
numeric type id_, numeric type addr_, numeric type user_);
  function AWFlit#(id_, addr_, user_) toAXIAWFlit (t x);
endtypeclass

instance ToAXIAWFlit#(AWFlit#(a, b, c), a, b, c);
  function toAXIAWFlit = id;
endinstance

typeclass FromAXIAWFlit#(type t,
numeric type id_, numeric type addr_, numeric type user_);
  function t fromAXIAWFlit (AWFlit#(id_, addr_, user_) x);
endtypeclass

instance FromAXIAWFlit#(AWFlit#(a, b, c), a, b, c);
  function fromAXIAWFlit = id;
endinstance

// typeclass to turn an interface to the Master interface

typeclass ToAXIAWMaster#(type t);
  module toAXIAWMaster#(t#(x) ifc) (AWMaster#(id_, addr_, user_))
  provisos (ToAXIAWFlit#(x, id_, addr_, user_));
endtypeclass

instance ToAXIAWMaster#(Source);
  module toAXIAWMaster#(Source#(t) src)
  (AWMaster#(id_, addr_, user_)) provisos (ToAXIAWFlit#(t, id_, addr_, user_));

    Wire#(AWFlit#(id_, addr_, user_)) flit <- mkDWire(?);
    rule peekFlit (src.canPeek); flit <= toAXIAWFlit(src.peek); endrule
    PulseWire dropWire <- mkPulseWire;
    rule doDrop (dropWire && src.canPeek); src.drop; endrule

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
    method awready(rdy) = action if (rdy) dropWire.send; endaction;

  endmodule
endinstance

instance ToAXIAWMaster#(FIFOF);
  module toAXIAWMaster#(FIFOF#(t) ff)
  (AWMaster#(id_, addr_, user_)) provisos (ToAXIAWFlit#(t, id_, addr_, user_));

    Wire#(AWFlit#(id_, addr_, user_)) flit <- mkDWire(?);
    rule peekFlit (ff.notEmpty); flit <= toAXIAWFlit(ff.first); endrule
    PulseWire deqWire <- mkPulseWire;
    rule doDeq (deqWire && ff.notEmpty); ff.deq; endrule

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
    method awvalid  = ff.notEmpty;
    method awready(rdy) = action if (rdy) deqWire.send; endaction;

  endmodule
endinstance

// typeclass to turn an interface to the Slave interface

typeclass ToAXIAWSlave#(type t);
  module toAXIAWSlave#(t#(x) ifc) (AWSlave#(id_, addr_, user_))
  provisos (FromAXIAWFlit#(x, id_, addr_, user_));
endtypeclass

instance ToAXIAWSlave#(Sink);
  module toAXIAWSlave#(Sink#(t) snk)
  (AWSlave#(id_, addr_, user_)) provisos (FromAXIAWFlit#(t, id_, addr_, user_));

    let w_awid     <- mkDWire(?);
    let w_awaddr   <- mkDWire(?);
    let w_awlen    <- mkDWire(?);
    let w_awsize   <- mkDWire(?);
    let w_awburst  <- mkDWire(?);
    let w_awlock   <- mkDWire(?);
    let w_awcache  <- mkDWire(?);
    let w_awprot   <- mkDWire(?);
    let w_awqos    <- mkDWire(?);
    let w_awregion <- mkDWire(?);
    let w_awuser   <- mkDWire(?);
    PulseWire putWire <- mkPulseWire;
    rule doPut (putWire && snk.canPut);
      snk.put(fromAXIAWFlit(AWFlit{
        awid:     w_awid,
        awaddr:   w_awaddr,
        awlen:    w_awlen,
        awsize:   w_awsize,
        awburst:  w_awburst,
        awlock:   w_awlock,
        awcache:  w_awcache,
        awprot:   w_awprot,
        awqos:    w_awqos,
        awregion: w_awregion,
        awuser:   w_awuser
      }));
    endrule

    method awid(id)         = action w_awid     <= id; endaction;
    method awaddr(addr)     = action w_awaddr   <= addr; endaction;
    method awlen(len)       = action w_awlen    <= len; endaction;
    method awsize(size)     = action w_awsize   <= size; endaction;
    method awburst(burst)   = action w_awburst  <= burst; endaction;
    method awlock(lock)     = action w_awlock   <= lock; endaction;
    method awcache(cache)   = action w_awcache  <= cache; endaction;
    method awprot(prot)     = action w_awprot   <= prot; endaction;
    method awqos(qos)       = action w_awqos    <= qos; endaction;
    method awregion(region) = action w_awregion <= region; endaction;
    method awuser(user)     = action w_awuser   <= user; endaction;
    method awvalid(valid)   = action if (valid) putWire.send; endaction;
    method awready          = snk.canPut;

  endmodule
endinstance

instance ToAXIAWSlave#(FIFOF);
  module toAXIAWSlave#(FIFOF#(t) ff)
  (AWSlave#(id_, addr_, user_)) provisos (FromAXIAWFlit#(t, id_, addr_, user_));

    let w_awid     <- mkDWire(?);
    let w_awaddr   <- mkDWire(?);
    let w_awlen    <- mkDWire(?);
    let w_awsize   <- mkDWire(?);
    let w_awburst  <- mkDWire(?);
    let w_awlock   <- mkDWire(?);
    let w_awcache  <- mkDWire(?);
    let w_awprot   <- mkDWire(?);
    let w_awqos    <- mkDWire(?);
    let w_awregion <- mkDWire(?);
    let w_awuser   <- mkDWire(?);
    PulseWire enqWire <- mkPulseWire;
    rule doEnq (enqWire && ff.notFull);
      ff.enq(fromAXIAWFlit(AWFlit{
        awid:     w_awid,
        awaddr:   w_awaddr,
        awlen:    w_awlen,
        awsize:   w_awsize,
        awburst:  w_awburst,
        awlock:   w_awlock,
        awcache:  w_awcache,
        awprot:   w_awprot,
        awqos:    w_awqos,
        awregion: w_awregion,
        awuser:   w_awuser
      }));
    endrule

    method awid(id)         = action w_awid     <= id; endaction;
    method awaddr(addr)     = action w_awaddr   <= addr; endaction;
    method awlen(len)       = action w_awlen    <= len; endaction;
    method awsize(size)     = action w_awsize   <= size; endaction;
    method awburst(burst)   = action w_awburst  <= burst; endaction;
    method awlock(lock)     = action w_awlock   <= lock; endaction;
    method awcache(cache)   = action w_awcache  <= cache; endaction;
    method awprot(prot)     = action w_awprot   <= prot; endaction;
    method awqos(qos)       = action w_awqos    <= qos; endaction;
    method awregion(region) = action w_awregion <= region; endaction;
    method awuser(user)     = action w_awuser   <= user; endaction;
    method awvalid(valid)   = action if (valid) enqWire.send; endaction;
    method awready          = ff.notFull;

  endmodule
endinstance
