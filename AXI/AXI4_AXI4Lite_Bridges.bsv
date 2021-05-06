/*-
 * Copyright (c) 2018-2021 Alexandre Joannou
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

// AXI imports
import AXI4_Types :: *;
import AXI4Lite_Types :: *;
import AXI4_AXI4Lite_Types :: *;

// BlueBasics import
import SourceSink :: *;

// Standard
import FIFOF :: *;
import SpecialFIFOs :: *;

// die helper
function Action die (Fmt m) = action $display (m); $finish (0); endaction;

///////////////////////////////////////////
// simple flit conversion and assertions //
////////////////////////////////////////////////////////////////////////////////

// set of default AXI4 flit to represent AXI4Lite flits
AXI4_AWFlit #(id_, addr_, awuser_) dfltAW =
  AXI4_AWFlit { awid: 0
              , awaddr: ?
              , awlen: 0
              , awsize: ?
              , awburst: FIXED
              , awlock: NORMAL
              , awcache: 0
              , awprot: ?
              , awqos: 0
              , awregion: 0
              , awuser: ? };
AXI4_WFlit #(data_, wuser_) dfltW =
  AXI4_WFlit { wdata: ?
             , wstrb: ?
             , wlast: True
             , wuser: ? };
AXI4_BFlit #(id_, buser_) dfltB =
  AXI4_BFlit { bid: 0
             , bresp: ?
             , buser: ? };
AXI4_ARFlit #(id_, addr_, aruser_) dfltAR =
  AXI4_ARFlit { arid: 0
              , araddr: ?
              , arlen: 0
              , arsize: ?
              , arburst: FIXED
              , arlock: NORMAL
              , arcache: 0
              , arprot: ?
              , arqos: 0
              , arregion: 0
              , aruser: ? };
AXI4_RFlit #(id_, data_, ruser_) dfltR =
  AXI4_RFlit { rid: 0
             , rdata: ?
             , rresp: ?
             , rlast: True
             , ruser: ? };

// see previous definitions for field values of AXI4 exclusive fields
function AXI4_AWFlit #(id_, addr_, awuser_)
         fromAXI4Lite_AW (AXI4Lite_AWFlit #(addr_, awuser_) x);
  AXI4_AWFlit #(id_, addr_, awuser_) res = dfltAW;
  res.awaddr = x.awaddr;
  res.awprot = x.awprot;
  res.awuser = x.awuser;
  return res;
endfunction
function AXI4_WFlit #(data_, wuser_)
         fromAXI4Lite_W (AXI4Lite_WFlit #(data_, wuser_) x);
  AXI4_WFlit #(data_, wuser_) res = dfltW;
  res.wdata = x.wdata;
  res.wstrb = x.wstrb;
  res.wuser = x.wuser;
  return res;
endfunction
function AXI4_BFlit #(id_, buser_)
         fromAXI4Lite_B (AXI4Lite_BFlit #(buser_) x);
  AXI4_BFlit #(id_, buser_) res = dfltB;
  res.bresp = x.bresp;
  res.buser = x.buser;
  return res;
endfunction
function AXI4_ARFlit #(id_, addr_, aruser_)
         fromAXI4Lite_AR (AXI4Lite_ARFlit #(addr_, aruser_) x);
  AXI4_ARFlit #(id_, addr_, aruser_) res = dfltAR;
  res.araddr = x.araddr;
  res.arprot = x.arprot;
  res.aruser = x.aruser;
  return res;
endfunction
function AXI4_RFlit #(id_, data_, ruser_)
         fromAXI4Lite_R (AXI4Lite_RFlit #(data_, ruser_) x);
  AXI4_RFlit #(id_, data_, ruser_) res = dfltR;
  res.rdata = x.rdata;
  res.rresp = x.rresp;
  res.ruser = x.ruser;
  return res;
endfunction

// always strict subset of fields
function AXI4Lite_AWFlit #(addr_, awuser_)
         toAXI4Lite_AW (AXI4_AWFlit #(id_, addr_, awuser_) x) =
  AXI4Lite_AWFlit { awaddr: x.awaddr
                  , awprot: x.awprot
                  , awuser: x.awuser };
function AXI4Lite_WFlit #(data_, wuser_)
         toAXI4Lite_W (AXI4_WFlit #(data_, wuser_) x) =
  AXI4Lite_WFlit { wdata: x.wdata
                 , wstrb: x.wstrb
                 , wuser: x.wuser };
function AXI4Lite_BFlit #(buser_)
         toAXI4Lite_B (AXI4_BFlit #(id_, buser_) x) =
  AXI4Lite_BFlit { bresp: x.bresp
                 , buser: x.buser };
function AXI4Lite_ARFlit #(addr_, aruser_)
         toAXI4Lite_AR (AXI4_ARFlit #(id_, addr_, aruser_) x) =
  AXI4Lite_ARFlit { araddr: x.araddr
                  , arprot: x.arprot
                  , aruser: x.aruser };
function AXI4Lite_RFlit #(data_, ruser_)
         toAXI4Lite_R (AXI4_RFlit #(id_, data_, ruser_) x) =
  AXI4Lite_RFlit { rdata: x.rdata
                 , rresp: x.rresp
                 , ruser: x.ruser };

// assertions on AXI4 flits
function Action checkAXI4_AWFlit ( AXI4_Size busSize
                                 , AXI4_AWFlit #(id_, addr_, awuser_) x) =
  action
    if (x.awid != 0)
      die ($format("Unsupported awid (0x%0x)", x.awid));
    if (x.awlen != 0)
      die ($format("Unsupported awlen (0x%0x)", x.awlen));
    if (x.awsize != busSize)
      die ($format("Unsupported awsize (0x%0x)", x.awsize));
    if (x.awburst != FIXED)
      die ($format("Unsupported awburst (", fshow(x.awburst), ")"));
    if (x.awlock != NORMAL)
      die ($format("Unsupported awlock (", fshow(x.awlock), ")"));
    if (x.awcache != 0)
      die ($format("Unsupported awcache (0x%0x)", x.awcache));
    if (x.awqos != 0)
      die ($format("Unsupported awqos (0x%0x)", x.awqos));
    if (x.awregion != 0)
      die ($format("Unsupported awregion (0x%0x)", x.awregion));
  endaction;
function Action checkAXI4_WFlit (AXI4_WFlit #(data_, wuser_) x) = action
  if (x.wlast != True)
    die ($format("Unsupported wlast (", fshow(x.wlast), ")"));
endaction;
function Action checkAXI4_BFlit (AXI4_BFlit #(id_, buser_) x) = action
  if (x.bid != 0)
    die ($format("Unsupported bid (0x%0x)", x.bid));
endaction;
function Action checkAXI4_ARFlit ( AXI4_Size busSize
                                 , AXI4_ARFlit #(id_, addr_, aruser_) x) =
  action
    if (x.arid != 0)
      die ($format("Unsupported arid (0x%0x)", x.arid));
    if (x.arlen != 0)
      die ($format("Unsupported arlen (0x%0x)", x.arlen));
    if (x.arsize != busSize)
      die ($format("Unsupported arsize (0x%0x)", x.arsize));
    if (x.arburst != FIXED)
      die ($format("Unsupported arburst (", fshow(x.arburst), ")"));
    if (x.arlock != NORMAL)
      die ($format("Unsupported arlock (", fshow(x.arlock), ")"));
    if (x.arcache != 0)
      die ($format("Unsupported arcache (0x%0x)", x.arcache));
    if (x.arqos != 0)
      die ($format("Unsupported arqos (0x%0x)", x.arqos));
    if (x.arregion != 0)
      die ($format("Unsupported arregion (0x%0x)", x.arregion));
  endaction;
function Action checkAXI4_RFlit (AXI4_RFlit #(id_, data_, ruser_) x) = action
  if (x.rid != 0)
    die ($format("Unsupported rid (0x%0x)", x.rid));
  if (x.rlast != True)
    die ($format("Unsupported rlast (", fshow(x.rlast), ")"));
endaction;

////////////////////////
// Master convertions //
////////////////////////////////////////////////////////////////////////////////

function (AXI4Lite_Master #( addr_, data_
                           , awuser_, wuser_, buser_
                           , aruser_, ruser_))
         fromAXI4ToAXI4Lite_Master (
           AXI4_Master #( id_, addr_, data_
                        , awuser_, wuser_, buser_
                        , aruser_, ruser_) m) = interface AXI4Lite_Master;
  let busSize = fromInteger(valueOf(data_)/8);
  interface aw = mapSource ( toAXI4Lite_AW
                           , onDrop (m.aw, checkAXI4_AWFlit (busSize)) );
  interface  w = mapSource ( toAXI4Lite_W
                           , onDrop (m.w, checkAXI4_WFlit) );
  interface  b = mapSink   ( fromAXI4Lite_B
                           , onPut (m.b, checkAXI4_BFlit) );
  interface ar = mapSource ( toAXI4Lite_AR
                           , onDrop (m.ar, checkAXI4_ARFlit (busSize)) );
  interface  r = mapSink   ( fromAXI4Lite_R
                           , onPut (m.r, checkAXI4_RFlit) );
endinterface;

function (AXI4_Master #( id_, addr_, data_
                       , awuser_, wuser_, buser_
                       , aruser_, ruser_))
         fromAXI4LiteToAXI4_Master (
           AXI4Lite_Master #( addr_, data_
                            , awuser_, wuser_, buser_
                            , aruser_, ruser_) mLite) = interface AXI4_Master;
  let busSize = fromInteger(valueOf(data_)/8);
  interface aw = onDrop ( mapSource (fromAXI4Lite_AW, mLite.aw)
                        , checkAXI4_AWFlit (busSize) );
  interface  w = onDrop ( mapSource (fromAXI4Lite_W, mLite.w)
                        , checkAXI4_WFlit );
  interface  b = onPut  ( mapSink (toAXI4Lite_B, mLite.b)
                        , checkAXI4_BFlit );
  interface ar = onDrop ( mapSource (fromAXI4Lite_AR, mLite.ar)
                        , checkAXI4_ARFlit (busSize) );
  interface  r = onPut  ( mapSink (toAXI4Lite_R, mLite.r)
                        , checkAXI4_RFlit );
endinterface;

///////////////////////
// Slave convertions //
////////////////////////////////////////////////////////////////////////////////

function (AXI4Lite_Slave #( addr_, data_
                          , awuser_, wuser_, buser_
                          , aruser_, ruser_))
         fromAXI4ToAXI4Lite_Slave (
           AXI4_Slave #( id_, addr_, data_
                       , awuser_, wuser_, buser_
                       , aruser_, ruser_) s) = interface AXI4Lite_Slave;
  let busSize = fromInteger(valueOf(data_)/8);
  interface aw = mapSink   ( fromAXI4Lite_AW
                           , onPut (s.aw, checkAXI4_AWFlit (busSize) ));
  interface  w = mapSink   ( fromAXI4Lite_W
                           , onPut (s.w, checkAXI4_WFlit) );
  interface  b = mapSource ( toAXI4Lite_B
                           , onDrop (s.b, checkAXI4_BFlit) );
  interface ar = mapSink   ( fromAXI4Lite_AR
                           , onPut (s.ar, checkAXI4_ARFlit (busSize)) );
  interface  r = mapSource ( toAXI4Lite_R
                           , onDrop (s.r, checkAXI4_RFlit) );
endinterface;

function (AXI4_Slave #( id_, addr_, data_
                      , awuser_, wuser_, buser_
                      , aruser_, ruser_))
         fromAXI4LiteToAXI4_Slave (
           AXI4Lite_Slave #( addr_, data_
                           , awuser_, wuser_, buser_
                           , aruser_, ruser_) sLite) = interface AXI4_Slave;
  let busSize = fromInteger(valueOf(data_)/8);
  interface aw = onPut  ( mapSink (toAXI4Lite_AW, sLite.aw)
                        , checkAXI4_AWFlit (busSize) );
  interface  w = onPut  ( mapSink (toAXI4Lite_W, sLite.w)
                        , checkAXI4_WFlit );
  interface  b = onDrop ( mapSource (fromAXI4Lite_B, sLite.b)
                        , checkAXI4_BFlit );
  interface ar = onPut  ( mapSink (toAXI4Lite_AR, sLite.ar)
                        , checkAXI4_ARFlit (busSize) );
  interface  r = onDrop ( mapSource (fromAXI4Lite_R, sLite.r)
                        , checkAXI4_RFlit );
endinterface;
