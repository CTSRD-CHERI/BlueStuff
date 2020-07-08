/*-
 * Copyright (c) 2020 Alexandre Joannou
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

package TransactionOrdering;

import SourceSink  :: *;
import MasterSlave :: *;

//import FIFOF :: *;
import SpecialFIFOs :: *;
import List  :: *;

//////////////////////
// Identify packets //
////////////////////////////////////////////////////////////////////////////////

typeclass Has_identifierField #(type t, type id_t)
  dependencies (t determines id_t);
  function id_t identifierField (t x);
endtypeclass

// TODO - trivially identifiable type wrapper?

///////////////////////////
// Strict ordering point //
////////////////////////////////////////////////////////////////////////////////
module mkStrictOrderingPoint #(Integer depth, Integer maxRspFlits)
                              (MasterSlave #(req, rsp));

  // Sanity checking
  ///////////////////////////////////////////////////////////////////////////////
  if (depth > 0) error ("mkStrictOrderingPoint: depth too small");
  if (maxRspFlits > 0) error ("mkStrictOrderingPoint: maxRspFlits too small");

  // Internal modules
  ///////////////////////////////////////////////////////////////////////////////
  // for interfaces
  let  inReqFF <- mkBypassFIFOF; let  inReqSrc = toSource (inReqFF);
  let outReqFF <- mkBypassFIFOF; let outReqSnk = toSink   (outReqFF);
  let  inRspFF <- mkBypassFIFOF; let  inRspSrc = toSource (inRspFF);
  let outRspFF <- mkBypassFIFOF; let outRspSnk = toSink   (outRspFF);

  // for internal behaviours
  let idFF <- mkUGSizedFF (depth);
  List #(Tuple2 #( Array #(Reg #(Maybe #(id_t)))
                 , List  #(Reg #(Maybe #(rsp)))))
    info <- replicateM (depth, tuple2 ( mkCReg (2, Invalid)
                                      , replicateM ( maxRspFlits
                                                   , mkReg (Invalid))));

  // misc helpers
  function isInvalid = compose (\not , isValid);
  function ifcN (n, ifcArray) = ifcArray[n];
  let idSlot1 = map (compose (ifcN (1), tpl_1), info);

  // Requests handling
  ///////////////////////////////////////////////////////////////////////////////
  // useful handles for requests - Use idSlot interface #1
  let reqID = identifierField (inReqSrc.peek);
  let mNextAvailSlot = find (compose (isInvalid, readReg), idSlot1);
  let mReqIDSlot = find (compose (\== (Valid (reqID)), readReg), idSlot1);
  let mReqSlot = (isInvalid (mReqIDSlot)) ? mNextAvailSlot : Invalid;
  let reqSlot = mReqSlot.Valid;

  rule forwardReq ( idFF.notFull
                 && inReqSrc.canPeek
                 && isValid (mReqSlot)
                 && outReqSnk.canPut);
    reqSlot <= Valid (reqID); // reserve slot
    outReqSnk.put (inReqSrc.peek);
    inReqSrc.drop;
    idFF.enq (reqID);
  endrule

  // Responses handling
  ///////////////////////////////////////////////////////////////////////////////
  // useful handles for responses - Use idSlot interface #0
  // XXX Note: using the haskell syntax would be much nicer... alternatively, we
  //           could really benefit from compose being vararg... ?
  function rspPred = compose ( compose ( compose (\== (Valid (rspID)), readReg)
                                       , ifcN (0))
                             , tpl_1);
  let rspID = identifierField (inRspSrc.peek);
  let mRspSlot = find (rspPred, info);
  let rspSlot = mRspSlot.Valid;
  let rspSlotID = compose (ifcN (0), tpl_1) (rspSlot);
  let rspSlotData = tpl_2 (rspSlot);
  let rspIdx <- mkReg (0);

  rule receiveRsp (inRspSrc.canPeek);
    if (!isValid (mRspSlot)) begin
      $display ("%0t - %m.mkOrderingPoint_core: response without allocated slot");
      $finish (0);
    end
    rspSlotData[rspIdx] <= Valid (inRspSrc.peek);
    inRspSrc.drop;
    rspIdx <= (isLast (inRspSrc.peek)) ? 0 : rspIdx + 1;
  endrule

  rule forwardRsp
    
  endrule

  // Interface
  ///////////////////////////////////////////////////////////////////////////////
  interface slave = interface Slave;
    interface   sink = toSink   (inReqFF);
    interface source = toSource (outRspFF);
  endinterface;

  interface master = interface Master;
    interface source = toSource (outReqFF);
    interface   sink = toSink   (inRspFF);
  endinterface;

endmodule
////////////////////
// ordering point //
////////////////////////////////////////////////////////////////////////////////
// The ordering point uses transaction identifier fields to maintain order. It
// considers transactions sharing an identifier part of a same ordered stream of
// transactions. Transactions with different identifiers are not considered as
// having ordering requirements between them.
// The ordering point is configured by list of integer.
// The length of the list indicates how many transactions with different
// identifiers can be in flight at the same time. Using a singleton list
// guarantees that only a single identifier is in flight at a time, providing
// full order between transactions in the incoming stream.
// The integer elements of the list determine the number of simultaneous
// transactions with the same identifier that are supported.
// The provided list should be non-empty and its elements should be strictly
// positive.
// XXX TODO depths are currently ignored TODO XXX
module mkOrderingPoint_core #(List #(Integer) cfg) (MasterSlave #(req, rsp))
  provisos ( Bits #(req, req_sz), Bits #(rsp, rsp_sz)
           , Bits #(id_t, id_sz), Eq #(id_t)
           , Has_identifierField #(req, id_t)
           , Has_identifierField #(rsp, id_t));

  // Sanity checking
  ///////////////////////////////////////////////////////////////////////////////
  Integer nID = length (cfg);
  if (nID == 0)
    error ("mkOrderingPoint_core: empty configuration");
  for (Integer i = 0; i < nID; i = i + 1) if (cfg[i] == 0)
    error ("mkOrderingPoint_core: configuration with null element");

  // TODO handle multiple in-flight transactions per identifier

  // Internal modules
  ///////////////////////////////////////////////////////////////////////////////
  // for interfaces
  let  inReqFF <- mkBypassFIFOF; let  inReqSrc = toSource (inReqFF);
  let outReqFF <- mkBypassFIFOF; let outReqSnk = toSink   (outReqFF);
  let  inRspFF <- mkBypassFIFOF; let  inRspSrc = toSource (inRspFF);
  let outRspFF <- mkBypassFIFOF; let outRspSnk = toSink   (outRspFF);

  // for internal behaviours
  // The idSlot list represent all the available reservation slots. Each slot is
  // a register with two interfaces, one for requests taking it and one for
  // responses releasing it. A taken slot has a valid identifier and a free one
  // is tagged invalid.
  List #(Array #(Reg #(Maybe #(id_t))))
    idSlot <- replicateM (nID, mkCReg (2, Invalid));

  // misc helpers
  function isInvalid = compose (\not , isValid);
  function ifcN (n, ifcArray) = ifcArray[n];
  let idSlot0 = map (ifcN (0), idSlot);
  let idSlot1 = map (ifcN (1), idSlot);

  // Requests handling
  ///////////////////////////////////////////////////////////////////////////////
  // useful handles for requests - Use idSlot interface #1
  let reqID = identifierField (inReqSrc.peek);
  let mNextAvailSlot = find (compose (isInvalid, readReg), idSlot1);
  let mReqIDSlot = find (compose (\== (Valid (reqID)), readReg), idSlot1);
  let mReqSlot = (isValid (mReqIDSlot)) ? mReqIDSlot : mNextAvailSlot;
  let reqSlot = mReqSlot.Valid;

  rule forwardReq (inReqSrc.canPeek && isValid (mReqSlot) && outReqSnk.canPut);
    reqSlot <= Valid (reqID); // reserve slot (if not already taken)
    outReqSnk.put (inReqSrc.peek);
    inReqSrc.drop;
  endrule

  // Responses handling
  ///////////////////////////////////////////////////////////////////////////////
  // useful handles for responses - Use idSlot interface #0
  let rspID = identifierField (inRspSrc.peek);
  let mRspSlot = find (compose (\== (Valid (rspID)), readReg), idSlot1);
  let rspSlot = mRspSlot.Valid;

  rule forwardRsp (inRspSrc.canPeek && outRspSnk.canPut);
    if (!isValid (mRspSlot)) begin
      $display ("%0t - %m.mkOrderingPoint_core: response without allocated slot");
      $finish (0);
    end
    rspSlot <= Invalid; // release slot (TODO only if last resp. for given id.)
    outRspSnk.put (inRspSrc.peek);
    inRspSrc.drop;
  endrule

  // Interface
  ///////////////////////////////////////////////////////////////////////////////
  interface slave = interface Slave;
    interface   sink = toSink   (inReqFF);
    interface source = toSource (outRspFF);
  endinterface;

  interface master = interface Master;
    interface source = toSource (outReqFF);
    interface   sink = toSink   (inRspFF);
  endinterface;

endmodule

endpackage
