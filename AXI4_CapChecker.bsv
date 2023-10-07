/*-
 * Copyright (c) 2023 Alexandre Joannou
 * All rights reserved.
 *
 * This material is based upon work supported by the DoD Information Analysis
 * Center Program Management Office (DoD IAC PMO), sponsored by the Defense
 * Technical Information Center (DTIC) under Contract No. FA807518D0004.  Any
 * opinions, findings and conclusions or recommendations expressed in this
 * material are those of the author(s) and do not necessarily reflect the views
 * of the Air Force Installation Contracting Agency (AFICA).
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

package AXI4_CapChecker;

import CHERICap :: *;
import CHERICC_Fat :: *;
import BlueAXI4 :: *;
import BlueBasics :: *;
import Connectable :: *;
import FIFOF :: *;
import Vector :: *;

interface AXI4_CapChecker #(
  numeric type id_,
  numeric type addr_,
  numeric type data_,
  numeric type s_awuser_,
  numeric type m_awuser_,
  numeric type wuser_,
  numeric type buser_,
  numeric type s_aruser_,
  numeric type m_aruser_,
  numeric type ruser_,
  numeric type mgnt_id_,
  numeric type mgnt_addr_,
  numeric type mgnt_data_,
  numeric type mgnt_awuser_,
  numeric type mgnt_wuser_,
  numeric type mgnt_buser_,
  numeric type mgnt_aruser_,
  numeric type mgnt_ruser_);
  interface AXI4_Slave #( id_, addr_, data_
                        , s_awuser_, wuser_, buser_
                        , s_aruser_, ruser_ ) slave;
  interface AXI4_Master #( id_, addr_, data_
                         , m_awuser_, wuser_, buser_
                         , m_aruser_, ruser_ ) master;
  interface AXI4_Slave #( mgnt_id_, mgnt_addr_, mgnt_data_
                        , mgnt_awuser_, mgnt_wuser_, mgnt_buser_
                        , mgnt_aruser_, mgnt_ruser_ ) management;
endinterface

// numeric type:
// number of bits required to hold the max number of bytes in a transfer
typedef TAdd #( // AXI4 len width + 1 because of representation is -1
                TAdd #(SizeOf #(AXI4_Len), 1)
                // (2^(AXI4 size width))-1 maximum shift amount
              , TSub #(TExp #(SizeOf #(AXI4_Size)), 1) ) MaxBytesSz;

module mkAXI4_CapChecker #(NumProxy #(rawN) nCapProxy)
  (AXI4_CapChecker #( id_, addr_, data_
                    , s_awuser_, m_awuser_, wuser_, buser_
                    , s_aruser_, m_aruser_, ruser_
                    , mgnt_id_, mgnt_addr_, mgnt_data_
                    , mgnt_awuser_, mgnt_wuser_, mgnt_buser_
                    , mgnt_aruser_, mgnt_ruser_ ))
  provisos ( // aliases
             Alias #(cap_t, CapMem)
           , NumAlias #(n, TExp #(TLog #(rawN)))
           , NumAlias #(idxSz, TLog #(n))
             // constraints
           , CHERICap #(cap_t, otypeW, flgW, addrW, inMemW, maskableW)
           , Add #(addrW, 0, addr_)
           , Add #(128, 0, inMemW)
           , Add #(128, 0, mgnt_data_)
           , Add #(1, 0, mgnt_wuser_)
           , Add #(1, 0, mgnt_ruser_)
           , Add #(a__, idxSz, mgnt_addr_)
           );

  AXI4_Shim #( id_, addr_, data_
             , s_awuser_, wuser_, buser_
             , s_aruser_, ruser_ ) inShim <- mkAXI4Shim;
  AXI4_Shim #( id_, addr_, data_
             , m_awuser_, wuser_, buser_
             , m_aruser_, ruser_ ) outShim <- mkAXI4Shim;
  AXI4_Shim #( mgnt_id_, mgnt_addr_, mgnt_data_
             , mgnt_awuser_, mgnt_wuser_, mgnt_buser_
             , mgnt_aruser_, mgnt_ruser_ ) ctrlShim <- mkAXI4Shim;

  // CapChecker management flow
  //////////////////////////////////////////////////////////////////////////////

  Vector #(n, Reg #(cap_t)) caps <- replicateM (mkReg (nullCap));
  rule writeCap;
    let awflit <- get (ctrlShim.master.aw);
    let wflit <- get (ctrlShim.master.w);
    Bit #(idxSz) idx = truncate (awflit.awaddr >> 4);
    //XXX TODO caps[idx] <= fromMem (tuple2 (unpack(wflit.wuser), wflit.wdata));
    caps[idx] <= ?;
    ctrlShim.master.b.put (AXI4_BFlit {
      bid: awflit.awid, bresp: OKAY, buser: ?
    });
  endrule
  rule readCap;
    let arflit <- get (ctrlShim.master.ar);
    Bit #(idxSz) idx = truncate (arflit.araddr >> 4);
    //XXX TODO match {.user, .data} = toMem (caps[idx]);
    match {.user, .data} = tuple2(1'b0, 128'h0);
    ctrlShim.master.r.put (AXI4_RFlit {
      rid: arflit.arid, rdata: data, rresp: OKAY, rlast: True, ruser: pack(user)
    });
  endrule

  // CapChecker dataflow
  //////////////////////////////////////////////////////////////////////////////

  function Bool checkAccess( cap_t cap
                           , Bit#(addrW) addr
                           , Bit#(MaxBytesSz) nBytes
                           , Bool isWrite
                           , Bool isRead );
    //XXX TODO Bit#(addrW) base = getBase(cap);
    Bit#(addrW) base = 0;
    //XXX TODO Bit#(TAdd #(addrW, 1)) top = getTop(cap);
    Bit#(TAdd #(addrW, 1)) top = 0;
    Bit#(addrW) endAddr = addr + zeroExtend(nBytes);
    let perms = getHardPerms(cap);
    let wraps = False; // XXX TODO check if the access wraps the address space
    return    isValidCap(cap)
           && base <= addr
           && {1'b0, endAddr} < top
           && !wraps
           && (!isWrite || perms.permitStore)
           && ( !isRead ||  perms.permitLoad);
  endfunction

  // write rules
  //////////////

  let forwardWFF <- mkFIFOF;
  let forwardBFF <- mkFIFOF;

  rule checkWrite(forwardWFF.notFull);
    let awflit <- get(inShim.master.aw);
    let nBytes = (zeroExtend (awflit.awlen) + 1) << pack (awflit.awsize);
    let cap = caps[awflit.awuser];
    let forward = checkAccess(cap, awflit.awaddr, nBytes, True, False);
    if (forward) outShim.slave.aw.put(mapAXI4_AWFlit_awuser(constFn(?), awflit));
    forwardWFF.enq(tuple2(awflit.awid, forward));
  endrule
  rule forwardW ( forwardWFF.notEmpty && tpl_2(forwardWFF.first)
                                      && forwardBFF.notFull );
    let wflit <- get(inShim.master.w);
    outShim.slave.w.put(wflit);
    if (wflit.wlast) begin
      forwardWFF.deq;
      forwardBFF.enq(True);
    end
  endrule
  rule drainW ( forwardWFF.notEmpty && !tpl_2(forwardWFF.first)
                                    && forwardBFF.notFull );
    let wflit <- get(inShim.master.w);
    if (wflit.wlast) begin
      forwardWFF.deq;
      forwardBFF.enq(False);
    end
  endrule
  rule forwardB ( forwardBFF.notEmpty && forwardBFF.first
                                      && outShim.slave.b.canPeek
                                      && inShim.master.b.canPut );
    let bflit <- get(outShim.slave.b);
    inShim.master.b.put(bflit);
    forwardBFF.deq;
  endrule
  rule errorB ( forwardBFF.notEmpty && !forwardBFF.first
                                    && inShim.master.b.canPut );
    inShim.master.b.put(AXI4_BFlit {
      bid: tpl_1(forwardWFF.first), bresp: DECERR, buser: ?
    });
    forwardBFF.deq;
  endrule

  // read rules
  /////////////

  let forwardRFF <- mkFIFOF;
  rule checkRead;
    let arflit <- get(inShim.master.ar);
    let nBytes = (zeroExtend (arflit.arlen) + 1) << pack (arflit.arsize);
    let cap = caps[arflit.aruser];
    let forward = checkAccess(cap, arflit.araddr, nBytes, False, True);
    if (forward)
      outShim.slave.ar.put(mapAXI4_ARFlit_aruser(constFn(?), arflit));
    forwardRFF.enq(tuple3(arflit.arid, arflit.arlen, forward));
  endrule
  rule forwardR ( forwardRFF.notEmpty && tpl_3(forwardRFF.first)
                                      && outShim.slave.r.canPeek
                                      && inShim.master.r.canPut );
    let rflit <- get(outShim.slave.r);
    inShim.master.r.put(rflit);
    if (rflit.rlast) forwardRFF.deq;
  endrule
  let cntErrorR <- mkReg(0);
  rule errorR ( forwardRFF.notEmpty && !tpl_3(forwardRFF.first)
                                    && outShim.slave.r.canPeek
                                    && inShim.master.r.canPut );
    let isLast = cntErrorR == tpl_2(forwardRFF.first);
    inShim.master.r.put(AXI4_RFlit {
      rid: tpl_1(forwardRFF.first), rdata: ?
    , rresp: DECERR, rlast: isLast, ruser: ?
    });
    if (isLast) begin
      forwardRFF.deq;
      cntErrorR <= 0;
    end else cntErrorR <= cntErrorR + 1;
  endrule

  // interfaces
  //////////////////////////////////////////////////////////////////////////////

  interface slave = inShim.slave;
  interface master = outShim.master;
  interface management = ctrlShim.slave;

endmodule

endpackage
