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

package AXI4_Avalon;

import FIFOF :: *;
import SpecialFIFOs :: *;
import GetPut :: *;
import ClientServer :: *;
import Connectable :: *;
import SourceSink :: *;

import BlueAXI4 :: *;
import BlueAvalon :: *;

function MemAccessPacketT #(waddr_, data_) axi4WriteReq2AvalonWriteReq
  ( AXI4_AWFlit #(id_, addr_, awuser_) awflit
  , AXI4_WFlit #(data_, wuser_) wflit )
  provisos ( Log #(TDiv #(data_, 8), offset_bits_)
           , Add #(waddr_, offset_bits_, addr_) ) =
  MemAccessPacketT { rw: MemWrite
                   , addr: unpack (truncateLSB (awflit.awaddr))
                   , data: unpack (wflit.wdata)
                   , arbiterlock: False
                   , byteenable: wflit.wstrb };

function MemAccessPacketT #(waddr_, data_) axi4ReadReq2AvalonReadReq
  (AXI4_ARFlit #(id_, addr_, aruser_) arflit)
  provisos ( Log #(TDiv #(data_, 8), offset_bits_)
           , Add #(waddr_, offset_bits_, addr_) ) =
  MemAccessPacketT { rw: MemRead
                   , addr: unpack (truncateLSB (arflit.araddr))
                   , data: ?
                   , arbiterlock: False
                   , byteenable: ? };

function AXI4_RFlit #(id_, data_, ruser_) avalonReadRsp2AXI4ReadRsp
  (Bit #(id_) rid, Bit #(data_) data) =
  AXI4_RFlit { rid: rid
             , rdata: data
             , rresp: OKAY
             , rlast: True
             , ruser: ? };

module mkAXI4Manager_to_AvalonHost #(AXI4_Master #( id_, addr_, data_
                                                  , awuser_, wuser_, buser_
                                                  , aruser_, ruser_) axm)
  (AvalonMasterIfc #(waddr_, data_))
  provisos ( Log #(TDiv #(data_, 8), offset_bits_)
           , Add #(waddr_, offset_bits_, addr_)
           , Add #(_a, SizeOf #(AXI4_Len), addr_) );
  // deburst the axi manager
  AXI4_Shim #(id_, addr_, data_ , awuser_, wuser_, buser_ , aruser_, ruser_)
    deBurst <- mkAXI4DeBurst;
  mkConnection (axm, deBurst.slave);
  // convert axi traffic into avalon traffic
  let s2avm <- mkServer2AvalonMaster;
  let observableRsp <- mkBypassFIFOF;
  rule observeRsp;
    let rsp <- s2avm.server.response.get;
    observableRsp.enq (rsp);
  endrule
  let write_id_ff <- mkFIFOF;
  let read_id_ff <- mkFIFOF;
  rule forward_write_req;
    let awflit <- get (deBurst.master.aw);
    let  wflit <- get (deBurst.master.w);
    s2avm.server.request.put (axi4WriteReq2AvalonWriteReq (awflit, wflit));
    write_id_ff.enq (awflit.awid);
  endrule
  rule forward_write_rsp (!isValid (observableRsp.first));
    observableRsp.deq;
    let bid <- get (write_id_ff);
    deBurst.master.b.put (AXI4_BFlit {bid: bid, bresp: OKAY, buser: ?});
  endrule
  rule forward_read_req;
    let arflit <- get (deBurst.master.ar);
    s2avm.server.request.put (axi4ReadReq2AvalonReadReq (arflit));
    read_id_ff.enq (arflit.arid);
  endrule
  rule forward_read_rsp (isValid (observableRsp.first));
    let rsp = observableRsp.first;
    observableRsp.deq;
    let rid <- get (read_id_ff);
    deBurst.master.r.put (avalonReadRsp2AXI4ReadRsp (rid, pack (rsp.Valid)));
  endrule
  // return the avalon master side of the converter
  return s2avm.avm;
endmodule

endpackage
