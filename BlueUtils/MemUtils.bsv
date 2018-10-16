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

import MemTypes :: *;
import MemBRAM :: *;
import MemSim :: *;
import AXI :: *;
import SourceSink :: *;
import GetPut :: *;
import ClientServer :: *;
import FIFO :: *;

////////////////////////////
// Mem to AXILite wrapper //
////////////////////////////////////////////////////////////////////////////////

module mkMemToAXILiteSlave#(Mem#(addr_t, data_t) mem)
  (AXILiteSlave#(addr_sz, data_sz))
  provisos (Bits#(addr_t, addr_sz), Bits#(data_t, data_sz));
  let shim <- mkAXILiteShim;
  // which response ?
  let expectWriteRsp <- mkFIFO;
  (* descending_urgency = "readReq, writeReq" *)
  rule writeReq;
    let awflit <- shim.master.aw.get;
    let  wflit <- shim.master.w.get;
    mem.request.put(WriteReq {
      addr: unpack(awflit.awaddr),
      byteEnable: unpack(wflit.wstrb),
      data: unpack(wflit.wdata)
    });
    expectWriteRsp.enq(True);
  endrule
  rule writeRsp(expectWriteRsp.first);
    expectWriteRsp.deq;
    let _ <- mem.response.get;
    shim.master.b.put(BLiteFlit{bresp: OKAY});
  endrule
  rule readReq;
    let arflit <- shim.master.ar.get;
    mem.request.put(ReadReq {
      addr: unpack(arflit.araddr),
      numBytes: fromInteger(valueOf(data_sz)/8)}
    );
    expectWriteRsp.enq(False);
  endrule
  rule readRsp(!expectWriteRsp.first);
    expectWriteRsp.deq;
    let rsp <- mem.response.get;
    shim.master.r.put(RLiteFlit{rdata: pack(rsp.ReadRsp), rresp: OKAY});
  endrule
  return shim.slave;
endmodule

////////////////
// 1 port Mem //
////////////////////////////////////////////////////////////////////////////////

module mkMem#(Integer size, String file) (Mem#(addr_t, data_t))
provisos(
  Bits#(addr_t, addr_sz), Bits#(data_t, data_sz),
  Add#(idx_sz, TLog#(TDiv#(data_sz, 8)), addr_sz),
  Log#(TAdd#(1, TDiv#(data_sz, 8)), TAdd#(TLog#(TDiv#(data_sz, 8)), 1)),
  // FShow instances
  FShow#(addr_t), FShow#(data_t)
);
  if (genC) begin
    Mem#(addr_t, data_t) mem[1] <- mkMemSim(1, size, file);
    return mem[0];
  end else begin
    BRAM#(idx_sz, data_sz) m <- mkAlteraBRAM(size, file);
    Mem#(addr_t, data_t) mem <- wrapUnaligned("port 0", m);
    return mem;
  end
endmodule

module mkAXILiteMem#(Integer size, String file) (AXILiteSlave#(a_sz, d_sz))
  provisos (
    Log#(TAdd#(1, TDiv#(d_sz, 8)), TAdd#(TLog#(TDiv#(d_sz, 8)), 1)),
    Add#(a__, TLog#(TDiv#(d_sz, 8)), a_sz)
  );
  Mem#(Bit#(a_sz), Bit#(d_sz)) mem <- mkMem(size, file);
  let ifc <- mkMemToAXILiteSlave(mem);
  return ifc;
endmodule

///////////////////////////
// 2 ports shared memory //
////////////////////////////////////////////////////////////////////////////////

module mkSharedMem2#(Integer size, String file) (Array#(Mem#(addr_t, data_t)))
provisos(
  Bits#(addr_t, addr_sz), Bits#(data_t, data_sz),
  Add#(idx_sz, TLog#(TDiv#(data_sz, 8)), addr_sz),
  Log#(TAdd#(1, TDiv#(data_sz, 8)), TAdd#(TLog#(TDiv#(data_sz, 8)), 1)),
  // FShow instances
  FShow#(addr_t), FShow#(data_t)
);
  if (genC) begin
    Mem#(addr_t, data_t) mem[2] <- mkMemSim(2, size, file);
    return mem;
  end else begin
    BRAM2#(idx_sz, data_sz, idx_sz, data_sz) m <- mkAlteraBRAM2(size, file);
    Mem#(addr_t, data_t) mem[2];
    mem[0] <- wrapUnaligned("port0", m.p0);
    mem[1] <- wrapUnaligned("port1", m.p1);
    return mem;
  end
endmodule

module mkAXILiteSharedMem2#(Integer size, String file)
  (Array#(AXILiteSlave#(addr_sz, data_sz)))
  provisos (
    Log#(TAdd#(1, TDiv#(data_sz, 8)), TAdd#(TLog#(TDiv#(data_sz, 8)), 1)),
    Add#(a__, TLog#(TDiv#(data_sz, 8)), addr_sz)
  );
  Mem#(Bit#(addr_sz), Bit#(data_sz)) mem[2] <- mkSharedMem2(size, file);
  AXILiteSlave#(addr_sz, data_sz) ifc[2];
  ifc[0] <- mkMemToAXILiteSlave(mem[0]);
  ifc[1] <- mkMemToAXILiteSlave(mem[1]);
  return ifc;
endmodule
