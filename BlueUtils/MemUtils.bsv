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

import MemTypes :: *;
import MemBRAM :: *;
import MemSim :: *;
import AXI :: *;
import SourceSink :: *;
import MasterSlave :: *;
import FIFO :: *;

/////////////////////////////
// Mem to AXI4Lite wrapper //
////////////////////////////////////////////////////////////////////////////////

`define PARAMS addr_sz, data_sz, 0, 0, 0, 0, 0

module mkMemToAXI4Lite_Slave#(Mem#(addr_t, data_t) mem)
  (AXI4Lite_Slave#(`PARAMS))
  provisos (
    Bits#(addr_t, addr_sz), Bits#(data_t, data_sz),
    Log#(TDiv#(data_sz, 8), lowIdx), Add#(a__, lowIdx, data_sz)
  );
  let shim <- mkAXI4LiteShim;
  // which response ?
  let expectWriteRsp <- mkFIFO;
  let readLowAddr <- mkFIFO;
  (* descending_urgency = "readReq, writeReq" *)
  rule writeReq;
    let awflit <- get(shim.master.aw);
    let  wflit <- get(shim.master.w);
    Bit#(lowIdx) lowAddr = awflit.awaddr[valueOf(lowIdx)-1:0];
    Bit#(data_sz) shiftAmount = zeroExtend(lowAddr) << 3;
    mem.sink.put(WriteReq {
      addr: unpack(awflit.awaddr),
      byteEnable: unpack(wflit.wstrb) >> lowAddr,
      data: unpack(wflit.wdata >> shiftAmount)
    });
    expectWriteRsp.enq(True);
  endrule
  rule writeRsp(expectWriteRsp.first);
    expectWriteRsp.deq;
    mem.source.drop;
    shim.master.b.put(AXI4Lite_BFlit{bresp: OKAY, buser: ?});
  endrule
  rule readReq;
    let arflit <- get(shim.master.ar);
    mem.sink.put(ReadReq {
      addr: unpack(arflit.araddr),
      numBytes: fromInteger(valueOf(data_sz)/8)}
    );
    Bit#(lowIdx) lowAddr = arflit.araddr[valueOf(lowIdx)-1:0];
    readLowAddr.enq(lowAddr);
    expectWriteRsp.enq(False);
  endrule
  rule readRsp(!expectWriteRsp.first);
    expectWriteRsp.deq;
    readLowAddr.deq;
    let rsp <- get(mem.source);
    Bit#(data_sz) shiftAmount = zeroExtend(readLowAddr.first) << 3;
    shim.master.r.put(AXI4Lite_RFlit {
      rdata: pack(rsp.ReadRsp) << shiftAmount, rresp: OKAY, ruser: ?
    });
  endrule
  return shim.slave;
endmodule

////////////////
// 1 port Mem //
////////////////////////////////////////////////////////////////////////////////

module mkMem#(Integer size, Maybe#(String) file) (Mem#(addr_t, data_t))
provisos(
  Bits#(addr_t, addr_sz), Bits#(data_t, data_sz),
  Add#(idx_sz, TLog#(TDiv#(data_sz, 8)), addr_sz),
  Log#(TAdd#(1, TDiv#(data_sz, 8)), TAdd#(TLog#(TDiv#(data_sz, 8)), 1))
  // FShow instances
  /*,FShow#(addr_t), FShow#(data_t)*/
);
  if (genC) begin
    Mem#(addr_t, data_t) mem[1] <- mkMemSim(1, size, file);
    return mem[0];
  end else begin
    let fname = "UNUSED";
    if (isValid(file)) fname = file.Valid;
    BRAM#(idx_sz, data_sz) m <- mkAlteraBRAM(size, fname);
    Mem#(addr_t, data_t) mem <- wrapUnaligned("port 0", m);
    return mem;
  end
endmodule

module mkAXI4LiteMem#(Integer size, Maybe#(String) file) (AXI4Lite_Slave#(`PARAMS))
  provisos (
    Log#(TAdd#(1, TDiv#(data_sz, 8)), TAdd#(TLog#(TDiv#(data_sz, 8)), 1)),
    Add#(a__, TLog#(TDiv#(data_sz, 8)), addr_sz),
    Add#(b__, TLog#(TDiv#(data_sz, 8)), data_sz)
  );
  Mem#(Bit#(addr_sz), Bit#(data_sz)) mem <- mkMem(size, file);
  let ifc <- mkMemToAXI4Lite_Slave(mem);
  return ifc;
endmodule

///////////////////////////
// 2 ports shared memory //
////////////////////////////////////////////////////////////////////////////////

module mkSharedMem2#(Integer size, Maybe#(String) file) (Array#(Mem#(addr_t, data_t)))
provisos(
  Bits#(addr_t, addr_sz), Bits#(data_t, data_sz),
  Add#(idx_sz, TLog#(TDiv#(data_sz, 8)), addr_sz),
  Log#(TAdd#(1, TDiv#(data_sz, 8)), TAdd#(TLog#(TDiv#(data_sz, 8)), 1))
  // FShow instances
  /*,FShow#(addr_t), FShow#(data_t)*/
);
  if (genC) begin
    Mem#(addr_t, data_t) mem[2] <- mkMemSim(2, size, file);
    return mem;
  end else begin
    let fname = "UNUSED";
    if (isValid(file)) fname = file.Valid;
    BRAM2#(idx_sz, data_sz, idx_sz, data_sz) m <- mkAlteraBRAM2(size, fname);
    Mem#(addr_t, data_t) mem[2];
    mem[0] <- wrapUnaligned("port0", m.p0);
    mem[1] <- wrapUnaligned("port1", m.p1);
    return mem;
  end
endmodule

module mkAXI4LiteSharedMem2#(Integer size, Maybe#(String) file)
  (Array#(AXI4Lite_Slave#(`PARAMS)))
  provisos (
    Log#(TAdd#(1, TDiv#(data_sz, 8)), TAdd#(TLog#(TDiv#(data_sz, 8)), 1)),
    Add#(a__, TLog#(TDiv#(data_sz, 8)), addr_sz),
    Add#(b__, TLog#(TDiv#(data_sz, 8)), data_sz)
  );
  Mem#(Bit#(addr_sz), Bit#(data_sz)) mem[2] <- mkSharedMem2(size, file);
  AXI4Lite_Slave#(`PARAMS) ifc[2];
  ifc[0] <- mkMemToAXI4Lite_Slave(mem[0]);
  ifc[1] <- mkMemToAXI4Lite_Slave(mem[1]);
  return ifc;
endmodule

`undef PARAMS
