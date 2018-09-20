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

///////////////////////////
// 2 ports shared memory //
////////////////////////////////////////////////////////////////////////////////

module mkSharedMem2#(Integer size, String file) (Array#(Mem#(addr_t, data_t)))
provisos(
  Bits#(addr_t, addr_sz), Bits#(data_t, data_sz),
  Add#(idx_sz, TLog#(TDiv#(data_sz, BitsPerByte)), addr_sz),
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
