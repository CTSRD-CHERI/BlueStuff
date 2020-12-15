/*-
 * Copyright (c) 2020 Jonas Fiala
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

package SpecialRegs;

export mkDRegOR;

import List :: *;
import SpecialWires :: *;

module mkDRegOR #( Integer n
                  , a_bits init
  ) (Array #(Reg #(a_bits))) provisos (
    Bits #(a_bits, bit_size));

  let wires <- mkDWireOR (n, (init));
  Reg #(a_bits) register <- mkReg(unpack(0));

  rule update_reg;
      register <= wires[0];
  endrule

  Reg#(a_bits) ifc[n];
  for (Integer i = 0; i < n; i = i + 1) begin
    ifc[i] = interface Wire;
      method _read = register;
      method _write = writeReg (wires[i]);
    endinterface;
  end
  return ifc;
endmodule

endpackage
