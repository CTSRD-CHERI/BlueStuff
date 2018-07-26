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

// BRAM interface

interface BRAM#(numeric type aw, numeric type dw);
  method Action put(Bit#(TDiv#(dw, 8)) we, Bit#(aw) addr, Bit#(dw) d);
  method Bit#(dw) peek();
endinterface

interface BRAM2#(numeric type a0w, numeric type d0w, numeric type a1w, numeric type d1w);
  interface BRAM#(a0w, d0w) p0;
  interface BRAM#(a1w, d1w) p1;
endinterface

/////////////////////////
// Altera BRAM wrapper //
////////////////////////////////////////////////////////////////////////////////

import "BVI" BlueUtils_BRAM2 =
module mkAlteraBRAM2#(Integer size, String filename)(BRAM2#(a0w,d0w,a1w,d1w));

  // XXX check  for bytes / bits
  Integer depth_a = size/valueOf(TDiv#(d0w,8));
  Integer depth_b = size/valueOf(TDiv#(d1w,8));

  // BVI statements
  default_clock clk(CLK, (*unused*) clk_gate);
  default_reset no_reset;

  // parameters
  parameter widthad_a = log2(depth_a);
  parameter widthad_b = log2(depth_b);
  parameter width_a = valueOf(d0w);
  parameter width_b = valueOf(d1w);
  parameter numwords_a = depth_a;
  parameter numwords_b = depth_b;
  parameter width_byteena_a = valueOf(TDiv#(d0w,8));
  parameter width_byteena_b = valueOf(TDiv#(d1w,8));
  parameter init_file = filename;

  interface BRAM p0;
    method put(wen_a, address_a, data_a) enable (en_a);
    method q_a peek();
  endinterface

  interface BRAM p1;
    method put(wen_b, address_b, data_b) enable (en_b);
    method q_b peek();
  endinterface

  // schedule
  schedule (p0.put) CF (p0.peek, p1.put, p1.peek);
  schedule (p0.put) C (p0.put);
  schedule (p0.peek) CF (p0.peek, p0.put, p1.put, p1.peek);
  schedule (p1.put) CF (p1.peek, p0.put, p0.peek);
  schedule (p1.put) C (p1.put);
  schedule (p1.peek) CF (p1.peek, p1.put, p0.put, p0.peek);

endmodule
