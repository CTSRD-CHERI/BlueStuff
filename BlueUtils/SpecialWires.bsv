package SpecialWires; 

export mkDWireOR;
export mkWireOP;

import List :: *;

// DWire where all writes are OR'd together
// Useful when setting fields of a Struct in
// different rules
module mkDWireOR #( Integer n
                  , a_bits init
  ) (Array #(Wire #(a_bits))) provisos (
    Bits #(a_bits, bit_size));

  let v <- mkWireOP (n, mkDWire (init), \| );
  return v;
endmodule

// Use this for OPs on Bits
// Force the op to be on Bit #(w) so that when passing an op on Bits the compiler won't
// complain about the op being of ambiguous type
module mkWireOP #( Integer n
                 , function module #(Wire #(a_bits)) wire_module
                 , function Bit #(bit_size) op (Bit #(bit_size) arg1, Bit #(bit_size) arg2)
  ) (Array #(Wire #(a_bits))) provisos (
    Bits #(a_bits, bit_size));

    let v <- mkWireOP_Core (n, wire_module, op);
    return v;
endmodule

// Only useful in rare cases when using some strange 'op'
// Most generic version, does all of the work for the wrappers
// To use with a generic Bits op (eg \|), must define helper fn. Eg:
// function Bit #(n) or_fn (Bit #(n) arg1, Bit #(n) arg2) = arg1 | arg2;
module mkWireOP_Core #( Integer n
                      , function module #(Wire #(a_bits)) wire_module
                      , function b_bits op (c_bits arg1, d_bits arg2)
  ) (Array #(Wire #(a_bits))) provisos (
    Bits #(a_bits, bit_size)
  , Bits #(b_bits, bit_size)
  , Bits #(c_bits, bit_size)
  , Bits #(d_bits, bit_size));

  if (n<0) error(quote("mkWireOP") + " cannot have a negative number of ports");

  function Bit #(bit_size) op_b (Bit #(bit_size) arg1_b, Bit #(bit_size) arg2_b) =
    pack (op (unpack (arg1_b), unpack (arg2_b)));
  
  List #(Wire #(a_bits)) wires <- replicateM (n, wire_module);
  let bit_wires = map (compose (pack, readReg), wires);
  a_bits op_read = unpack (fold (op_b, bit_wires));

  Wire#(a_bits) v[n];
  for (Integer i = 0; i < n; i = i + 1) begin
    v[i] = interface Wire;
      method _read = op_read;
      method _write = writeReg (wires[i]);
    endinterface;
  end
  return v;
endmodule

endpackage
