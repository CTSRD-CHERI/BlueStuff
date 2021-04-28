package AXI4S_Types;

import Connectable :: *;

// BlueBasics import
import SourceSink :: *;

import AXI4_Types :: *;

typedef struct {
   Bit #(data_)            tdata;
   Bit #(TDiv #(data_, 8)) tstrb;
   Bit #(TDiv #(data_, 8)) tkeep;
   Bool                    tlast;
   Bit #(id_)              tid;
   Bit #(dest_)            tdest;
   Bit #(user_)            tuser;
} AXI4S_TFlit #(numeric type id_, numeric type data_, numeric type dest_, numeric type user_)
deriving (Bits, FShow);

(* always_ready, always_enabled *)
interface AXI4S_T_Master_Synth #(
   numeric type id_,
   numeric type data_,
   numeric type dest_,
   numeric type user_);
   method Bit #(data_)            tdata;
   method Bit #(TDiv #(data_, 8)) tstrb;
   method Bit #(TDiv #(data_, 8)) tkeep;
   method Bool                    tlast;
   method Bit #(id_)              tid;
   method Bit #(dest_)            tdest;
   method Bit #(user_)            tuser;
   method Bool                    tvalid;
   (* prefix="" *) method Action  tready (Bool tready);
endinterface

(* always_ready, always_enabled *)
interface AXI4S_T_Slave_Synth #(
   numeric type id_,
   numeric type data_,
   numeric type dest_,
   numeric type user_);
   (* prefix="" *) method Action tflit ( Bool                    tvalid
                                       , Bit #(data_)            tdata
                                       , Bit #(TDiv #(data_, 8)) tstrb
                                       , Bit #(TDiv #(data_, 8)) tkeep
                                       , Bool                    tlast
                                       , Bit #(id_)              tid
                                       , Bit #(dest_)            tdest
                                       , Bit #(user_)            tuser);
   method Bool tready;
endinterface

interface AXI4S_Master #(
   numeric type id_,
   numeric type data_,
   numeric type dest_,
   numeric type user_);
   interface Source #(AXI4S_TFlit #(id_, data_, dest_, user_)) t;
endinterface

interface AXI4S_Master_Synth #(
   numeric type id_,
   numeric type data_,
   numeric type dest_,
   numeric type user_);
   interface AXI4S_T_Master_Synth #(id_, data_, dest_, user_) t;
endinterface

instance CulDeSac#(AXI4S_Master#(id_, data_, dest_, user_));
  function culDeSac = interface AXI4S_Master;
    interface t = nullSource;
  endinterface;
endinstance

interface AXI4S_Slave #(
   numeric type id_,
   numeric type data_,
   numeric type dest_,
   numeric type user_);
   interface Sink #(AXI4S_TFlit #(id_, data_, dest_, user_)) t;
endinterface

interface AXI4S_Slave_Synth #(
   numeric type id_,
   numeric type data_,
   numeric type dest_,
   numeric type user_);
   interface AXI4S_T_Slave_Synth #(id_, data_, dest_, user_) t;
endinterface

instance CulDeSac#(AXI4S_Slave#(id_, data_, dest_, user_));
  function culDeSac = interface AXI4S_Slave;
    interface t = nullSink;
  endinterface;
endinstance

instance Connectable#(
  AXI4S_Master#(a, b, c, d),
  AXI4S_Slave#(a, b, c, d));
  module mkConnection#(
    AXI4S_Master#(a, b, c, d) m,
    AXI4S_Slave#(a, b, c, d) s)
    (Empty);
    mkConnection(m.t, s.t);
  endmodule
endinstance
instance Connectable#(
  AXI4S_Slave#(a, b, c, d),
  AXI4S_Master#(a, b, c, d));
  module mkConnection#(
    AXI4S_Slave#(a, b, c, d) s,
    AXI4S_Master#(a, b, c, d) m)
    (Empty);
    mkConnection(m, s);
  endmodule
endinstance

interface AXI4S_Shim#(
  numeric type id_,
  numeric type data_,
  numeric type dest_,
  numeric type user_);
  method Action clear;
  interface AXI4S_Master#(
    id_, data_, dest_, user_
  ) master;
  interface AXI4S_Slave#(
    id_, data_, dest_, user_
  ) slave;
endinterface


endpackage
