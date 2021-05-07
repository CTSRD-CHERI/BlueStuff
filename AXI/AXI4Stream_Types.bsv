package AXI4Stream_Types;

import Connectable :: *;

// BlueBasics import
import SourceSink :: *;

import AXI4_AXI4Lite_AXI4Stream_Types :: *;

typedef struct {
  Bit #(data_)            tdata;
  Bit #(TDiv #(data_, 8)) tstrb;
  Bit #(TDiv #(data_, 8)) tkeep;
  Bool                    tlast;
  Bit #(id_)              tid;
  Bit #(dest_)            tdest;
  Bit #(user_)            tuser;
} AXI4Stream_Flit #( numeric type id_
                   , numeric type data_
                   , numeric type dest_
                   , numeric type user_)
deriving (Bits, FShow);

(* always_ready, always_enabled *)
interface AXI4Stream_Master_Synth #( numeric type id_
                                   , numeric type data_
                                   , numeric type dest_
                                   , numeric type user_);
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
interface AXI4Stream_Slave_Synth #( numeric type id_
                                  , numeric type data_
                                  , numeric type dest_
                                  , numeric type user_);
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

typedef Source #(AXI4Stream_Flit #(id_, data_, dest_, user_))
        AXI4Stream_Master #( numeric type id_
                           , numeric type data_
                           , numeric type dest_
                           , numeric type user_);

typedef Sink #(AXI4Stream_Flit #(id_, data_, dest_, user_))
        AXI4Stream_Slave #( numeric type id_
                          , numeric type data_
                          , numeric type dest_
                          , numeric type user_);


instance CulDeSac#(AXI4Stream_Master #(id_, data_, dest_, user_));
  function culDeSac = nullSource;
endinstance

instance CulDeSac#(AXI4Stream_Slave #(id_, data_, dest_, user_));
  function culDeSac = nullSink;
endinstance

interface AXI4Stream_Shim #( numeric type id_
                           , numeric type data_
                           , numeric type dest_
                           , numeric type user_);
  method Action clear;
  interface AXI4Stream_Master #(id_, data_, dest_, user_) master;
  interface AXI4Stream_Slave #(id_, data_, dest_, user_) slave;
endinterface


endpackage
