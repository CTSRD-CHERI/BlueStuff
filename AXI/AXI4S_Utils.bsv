package AXI4S_Utils;

import FIFOF :: *;
import SpecialFIFOs :: *;

// BlueBasics import
import SourceSink :: *;

// AXI4 import
import AXI4S_Types :: *;

`define defAXI4SShimFIFOF (name, mkFF)\
module mkAXI4SShim``name (AXI4S_Shim#(a, b, c, d));\
  let  tff <- mkFF;\
  method clear = action\
    tff.clear;\
  endaction;\
  interface master = interface AXI4S_Master;\
    interface t = toSource(tff);\
  endinterface;\
  interface slave = interface AXI4S_Slave;\
    interface t = toSink(tff);\
  endinterface;\
endmodule

`defAXI4SShimFIFOF(BypassFIFOF, mkBypassFIFOF)
`defAXI4SShimFIFOF(BypassFF1, mkSizedBypassFIFOF(1))
`defAXI4SShimFIFOF(FF1, mkFIFOF1)
`defAXI4SShimFIFOF(FF, mkFIFOF)
`defAXI4SShimFIFOF(SizedFIFOF4, mkSizedFIFOF(4))
`defAXI4SShimFIFOF(SizedFIFOF32, mkSizedFIFOF(32))
`defAXI4SShimFIFOF(UGSizedFIFOF32, mkUGSizedFIFOF(32))
`defAXI4SShimFIFOF(UGSizedFIFOF4, mkUGSizedFIFOF(4))




typeclass ToAXI4S_TFlit #( type t
                         , numeric type id_
                         , numeric type data_
                         , numeric type dest_
                         , numeric type user_);
   function AXI4S_TFlit #(id_, data_, dest_, user_) toAXI4S_TFlit (t x);
endtypeclass

instance ToAXI4S_TFlit #(AXI4S_TFlit #(id_, data_, dest_, user_), id_, data_, dest_, user_);
   function toAXI4S_TFlit = id;
endinstance

typeclass FromAXI4S_TFlit #( type t
                           , numeric type id_
                           , numeric type data_
                           , numeric type dest_
                           , numeric type user_);
   function t fromAXI4S_TFlit (AXI4S_TFlit #(id_, data_, dest_, user_) x);
endtypeclass

instance FromAXI4S_TFlit #(AXI4S_TFlit #(id_, data_, dest_, user_), id_, data_, dest_, user_);
   function fromAXI4S_TFlit = id;
endinstance




module toAXI4S_T_Master_Synth #(src_t #(t) s)
                               (AXI4S_T_Master_Synth #(id_, data_, dest_, user_))
   provisos ( ToSource #(src_t #(t), t)
            , ToAXI4S_TFlit #(t, id_, data_, dest_, user_)
            , Bits #(t, t_sz));
   let src <- toUnguardedSource (s, ?);
   AXI4S_TFlit #(id_, data_, dest_, user_) flit = toAXI4S_TFlit (src.peek);
   method tdata   = flit.tdata;
   method tstrb   = flit.tstrb;
   method tkeep   = flit.tkeep;
   method tlast   = flit.tlast;
   method tid     = flit.tid;
   method tdest   = flit.tdest;
   method tuser   = flit.tuser;
   method tvalid  = src.canPeek;
   method tready (rdy) = action if (src.canPeek && rdy) src.drop; endaction;
endmodule


module toAXI4S_T_Slave_Synth #(snk_t s)
                              (AXI4S_T_Slave_Synth #(id_, data_, dest_, user_))
   provisos ( ToSink #(snk_t, t)
            , FromAXI4S_TFlit #(t, id_, data_, dest_, user_)
            , Bits #(t, t_sz));
   let snk <- toUnguardedSink (s);

   method tflit ( tvalid
                , tdata
                , tstrb
                , tkeep
                , tlast
                , tid
                , tdest
                , tuser) =
      action
         if (tvalid && snk.canPut)
            snk.put (fromAXI4S_TFlit (AXI4S_TFlit { tdata: tdata
                                                  , tstrb: tstrb
                                                  , tkeep: tkeep
                                                  , tlast: tlast
                                                  , tid  : tid
                                                  , tdest: tdest
                                                  , tuser: tuser }));
      endaction;
   method tready = snk.canPut;
endmodule

module toAXI4S_Master_Synth #(AXI4S_Master #(id_, data_, dest_, user_) master)
                             (AXI4S_Master_Synth #(id_, data_, dest_, user_));
   let tSynth <- toAXI4S_T_Master_Synth (master.t);
   interface t = tSynth;
endmodule

module toAXI4S_Slave_Synth #(AXI4S_Slave #(id_, data_, dest_, user_) slave)
                            (AXI4S_Slave_Synth #(id_, data_, dest_, user_));
   let tSynth <- toAXI4S_T_Slave_Synth (slave.t);
   interface t = tSynth;
endmodule



function AXI4S_Master #(id_, data_, dest_, user_)
         debugAXI4S_Master (AXI4S_Master #(id_, data_, dest_, user_) m, Fmt msg) =
   interface AXI4S_Master;
      interface t = debugSource (m.t, $format (msg, " t"));
   endinterface;

function AXI4S_Slave #(id_, data_, dest_, user_)
         debugAXI4S_Slave (AXI4S_Slave #(id_, data_, dest_, user_) s, Fmt msg) =
   interface AXI4S_Slave;
      interface t = debugSink (s.t, $format (msg, " t"));
   endinterface;



endpackage
