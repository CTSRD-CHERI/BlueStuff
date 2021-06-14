Bluestuff's [`AXI4_Common_Types`](AXI4_Common_Types.bsv) and [AXI4_Types`](AXI4_Types.bsv) packages provide a set of types and interface capturing the AXI4 protocol described in the specification in
> AMBA AXI and ACE Protocol Specification
> AXI3, AXI4, AXI5, ACE and ACE5
> ARM IHI 0022F.b (ID122117)

# AXI4_Common_Types
- To capture AXI4 burst length (A3-48), the `AXI4_Len` type
- To capture AXI4 burst size (A3-49), the `AXI4_Size` type
- To capture AXI4 burst type (A3-49), the `AXI4_Burst` type
- To capture AXI4 locked accesses (A7-101), the `AXI4_Lock` type
- To capture AXI4 memory types (A4-69), the `AXI4_Cache` type
- To capture AXI4 access permissions (A4-75), the `AXI4_Prot` type
- To capture AXI4 QoS signaling (A8-104), the `AXI4_QoS` type
- To capture AXI4 multiple region signaling (A8-105), the `AXI4_Region` type
- To capture AXI4 read and write response structure (A3-59), the `AXI4_Resp` type

# AXI4_Types

There are 5 AXI4 channels:
- The AW (Address Write) channel for requests from master to slave
- The W (Write Data) channel for requests from master to slave
- The B (Write Response) channel for responses from slave to master
- The AR (Address Read) channel for requests from master to slave
- The R (Read Response) channel for responses from slave to master

For each of these channels, we define a `AXI4_*Flit` type. These types only specify fields relevant to their payload, and control flow is enforced leveraging the more fundamental `Source` and `Sink` interfaces. For boundaries of a design that want to export an AXI interface down to Verilog, we also provide a `AXI4_*_Master_Synth` interface and a `AXI4_*_Slave_Synth` interface.

We provide for AXI4 masters the following interfaces:
- ```bsv
  interface AXI4_Master#(numeric type id_,
                         numeric type addr_,
                         numeric type data_,
                         numeric type awuser_,
                         numeric type wuser_,
                         numeric type buser_,
                         numeric type aruser_,
                         numeric type ruser_);
    (* prefix = "" *) interface Source#(AXI4_AWFlit#(id_, addr_, awuser_)) aw;
    (* prefix = "" *) interface Source#(AXI4_WFlit#(data_, wuser_)) w;
    (* prefix = "" *) interface Sink#(AXI4_BFlit#(id_, buser_)) b;
    (* prefix = "" *) interface Source#(AXI4_ARFlit#(id_, addr_, aruser_)) ar;
    (* prefix = "" *) interface Sink#(AXI4_RFlit#(id_, data_, ruser_)) r;
  endinterface
  ```
- ```bsv
  interface AXI4_Master_Synth#(numeric type id_,
                               numeric type addr_,
                               numeric type data_,
                               numeric type awuser_,
                               numeric type wuser_,
                               numeric type buser_,
                               numeric type aruser_,
                               numeric type ruser_);
    (* prefix = "" *) interface AXI4_AW_Master_Synth#(id_, addr_, awuser_) aw;
    (* prefix = "" *) interface AXI4_W_Master_Synth#(data_, wuser_) w;
    (* prefix = "" *) interface AXI4_B_Master_Synth#(id_, buser_) b;
    (* prefix = "" *) interface AXI4_AR_Master_Synth#(id_, addr_, aruser_) ar;
    (* prefix = "" *) interface AXI4_R_Master_Synth#(id_, data_, ruser_) r;
  endinterface
  ```

We provide for AXI4 slaves the following interfaces:
- ```bsv
  interface AXI4_Slave#(numeric type id_,
                        numeric type addr_,
                        numeric type data_,
                        numeric type awuser_,
                        numeric type wuser_,
                        numeric type buser_,
                        numeric type aruser_,
                        numeric type ruser_);
    interface Sink#(AXI4_AWFlit#(id_, addr_, awuser_)) aw;
    interface Sink#(AXI4_WFlit#(data_, wuser_)) w;
    interface Source#(AXI4_BFlit#(id_, buser_)) b;
    interface Sink#(AXI4_ARFlit#(id_, addr_, aruser_)) ar;
    interface Source#(AXI4_RFlit#(id_, data_, ruser_)) r;
  endinterface
  ```
- ```bsv
  interface AXI4_Slave_Synth#(numeric type id_,
                              numeric type addr_,
                              numeric type data_,
                              numeric type awuser_,
                              numeric type wuser_,
                              numeric type buser_,
                              numeric type aruser_,
                              numeric type ruser_);
    (* prefix = "" *) interface AXI4_AW_Slave_Synth#(id_, addr_, awuser_) aw;
    (* prefix = "" *) interface AXI4_W_Slave_Synth#(data_, wuser_) w;
    (* prefix = "" *) interface AXI4_B_Slave_Synth#(id_, buser_) b;
    (* prefix = "" *) interface AXI4_AR_Slave_Synth#(id_, addr_, aruser_) ar;
    (* prefix = "" *) interface AXI4_R_Slave_Synth#(id_, data_, ruser_) r;
  endinterface
  ```

We provide instances  of `Connectable` for the pair of `AXI4_Master` and `AXI4_Slave`.

We provide instances  of `Connectable` for the pair of `AXI4_Master_Synth` and `AXI4_Slave_Synth`.

## AXI4 Address Write Channel (AW)
- ```bsv
  typedef struct {
    Bit#(id_) awid;
    Bit#(addr_) awaddr;
    AXI4_Len awlen;
    AXI4_Size awsize;
    AXI4_Burst awburst;
    AXI4_Lock awlock;
    AXI4_Cache awcache;
    AXI4_Prot awprot;
    AXI4_QoS awqos;
    AXI4_Region awregion;
    Bit#(user_) awuser;
  } AXI4_AWFlit#(numeric type id_, numeric type addr_, numeric type user_)
  ```
  The `AXI4_AWFlit` type is an instance of `Routable` and of `DetectLast`.
- ```bsv
  (* always_ready, always_enabled *)
  interface AXI4_AW_Master_Synth#(numeric type id_,
                                  numeric type addr_,
                                  numeric type user_);
    method Bit#(id_) awid;
    method Bit#(addr_) awaddr;
    method AXI4_Len awlen;
    method AXI4_Size awsize;
    method AXI4_Burst awburst;
    method AXI4_Lock awlock;
    method AXI4_Cache awcache;
    method AXI4_Prot awprot;
    method AXI4_QoS awqos;
    method AXI4_Region awregion;
    method Bit#(user_) awuser;
    method Bool awvalid;
    (* prefix="" *) method Action awready(Bool awready);
  endinterface
  ```
  The `AXI4_AW_Master_Synth` type is an instance of `CulDeSac`.
- ```bsv
  interface AXI4_AW_Slave_Synth#(numeric type id_,
                                 numeric type addr_,
                                 numeric type user_);
    (* always_ready, enable="awvalid", prefix="" *)
    method Action awflit (Bit#(id_) awid,
                          Bit#(addr_) awaddr,
                          AXI4_Len awlen,
                          AXI4_Size awsize,
                          AXI4_Burst awburst,
                          AXI4_Lock awlock,
                          AXI4_Cache awcache,
                          AXI4_Prot awprot,
                          AXI4_QoS awqos,
                          AXI4_Region awregion,
                          Bit#(user_) awuser);
    (* always_ready, always_enabled *)
    method Bool awready;
  endinterface
  ```
  The `AXI4_AW_Slave_Synth` type is an instance of `CulDeSac`.
- We provide instances  of `Connectable` for the pair of `AXI4_AW_Master_Synth` and `AXI4_AW_Slave_Synth`.
