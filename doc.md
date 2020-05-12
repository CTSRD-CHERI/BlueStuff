# OneHotArbiter
Bluestuff's [`OneHotArbiter`](OneHotArbiter.bsv) package provides:
- an `Arbitrable#(type a)` class with a`function Bool isRequesting(a x)` method together with instances for `Bool`, `FIFOF#(x)` and `Source#(x)`
- a `OneHotArbiter` interface with a `method ActionValue#(List#(Bool)) next()` method
-  a generic one-hot arbiter `module mkOneHotArbiter#(List#(t) xs) (OneHotArbiter) provisos (Arbitrable#(t))` module which takes a list of `Arbitrable`s to arbitrate between as an argument, and can return the next elected element via its `next()` method using a rotating priority

# Routable
Bluestuff's [`Routable`](Routable.bsv) package provides:
- a `NoRouteFoundIfc#(type req_t, type rsp_t)` interface for a default routing target reached when no other route was found, with
  * a `method Action pushReq (req_t req)` method
  * a `method ActionValue#(Tuple2#(Bool, rsp_t)) getRsp` method (the `Bool` tells whether the returned flit is the last)
- a `Routable#(type a, type b, type c) dependencies (a determines (b, c))` class with
  * a `function c routingField (a val)` method to extract a field to route with
  * a `module mkNoRouteFound (NoRouteFoundIfc#(a, b))` module capturing desired behaviour in case of absence of viable route
- a `DetectLast#(type a)` class with a `Bool detectLast (a val)` method
- an `ExpandReqRsp` class:
  ```bsv
  typeclass ExpandReqRsp#(type req_a, type req_b, type rsp_a, type rsp_b, type t)
    dependencies (
      (req_a, req_b, t) determines (rsp_a, rsp_b),
      rsp_a determines (req_a, req_b, rsp_b, t)
    );
    function req_b expand(req_a r, t x);
    function Tuple2#(rsp_b, t) shrink(rsp_a r);
  endtypeclass
  ```
  This class should be implemented by request types whose "id fields" needs to be expanded by interconnect logic in order to later route responses (as required for instance by the `mkTwoWayBus` module provided in the [`Interconnect`](Interconnect.bsv) package).
- a `Range#(numeric type n)` type with two fields `Bit#(n) base` and `Bit#(n) size` an associated functions:
  * `function Bit#(n) rangeBase(Range#(n) range)`
  * `function Bit#(n) rangeSize(Range#(n) range)`
  * `function Bit#(n) rangeTop (Range#(n) range)`
  * `function Bool inRange(Range#(n) range, Bit#(n) addr)`
- a `MappingTable#(numeric type n, numeric type a)` type which consists in a `Vector#(n, Range#(a)` and an associated `function Vector#(n, Bool) routeFromMappingTable (MappingTable#(n, a) mt, Bit#(a) addr)` function which looks for the given address in each memory range in the mapping table and returns the index of the range that hit as a one-hot value (or 0 for no hit).

# Interconnect
All buses provided in Bluestuff's [`Interconnect`](Interconnect.bsv) package support multi-flit transactions. The package provides:
- a `mkOneWayBus` module:
  ```bsv
  module mkOneWayBus#(
    Vector#(nIns, Tuple2#(in_t, path_t)) ins,
    Vector#(nOuts, out_t#(flit_t)) outs
  ) (Empty) provisos (
    Bits#(flit_t, flit_sz), DetectLast#(flit_t),
    ToSource#(in_t, flit_t),
    ToSource#(path_t, Vector#(nOuts, Bool)),
    ToSink#(out_t#(flit_t), flit_t),
    // assertion on argument sizes
    Add#(1, c__, nIns), // at least one source is needed
    Add#(1, d__, nOuts) // at least one sink is needed
  );
  ```
    This module connects a vector of sources with a vector of sinks. The sources produce some payload together with their desired destination used to select the appropriate sink. Only a single source can be connected to a single sink at any given time. This bus achieves this by using an instance of the arbiter provided in the [`OneHotArbiter`](OneHotArbiter.bsv) package.
- a `mkTwoWayBus` module:
  ```bsv
  module mkTwoWayBus#(
  function Vector#(nRoutes, Bool) route (routing_t val),
  Vector#(nMasters, Master#(m2s_a, s2m_b)) ms,
  Vector#(nSlaves, Slave#(m2s_b, s2m_a)) ss
  ) (Empty) provisos (
  Bits#(m2s_a, m2s_a_sz), Bits#(s2m_b, s2m_b_sz),
  Bits#(m2s_b, m2s_b_sz), Bits#(s2m_a, s2m_a_sz),
  ExpandReqRsp#(m2s_a, m2s_b, s2m_a, s2m_b, Bit#(TLog#(nMasters))),
  Routable#(m2s_a, s2m_b, routing_t),
  DetectLast#(m2s_b), DetectLast#(s2m_b),
  // assertion on argument sizes
  Add#(1, a__, nMasters), // at least one Master is needed
  Add#(1, b__, nSlaves), // at least one slave is needed
  Add#(nRoutes, 0, nSlaves) // nRoutes == nSlaves
  );
  ```
  This module connects a vector of masters with a vector of slaves using a provided routing function. The masters are wrapped such that they provide a source of requests together with a desired destination (using the provided routing function) and a sink of response. The slaves are wrapped such that they provide a sink of "large" requests (with additional provenance information) and a source of responses together with the additional provenance information (see the `ExpandReqRsp` class in the [`Routable`](Routable.bsv) package). The pairs of sources and sinks are connected using two `mkOneWayBus`s.
- a `mkInOrderTwoWayBus` which imposes in order delivery of requests and responses on a `mkTwoWayBus`.
