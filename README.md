# lsim

[![Build Status](https://travis-ci.org/vitorenesduarte/lsim.svg?branch=master)](https://travis-ci.org/vitorenesduarte/lsim/)


#### Docker
To build an image:

```bash
$ cd Dockerfiles/
$ docker build -t vitorenesduarte/lsim -f lsim  .
```

To push it to [Docker Hub](https://hub.docker.com/):

```bash
$ docker push vitorenesduarte/lsim
```


#### Experiments

- __LDB_MODE__:
  - state_based
  - delta_based
  - pure_op_based
- __LDB_JOIN_DECOMPOSITIONS__: when set to _true_, applies
join-decompositions to the received delta groups (this will only
have an effect if __LBD_MODE=delta_based__)
- __BRANCH__: which lsim branch should instances run
- __LSIM_OVERLAY__: if hyparview is set, partisan will be used; otherwise
a static peer service will be used
  - line
  - ring
  - erdos_renyi
  - hyparview
- __LSIM_NODE_NUMBER__: Check __src/lsim_overlay.erl__ if using
a static overlay, to see which number of nodes is supported for that
overlay
- __LSIM_SIMULATION__:
  - gcounter
  - gset
- __LSIM_NODE_EVENT_NUMBER__: number of events to be performed in
the simulation. The event interval is 1 second
- __LSIM_SIMULATION_TS__: Unique timestamp across all simulations

