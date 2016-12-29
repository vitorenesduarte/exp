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

## Remote experiments in DCOS
First authenticate to the DCOS cluster with:

```bash
$ dcos config set core.dcos_url http://$DCOS
$ dcos auth login
```

Once authenticated, launch the Mongo instance
(this is where __lsim__ instances will push their logs).

```bash
$ cd bin/
$ ./mongo-deploy.sh
```

Last but not least, launch __lsim__ instances.
```bash
$ cd bin/
$ ./lsim-deploy.sh
```

In order to execute this last script, a set of environment variables
have to be defined:

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
a static overlay, to see each number of nodes is supported.
- __LSIM_SIMULATION__:
  - gcounter
  - gset
- __LSIM_NODE_EVENT_NUMBER__: number of events to be performed in
the simulation. The event interval is 1 second.
- __LSIM_SIMULATION_TS__: Unique timestamp across all simulations


To see the results of the experiments, firstly you need to pull logs
from Mongo instance.

- Find Mongo and __private host__ and __port__ with

```bash
$ bin/get-mongo-config.sh
```

- Find the respective __public host__ and

```bash
$ make shell
1> PublicHost = "192.168.116.148".
"192.168.116.148"
2> Port = 1756.
1756
3> lsim_pull_logs:go(PublicHost, Port).
ok
4>
```
