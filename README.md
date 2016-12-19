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
join-decompositions to the received delta buffers (this will only
have an effect if __LBD_MODE=delta_based__)
- __LDB_EXTENDED_LOGGING__: _true_/_false_
- __BRANCH__: which lsim branch should instances run
- __OVERLAY__:
  - line
  - ring
  - hyparview
  - erdos_renyi
- __NODE_NUMBER__: number of LDBs nodes. Since the overlays are
not yet created in runtime, only __3__ and __13__ nodes for some of
the overlays. Check __src/lsim_overlay.erl__
- __SIMULATION__:
  - TODO
- __SIMULATION_IDENTIFIER__: set this with one of the following
values, depending on which simulation you are running. This id will
be later used to generate the graphs with proper labels.
  - state_based_$OVERLAY
  - delta_based_$OVERLAY
  - pure_op_based_$OVERLAY
  - join_decompositions_$OVERLAY
- __SIMULATION_TIMESTAMP__: When running concurrent experiments
in the cluster, this timestamp should be unique.


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
3> ldb_pull_logs:go(PublicHost, Port).
ok
4>
```
