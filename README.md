# exp 

[![Build Status](https://img.shields.io/travis/vitorenesduarte/exp/master.svg)](https://travis-ci.org/vitorenesduarte/exp)
[![Coverage Status](https://img.shields.io/coveralls/github/vitorenesduarte/exp/master.svg?maxAge=60)](https://coveralls.io/github/vitorenesduarte/exp?branch=master)


#### Experiments

- __LDB_MODE__:
  - `state_based`
  - `delta_based`
  - `scuttlebutt`
- __LDB_STATE_SYNC_INTERVAL__: milliseconds
- __LDB_REDUNDANT_DGROUPS__: boolean
- __LDB_DGROUP_BACK_PROPAGATION__: boolean
- __NODE_NUMBER__: number of nodes
- __OVERLAY__:
  - `fullmesh`
  - `line`
  - `ring`
  - `partialmesh`
  - `tree`
- __SIMULATION__:
  - `gcounter`
  - `gset`
  - `gmap`
  - `retwis`
- __NODE_EVENT_NUMBER__: number of events to be performed in
the simulation
- __EVENT_INTERVAL__: milliseconds between events
- __GMAP_SIMULATION_KEY_PERCENTAGE__: percentage of keys update at each event in the gmap simulation
- __RETWIS_ZIPF__: Zipf coefficient to be used in
the Retwis application


#### Kubernetes

```bash
$ bin/exp.sh build
$ bin/exp.sh
```

- `build` will build a new image, push it, and the run the experiments with that image
- if no argument, the experiments will be run using the last pushed image


#### Google Cloud Platform

- To start and stop the cluster:

```bash
$ bin/g-cluster.sh start
$ bin/g-cluster.sh stop
```

##### Tail the logs

```bash
$ kubectl get pods
              READY     STATUS    RESTARTS   AGE
exp-1488549530072065763-3946360666-0b6d8   0/1       Pending   0
...
$ kubectl logs -f exp-1488549530072065763-3946360666-0b6d8
```


##### dashboard

- To start the dashboard:
```bash
$ bin/lsim-dash-deploy.sh
```

- To open the dashboard:

```bash
$ bin/dash-proxy.sh
```

This will open a new chrome tab with the dashboard.
