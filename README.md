# lsim

[![Build Status](https://travis-ci.org/vitorenesduarte/lsim.svg?branch=master)](https://travis-ci.org/vitorenesduarte/lsim/)


#### Experiments

- __BRANCH__: which lsim branch should instances run
- __LDB_MODE__:
  - state_based
  - delta_based
  - pure_op_based
- __LDB_REDUNDANT_DGROUPS__: boolean
- __LDB_DGROUP_BACK_PROPAGATION__: boolean
- __LDB_METRICS__: boolean
- __OVERLAY__:
  - hyparview
  - line
  - ring
- __NODE_NUMBER__: number of nodes
- __SIMULATION__:
  - gcounter
  - gset
- __NODE_EVENT_NUMBER__: number of events to be performed in
the simulation. The event interval is 1 second


#### Kubernetes

```bash
$ BRANCH=master \
  LDB_MODE=delta_based \
  LDB_REDUNDANT_DGROUPS=true \
  LDB_DGROUP_BACK_PROPAGATION=true \
  OVERLAY=hyparview \
  SIMULATION=gset \
  NODE_NUMBER=13 \
  NODE_EVENT_NUMBER=10 ./bin/lsim-deploy.sh
```

```bash
$ kubectl get pods
              READY     STATUS    RESTARTS   AGE
lsim-1488549530072065763-3946360666-0b6d8   0/1       Pending   0
...
$ kubectl logs -f lsim-1488549530072065763-3946360666-0b6d8
```


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

