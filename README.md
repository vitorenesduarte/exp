# lsim

[![Build Status](https://travis-ci.org/vitorenesduarte/lsim.svg?branch=master)](https://travis-ci.org/vitorenesduarte/lsim/)


#### Experiments

- __BRANCH__: which lsim branch should instances run
- __LDB_MODE__:
  - `state_based`
  - `delta_based`
  - `pure_op_based`
- __LDB_DRIVEN_MODE__:
  - `none`
  - `state_driven`
  - `digest_driven`
- __LDB_STATE_SYNC_INTERVAL__: milliseconds
- __LDB_REDUNDANT_DGROUPS__: boolean
- __LDB_DGROUP_BACK_PROPAGATION__: boolean
- __OVERLAY__:
  - `hyparview`
  - `line`
  - `ring`
- __NODE_NUMBER__: number of nodes
- __SIMULATION__:
  - `gcounter`
  - `gset`
- __NODE_EVENT_NUMBER__: number of events to be performed in
the simulation. The event interval is 1 second
- __PARTITION_NUMBER__: number of connected components to be formed
during the simulation. The partition is induced when the simulation
reaches 25% of progress and healed at 75%. With static memberships
it's easy to have control on the number of connected components
created; but it's not the case if using HyParView.


#### Kubernetes

```bash
$ WHAT=run bin/sim.sh
```

- `WHAT=run` will run the experiments without pullling a new image
- `WHAT=build` will build a new image, push it, and the run the experiments with that image
- otherwise, it will use an image that clones the repository in the current branch


##### Tail the logs

```bash
$ kubectl get pods
              READY     STATUS    RESTARTS   AGE
lsim-1488549530072065763-3946360666-0b6d8   0/1       Pending   0
...
$ kubectl logs -f lsim-1488549530072065763-3946360666-0b6d8
```


##### lsim dashboard
```bash
$ bin/dash-proxy.sh
```

This will open a new chrome tab with the dashboard.


#### Docker
To build and push an image:

```bash
$ BRANCH=master \
  IMAGE=vitorenesduarte/lsim \
  DOCKERFILE=Dockerfiles/lsim bin/image.sh
```
