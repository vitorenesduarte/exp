# How to reproduce the experiments

#### Prerequisites

- A running Kubernetes cluster
- [`kubectl`](https://github.com/kubernetes/kubectl)
- [Erlang](https://github.com/erlang/otp)
- [R](https://www.r-project.org/)

#### Instructions

From the root of the repository:

```bash
$ make
$ bin/icde19-exp.sh
```

When it ends:
```bash
$ cd evaluation/
$ make all
```

The plots:
```bash
$ ls *.png
```
