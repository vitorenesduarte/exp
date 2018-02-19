# How to reproduce the experiments

#### Prerequisites

- A running Kubernetes cluster
- [`kubectl`](https://github.com/kubernetes/kubectl)
- [Erlang](https://github.com/erlang/otp)
- [R](https://www.r-project.org/)

From the root of the repository:

```bash
$ bin/europar18-exp.sh
```

When it ends:
```bash
$ cd evaluation/
$ make europar
$ ls *.png
plot0.png  plot1.png  plot2.png  plot3.png  plot4.png
```
