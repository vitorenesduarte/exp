### Kubernetes

[__[Original link]__](https://kubernetes.io/docs/getting-started-guides/kubeadm/)

- On all nodes:

```bash
sudo su -
apt-get update && apt-get install -y apt-transport-https
curl -s https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add -
cat <<EOF > /etc/apt/sources.list.d/kubernetes.list
deb http://apt.kubernetes.io/ kubernetes-xenial main
EOF
apt-get update
apt-get install -y docker.io
apt-get install -y kubelet kubeadm kubectl kubernetes-cni
```

- On master (with public ip __192.168.116.101__):
1. __Initialize__:
```bash
kubeadm reset
systemctl start kubelet.service
kubeadm init --api-advertise-addresses 192.168.116.101
```
2. __Save the output__ (`kube join ...`)
3. __Install a pod network__. We're going to install [calico](http://docs.projectcalico.org/v2.0/getting-started/kubernetes/installation/hosted/kubeadm/)
```bash
kubectl apply -f http://docs.projectcalico.org/v2.0/getting-started/kubernetes/installation/hosted/kubeadm/calico.yaml
```
4. __Copy config file__
```bash
cp /etc/kubernetes/admin.conf
chown ubuntu admin.conf
exit
scp ubuntu@192.168.116.101:admin.conf .
```

- On the other nodes:
1. __Join cluster__:
```bash
kubeadm reset
systemctl start kubelet.service
kube join ...
```

- On your machine:
```bash
kubectl --kubeconfig ./admin.conf get nodes
```
