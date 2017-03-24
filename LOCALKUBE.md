### Kubernetes on CentOS 7

[__[Original link]__](https://kubernetes.io/docs/getting-started-guides/kubeadm/)

- On all nodes:

```bash
vi d.sh
```

```bash
cat <<EOF > /etc/yum.repos.d/kubernetes.repo
[kubernetes]
name=Kubernetes
baseurl=http://yum.kubernetes.io/repos/kubernetes-el7-x86_64
enabled=1
gpgcheck=1
repo_gpgcheck=1
gpgkey=https://packages.cloud.google.com/yum/doc/yum-key.gpg
       https://packages.cloud.google.com/yum/doc/rpm-package-key.gpg
EOF
setenforce 0
yum install -y docker kubelet kubeadm kubectl kubernetes-cni
systemctl enable docker && systemctl start docker
systemctl enable kubelet && systemctl start kubelet
```

```bash
sudo bash d.sh
```

- On master (with public ip __192.168.116.101__):
1. __Initialize__:
```bash
sudo kubeadm init --api-advertise-addresses 192.168.116.101
```
2. __Save the output__ (`kube join ...`)
3. __Install a pod network__
```bash
kubectl apply -f https://git.io/weave-kube
```
4. __Copy config file__
```bash
sudo cp /etc/kubernetes/admin.conf .
sudo chown ubuntu admin.conf
exit
scp ubuntu@192.168.116.101:admin.conf .
```

- On the other nodes:
1. __Join cluster__:
```bash
kube join ...
```

- On your machine:
```bash
kubectl --kubeconfig ./admin.conf get nodes
```
