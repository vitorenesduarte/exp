#!/usr/bin/env bash

RUNNING=$(kubectl get pods |
          grep lsim-dash |
          grep Running)

if [ ! -z "$RUNNING" ]; then
  echo "[$(date +%T)] lsim-dash already running. Exiting."
  exit
fi

# ENV SETUP:
# Kubernetes server and auth token
APISERVER=$(kubectl config view |
            grep "${CONTEXT}" -b1 |
            grep "server:" |
            grep -Eo "https://[0-9\.:]+")
TOKEN=$(kubectl describe secret |
        grep "token:" |
        awk '{print $2}')

# YAML file
FILE=/tmp/lsim-dash.yaml

cat <<EOF > ${FILE}
apiVersion: extensions/v1beta1
kind: Deployment
metadata:
  name: lsim-dash
spec:
  replicas: 1
  template:
    metadata:
      labels:
        tag: lsim-dash
    spec:
      containers:
      - name: lsim-dash
        image: vitorenesduarte/lsim-dash
        imagePullPolicy: Always
        env:
        - name: APISERVER
          value: "${APISERVER}"
        - name: TOKEN
          value: "${TOKEN}"
EOF

kubectl create -f ${FILE}
