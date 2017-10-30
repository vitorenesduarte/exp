#!/usr/bin/env bash

POD_NAME=$(kubectl get pods |
           grep redis |
           awk '{print $1}')

PORT=6379
DIR=$(dirname "$0")
METRICS_DIR=${DIR}/../evaluation/metrics

kubectl port-forward "${POD_NAME}" ${PORT}:${PORT} & TUNNEL_PID=$!

echo "[$(date +%T)] Port forwarding starting..."
sleep 5

METRICS_DIR=${METRICS_DIR} ./"${DIR}"/redis-sync.erl

echo "[$(date +%T)] All files downloaded!"

kill ${TUNNEL_PID}

