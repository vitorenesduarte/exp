#!/usr/bin/env bash

POD_NAME=$(kubectl describe pod redis |
           grep -E "^Name:" |
           awk '{print $2}')

PORT=6379
DIR=$(dirname $0)
METRICS_DIR=${DIR}/../priv/evaluation/metrics

kubectl port-forward ${POD_NAME} ${PORT}:${PORT} & TUNNEL_PID=$!

echo "[$(date +%T)] Port forwarding starting..."
sleep 3

METRICS_DIR=${METRICS_DIR} ./$DIR/redis-sync.erl

echo "[$(date +%T)] All files downloaded!"

kill ${TUNNEL_PID}

