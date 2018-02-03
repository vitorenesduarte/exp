#!/usr/bin/env bash

EXCLUDE="(redis|lsim-dash)"

while [ "${pods}" != "0" ]; do
  kubectl get deployment  --no-headers |
  grep -vE ${EXCLUDE} |
  awk '{ print $1 }' |
  xargs kubectl delete deployment

  sleep 2
  pods=$(kubectl get pods --show-all --no-headers 2>&1 |
         grep -vE ${EXCLUDE} |
         grep -v "No resources found." |
         wc -l | xargs echo)
done
