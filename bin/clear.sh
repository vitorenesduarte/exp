#!/usr/bin/env bash

while [ "${pods}" != "0" ]; do
  kubectl delete deployment --all

  sleep 2
  pods=$(kubectl get pods --show-all --no-headers 2>&1 | grep -v "No resources found." | wc -l | xargs echo)
done
