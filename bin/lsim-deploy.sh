#!/bin/bash

DCOS=$(dcos config show core.dcos_url)
TOKEN=$(dcos config show core.dcos_acs_token)

ENV_VARS=(
  BRANCH
  LDB_MODE
  LDB_JOIN_DECOMPOSITIONS
  LSIM_OVERLAY
  LSIM_SIMULATION
  LSIM_NODE_NUMBER
  LSIM_NODE_EVENT_NUMBER
  LSIM_SIMULATION_TS
)

for ENV_VAR in "${ENV_VARS[@]}"
do
  if [ -z "${!ENV_VAR}" ]; then
    echo ">>> ${ENV_VAR} is not configured. Please export it."
    exit 1
  fi
done

echo ">>> Configuring lsims"
cd /tmp

MEMORY=512.0
CPU=0.5

cat <<EOF > lsims.json
{
  "acceptedResourceRoles": [
    "slave_public"
  ],
  "id": "lsims",
  "dependencies": [],
  "cpus": $CPU,
  "mem": $MEMORY,
  "instances": $NODE_NUMBER,
  "container": {
    "type": "DOCKER",
    "docker": {
      "image": "vitorenesduarte/lsim",
      "network": "HOST",
      "forcePullImage": true,
      "parameters" : [
        { "key": "oom-kill-disable", "value": "true" }
      ]
    }
  },
  "ports": [0],
  "env" : {
    "DCOS": "$DCOS",
    "TOKEN": "$TOKEN",
  	"BRANCH": "$BRANCH",
  	"LDB_MODE": "$LDB_MODE",
  	"LDB_JOIN_DECOMPOSITIONS": "$LDB_JOIN_DECOMPOSITIONS",
  	"LSIM_OVERLAY": "$LSIM_OVERLAY",
  	"LSIM_SIMULATION": "$LSIM_SIMULATION",
  	"LSIM_NODE_NUMBER": "$LSIM_NODE_NUMBER",
  	"LSIM_NODE_EVENT_NUMBER": "$LSIM_NODE_EVENT_NUMBER",
  	"LSIM_SIMULATION_TS": "$LSIM_SIMULATION_TS"
  },
  "healthChecks": []
}
EOF

echo ">>> Adding lsims to Marathon"
curl -s -H "Authorization: token=$TOKEN" -H 'Content-type: application/json' -X POST -d @lsims.json "$DCOS/service/marathon/v2/apps" > /dev/null
