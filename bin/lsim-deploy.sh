#!/bin/bash

DCOS=$(dcos config show core.dcos_url)
TOKEN=$(dcos config show core.dcos_acs_token)

ENV_VARS=(
  LDB_MODE
  LDB_JOIN_DECOMPOSITIONS
  BRANCH
  OVERLAY
  SIMULATION
  NODE_NUMBER
  SIMULATION_IDENTIFIER
  SIMULATION_TIMESTAMP
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
    "LDB_MODE": "$LDB_MODE",
    "LDB_JOIN_DECOMPOSITIONS": "$LDB_JOIN_DECOMPOSITIONS",
    "BRANCH": "$BRANCH",
    "OVERLAY": "$OVERLAY",
    "SIMULATION": "$SIMULATION",
    "NODE_NUMBER": "$NODE_NUMBER",
    "SIMULATION_IDENTIFIER": "$SIMULATION_IDENTIFIER",
    "SIMULATION_TIMESTAMP": "$SIMULATION_TIMESTAMP"
  },
  "healthChecks": []
}
EOF

echo ">>> Adding lsims to Marathon"
curl -s -H "Authorization: token=$TOKEN" -H 'Content-type: application/json' -X POST -d @lsims.json "$DCOS/service/marathon/v2/apps" > /dev/null
