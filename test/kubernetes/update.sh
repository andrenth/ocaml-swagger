#!/usr/bin/bash -ue

K8S_VERSION=v1.25.2

curl -L "https://github.com/kubernetes/kubernetes/blob/$K8S_VERSION/api/openapi-spec/swagger.json?raw=true" -o swagger.json
