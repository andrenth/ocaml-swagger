#!/usr/bin/bash -ue

set -x

DOCKER_VERSION=v1.41

tmp=$(mktemp -d)
curl -L "https://docs.docker.com/engine/api/$DOCKER_VERSION.yaml" -o "$tmp"/docker.yaml
docker run --pull always --rm -v "$tmp":/docs swaggerapi/swagger-codegen-cli generate -i /docs/docker.yaml -l swagger -o /docs
mv "$tmp"/swagger.json .
docker run --pull always --rm -v "$tmp":/docs --entrypoint /bin/rm alpine -rf -- '/docs/.swagger-codegen'
rm -rf -- "$tmp"
