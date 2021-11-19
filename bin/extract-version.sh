#!/usr/bin/env bash

set -euo pipefail
DIR="$(dirname "$(realpath "$0")")"

if test -n "${1-}"; then
  sed -n 's/\(:neo4j-cypher-dsl-version-'"${1}"':\) \(.*\)/\2/p' $DIR/../README.adoc
else
  sed -n 's/\(:neo4j-cypher-dsl-version:\) \(.*\)/\2/p' $DIR/../README.adoc
fi
