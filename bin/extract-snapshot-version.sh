#!/usr/bin/env bash

set -euo pipefail
DIR="$(dirname "$(realpath "$0")")"

sed -n 's/\(:neo4j-cypher-dsl-version:\) \(.*\)/\2/p' $DIR/../README.adoc
