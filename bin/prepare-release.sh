#!/usr/bin/env bash

set -euo pipefail
DIR="$(dirname "$(realpath "$0")")"

sed -i .bak 's/\(:neo4j-cypher-dsl-version-latest:\) \(.*\)/\1 '"${1}"'/g' $DIR/../README.adoc

if test -n "${2-}"; then
  sed -i .bak 's/\(:neo4j-cypher-dsl-version:\) \(.*\)/\1 '"${2}"'/g' $DIR/../README.adoc
fi

rm $DIR/../README.adoc.bak

git add README.adoc
git commit -m "Prepare release."
