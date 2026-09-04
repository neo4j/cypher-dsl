#!/usr/bin/env bash

set -euo pipefail
DIR="$(dirname "$(realpath "$0")")"

sed -i.bak 's/\(:neo4j-cypher-dsl-version-latest:\) \(.*\)/\1 '"${1}"'/g' $DIR/../README.adoc
rm $DIR/../README.adoc.bak

if test -n "${2-}"; then
  DRYRUN=$2
else
  DRYRUN='false'
fi

if [ "$DRYRUN" != "true" ]; then
  git add $DIR/../README.adoc
  git commit -m "[maven-release-plugin] update README.adoc"
fi
