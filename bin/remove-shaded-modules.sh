#!/usr/bin/env bash

set -euo pipefail
DIR="$(dirname "$(realpath "$0")")"

mkdir -p $DIR/../neo4j-cypher-dsl/target/generated-sources/misc
sed 's/requires org.neo4j.cypherdsl.support.schema_name;$//g'  $DIR/../neo4j-cypher-dsl/src/main/java/module-info.java >  $DIR/../neo4j-cypher-dsl/target/generated-sources/misc/module-info.java
