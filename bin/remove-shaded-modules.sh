#!/usr/bin/env bash

set -euo pipefail
DIR="$(dirname "$(realpath "$0")")"

mkdir -p $DIR/../neo4j-cypher-dsl/target/modules
sed 's/requires org.neo4j.cypherdsl.support.schema_name;$//g'  $DIR/../neo4j-cypher-dsl/src/main/java/module-info.java >  $DIR/../neo4j-cypher-dsl/target/modules/module-info.java
