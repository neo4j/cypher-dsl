#!/usr/bin/env bash
#
# Copyright (c) 2023-2026 "Neo4j,"
# Neo4j Sweden AB [https://neo4j.com]
#
# This file is part of Neo4j.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#


set -euo pipefail
DIR="$(dirname "$(realpath "$0")")"

NEW_OLD_VERSION=$(sed -n 's/project\.rel\.org\.neo4j\\:neo4j-cypher-dsl-parent=\(.*\)/\1/p' "$DIR"/../release.properties)
if [[ $NEW_OLD_VERSION != *-M* && $NEW_OLD_VERSION != *-RC* ]]
then
  "$DIR"/../mvnw versions:set-property -DgenerateBackupPoms=false -Dproperty=cypher-dsl.version.old -DnewVersion="$NEW_OLD_VERSION" -pl :neo4j-cypher-dsl-parent
fi
