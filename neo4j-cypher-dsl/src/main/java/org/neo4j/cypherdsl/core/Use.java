/*
 * Copyright (c) 2019-2022 "Neo4j,"
 * Neo4j Sweden AB [https://neo4j.com]
 *
 * This file is part of Neo4j.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.neo4j.cypherdsl.core;

import org.neo4j.cypherdsl.core.Clauses.DynamicUseClause;
import org.neo4j.cypherdsl.core.Clauses.StaticUseClause;

/**
 * The {@literal USE} clause can be prepended to statements or be used in a {@literal CALL} subquery. It is meant to
 * select composite databases or constituents thereof.
 *
 * @author Michael J. Simons
 * @since 2023.0.0
 */
public sealed interface Use extends Clause permits StaticUseClause, DynamicUseClause {
}
