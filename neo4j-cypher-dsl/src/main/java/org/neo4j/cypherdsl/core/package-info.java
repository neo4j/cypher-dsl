/*
 * Copyright (c) 2019-2025 "Neo4j,"
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
/**
 * Contains an internal DSL for creating Cypher. This API is in an experimental status at
 * the moment and might change without further notice.
 * <p>
 * While we have extensive tests in place to check that queries SDN needs are correctly
 * generated, the AST itself is not validated for type errors. That is, one could misuse
 * the DSL here to create an AST that renders to a syntactically correct Cypher statement,
 * that will explode during runtime. For example using wrongly typed expression (an
 * expression referencing a list while a map is needed or something like that).
 * <p>
 * With this in mind, please use this DSL consciously if you find it useful. It won't go
 * away anytime soon, but might change in ways that break your code without further
 * notice.
 *
 * @author Michael J. Simons
 */
package org.neo4j.cypherdsl.core;
