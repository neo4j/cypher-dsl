/*
 * Copyright (c) 2019-2023 "Neo4j,"
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

/**
 * A sub-query expression can either be an {@link ExistentialSubquery EXISTS-tential} or a {@link CountExpression COUNT}
 * expression as of Neo4j 5.
 *
 * @author Michael J. Simons
 * @since 2023.0.0
 * @soundtrack In Flames - The Jester Race
 */
public sealed interface SubqueryExpression extends Expression permits ExistentialSubquery, CountExpression, CollectExpression {
}
