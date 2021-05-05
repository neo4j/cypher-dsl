/*
 * Copyright (c) 2019-2021 "Neo4j,"
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

import static org.apiguardian.api.API.Status.EXPERIMENTAL;

import org.apiguardian.api.API;

/**
 * A statement that returns items from the graph. The shape of those items can be pretty much anything:
 * A list of records containing only properties, or nodes with properties mixed with relationships and
 * so on. The only guarantee given is that the query will return some data when executed.
 *
 * @author Michael J. Simons
 * @soundtrack Motörhead - Live At Brixton Academy
 * @since 2021.2.1
 */
@API(status = EXPERIMENTAL, since = "2021.2.1")
public interface ResultStatement extends Statement {
}
