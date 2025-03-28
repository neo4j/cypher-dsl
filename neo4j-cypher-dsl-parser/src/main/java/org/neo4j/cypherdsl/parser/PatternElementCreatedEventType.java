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
package org.neo4j.cypherdsl.parser;

import static org.apiguardian.api.API.Status.STABLE;

import org.apiguardian.api.API;

/**
 * @author Michael J. Simons
 * @soundtrack Black Sabbath - The End: Live in Birmingham
 * @since 2022.1.1
 */
@API(status = STABLE, since = "2022.2.0")
public enum PatternElementCreatedEventType {

	/**
	 * A {@link org.neo4j.cypherdsl.core.PatternElement} is created during the creation of a {@code MATCH} clause.
	 */
	ON_MATCH,
	/**
	 * A {@link org.neo4j.cypherdsl.core.PatternElement} is created during the creation of a {@code CREATE} clause.
	 */
	ON_CREATE,
	/**
	 * A {@link org.neo4j.cypherdsl.core.PatternElement} is created during the creation of a {@code MERGE} clause.
	 */
	ON_MERGE,
}
