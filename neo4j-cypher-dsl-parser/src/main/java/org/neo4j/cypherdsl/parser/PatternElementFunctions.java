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

import org.neo4j.cypherdsl.core.FunctionInvocation;

/**
 * A list of additional pattern element functions, to avoid opening up or extending the Cypher-DSLs build in API
 * @author Michael J. Simons
 * @soundtrack Chingon - Mexican Spaghetti Western
 */
enum PatternElementFunctions implements FunctionInvocation.FunctionDefinition {

	SHORTEST_PATH("shortestPath"),
	ALL_SHORTEST_PATHS("allShortestPaths");

	private final String implementationName;

	PatternElementFunctions(String implementationName) {
		this.implementationName = implementationName;
	}

	@Override
	public String getImplementationName() {
		return implementationName;
	}
}
