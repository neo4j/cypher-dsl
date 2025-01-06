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
package org.neo4j.cypherdsl.codegen.core;

import static org.apiguardian.api.API.Status.INTERNAL;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.NodeBase;
import org.neo4j.cypherdsl.core.RelationshipBase;

/**
 * Strategy interface for generating class names for {@link NodeBase} or
 * {@link RelationshipBase}.
 *
 * @author Michael J. Simons
 * @soundtrack Daft Punk - TRON Legacy
 * @since 2021.1.0
 */
@API(status = INTERNAL, since = "2021.1.0")
@FunctionalInterface
public interface ClassNameGenerator {

	/**
	 * Generates a valid Java class based on a suggestion.  Most of the time the suggestion will either be a simple string,
	 * maybe an existing class name or the concatenated labels of a node as returned by {@code db.schema.nodeTypeProperties}.
	 * <p>
	 * Any implementation must remove invalid identifier parts.
	 *
	 * @param suggestedName Basically an arbitrary string suggesting a type name.
	 * @return A pair of names: A valid identifier for a type and matching field name.
	 */
	String generate(String suggestedName);
}
