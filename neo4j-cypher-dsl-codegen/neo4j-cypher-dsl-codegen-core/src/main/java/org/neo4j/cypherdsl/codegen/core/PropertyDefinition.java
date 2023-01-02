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
package org.neo4j.cypherdsl.codegen.core;

import static org.apiguardian.api.API.Status.EXPERIMENTAL;

import java.util.Objects;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * Represents a property name: With a name in the graph and an optional name in the domain model.
 *
 * @author Michael J. Simons
 * @since 2021.1.0
 */
@API(status = EXPERIMENTAL, since = "2021.1.0")
public final class PropertyDefinition {

	/**
	 * Creates a new property definition.
	 *
	 * @param nameInGraph          Required name in graph.
	 * @param optionalNameInDomain Optional in domain
	 * @return A working definition
	 */
	public static PropertyDefinition create(String nameInGraph, String optionalNameInDomain) {

		Assertions.hasText(nameInGraph, "The name of the property in the graph must not be null or empty.");
		return new PropertyDefinition(nameInGraph, optionalNameInDomain);
	}

	/**
	 * Name of the property inside the graph.
	 */
	private final String nameInGraph;

	/**
	 * Optional name of the property inside the domain.
	 */
	private final String nameInDomain;

	private PropertyDefinition(String nameInGraph, String nameInDomain) {
		this.nameInGraph = nameInGraph;
		this.nameInDomain = nameInDomain;
	}

	public String getNameInGraph() {
		return nameInGraph;
	}

	public String getNameInDomain() {
		return nameInDomain;
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) {
			return true;
		}
		if (o == null || getClass() != o.getClass()) {
			return false;
		}
		PropertyDefinition that = (PropertyDefinition) o;
		return nameInGraph.equals(that.nameInGraph) && Objects.equals(nameInDomain, that.nameInDomain);
	}

	@Override
	public int hashCode() {
		return Objects.hash(nameInGraph, nameInDomain);
	}
}
