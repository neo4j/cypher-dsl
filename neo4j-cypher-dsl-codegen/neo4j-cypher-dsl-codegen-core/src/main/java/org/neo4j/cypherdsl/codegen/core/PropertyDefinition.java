/*
 * Copyright (c) 2019-2026 "Neo4j,"
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

import java.util.Objects;

import org.apiguardian.api.API;

import static org.apiguardian.api.API.Status.EXPERIMENTAL;

/**
 * Represents a property name: With a name in the graph and an optional name in the domain
 * model.
 *
 * @author Michael J. Simons
 * @since 2021.1.0
 */
@API(status = EXPERIMENTAL, since = "2021.1.0")
public final class PropertyDefinition {

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

	/**
	 * Creates a new property definition.
	 * @param nameInGraph required name in graph.
	 * @param optionalNameInDomain optional in domain
	 * @return a working definition
	 */
	public static PropertyDefinition create(String nameInGraph, String optionalNameInDomain) {

		if (nameInGraph == null || nameInGraph.isBlank()) {
			throw new IllegalArgumentException("The name of the property in the graph must not be null or empty.");
		}
		return new PropertyDefinition(nameInGraph, optionalNameInDomain);
	}

	/**
	 * {@return the name as defined in the future graph}
	 */
	public String getNameInGraph() {
		return this.nameInGraph;
	}

	/**
	 * {@return the name as defined in the domain object (most likely the field name)}
	 */
	public String getNameInDomain() {
		return this.nameInDomain;
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
		return this.nameInGraph.equals(that.nameInGraph) && Objects.equals(this.nameInDomain, that.nameInDomain);
	}

	@Override
	public int hashCode() {
		return Objects.hash(this.nameInGraph, this.nameInDomain);
	}

}
