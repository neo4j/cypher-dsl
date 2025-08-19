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

import org.apiguardian.api.API;

import static org.apiguardian.api.API.Status.EXPERIMENTAL;

/**
 * This represents a method in the static metamodel that creates a relationship.
 *
 * @author Andreas Berger
 * @since 2023.9.0
 */
@API(status = EXPERIMENTAL, since = "2023.9.0")
public final class RelationshipFactoryDefinition {

	/**
	 * Optional name in the domain, for example a field name.
	 */
	private final String nameInDomain;

	/**
	 * start of the relationship.
	 */
	private final NodeModelBuilder start;

	/**
	 * end of the relationship.
	 */
	private final NodeModelBuilder end;

	/**
	 * The actual builder that defines this relationship.
	 */
	private final RelationshipModelBuilder relationshipBuilder;

	private RelationshipFactoryDefinition(String nameInDomain, RelationshipModelBuilder relationshipBuilder,
			NodeModelBuilder start, NodeModelBuilder end) {
		this.nameInDomain = nameInDomain;
		this.start = start;
		this.end = end;
		this.relationshipBuilder = relationshipBuilder;
	}

	/**
	 * Creates a new definition.
	 * @param nameInDomain the name of the relationship in the domain class
	 * @param start builder for the start node in the domain
	 * @param end builder for the end node in the domain
	 * @return a valid definition
	 */
	public static RelationshipFactoryDefinition create(String nameInDomain, NodeModelBuilder start,
			NodeModelBuilder end) {
		return new RelationshipFactoryDefinition(nameInDomain, null, start, end);
	}

	/**
	 * Creates a new relationship definition with a new builder for it.
	 * @param newBuilder the new builder to use
	 * @return a new instance, {@literal this} won't change
	 */
	public RelationshipFactoryDefinition withBuilder(RelationshipModelBuilder newBuilder) {
		return new RelationshipFactoryDefinition(this.nameInDomain, newBuilder, this.start, this.end);
	}

	/**
	 * Returns the name in the domain model (most likely the field name).
	 * @return the name in the domain model
	 */
	public String getNameInDomain() {
		return this.nameInDomain;
	}

	/**
	 * {@return a builder for the start node}
	 */
	public NodeModelBuilder getStart() {
		return this.start;
	}

	/**
	 * {@return a builder for the end node}
	 */
	public NodeModelBuilder getEnd() {
		return this.end;
	}

	RelationshipModelBuilder getRelationshipBuilder() {
		return this.relationshipBuilder;
	}

}
