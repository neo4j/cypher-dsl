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

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.apiguardian.api.API;

import static org.apiguardian.api.API.Status.EXPERIMENTAL;

/**
 * This represents a property in the static metamodel for a relationship. Such a property
 * can refer to several other {@link PropertyDefinition property definitions} on its own
 * when we detect properties stored on the relationship.
 *
 * @author Michael J. Simons
 * @since 2021.1.0
 */
@API(status = EXPERIMENTAL, since = "2021.1.0")
public final class RelationshipPropertyDefinition {

	/**
	 * A set of properties to generate.
	 */
	private final Set<PropertyDefinition> properties;

	/**
	 * Relationship type as stored in the database.
	 */
	private final String type;

	private final String optionalPropertyHolder;

	/**
	 * Optional name in the domain, for example a field name.
	 */
	private final String nameInDomain;

	/**
	 * Start of the relationship. Generated, static relationships are always left to right
	 * (start to end).
	 */
	private final NodeModelBuilder start;

	/**
	 * end of the relationship. Generated, static relationships are always left to right
	 * (start to end).
	 */
	private final NodeModelBuilder end;

	/**
	 * The actual builder that defines this relationship.
	 */
	private RelationshipModelBuilder relationshipBuilder;

	private RelationshipPropertyDefinition(String type, String optionalPropertyHolder, String nameInDomain,
			NodeModelBuilder start, RelationshipModelBuilder relationshipBuilder, NodeModelBuilder end,
			Set<PropertyDefinition> properties) {
		this.type = type;
		this.optionalPropertyHolder = optionalPropertyHolder;
		this.nameInDomain = nameInDomain;
		this.start = start;
		this.end = end;
		this.relationshipBuilder = relationshipBuilder;
		this.properties = properties;
	}

	/**
	 * Creates a new definition.
	 * @param type the type of the relationship as stored in the database
	 * @param optionalPropertyHolder the class name of the holder of the relationships
	 * properties
	 * @param nameInDomain the name of the relationship in the domain class
	 * @param start builder for the start node in the domain
	 * @param end builder for the end node in the domain
	 * @param optionalProperties a collection of properties, maybe null or empty
	 * @return a valid definition
	 */
	public static RelationshipPropertyDefinition create(String type, String optionalPropertyHolder, String nameInDomain,
			NodeModelBuilder start, NodeModelBuilder end, Collection<PropertyDefinition> optionalProperties) {

		return new RelationshipPropertyDefinition(type, optionalPropertyHolder, nameInDomain, start, null, end,
				(optionalProperties != null) ? new HashSet<>(optionalProperties) : Collections.emptySet());
	}

	/**
	 * Creates a new relationship definition with a new builder for it.
	 * @param newBuilder the new builder to use
	 * @return a new instance, {@literal this} won't change
	 */
	public RelationshipPropertyDefinition withBuilder(RelationshipModelBuilder newBuilder) {
		return new RelationshipPropertyDefinition(this.type, this.optionalPropertyHolder, this.nameInDomain, this.start,
				newBuilder, this.end, this.properties);
	}

	/**
	 * {@return the type of the relationship}
	 */
	public String getType() {
		return this.type;
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

	/**
	 * {@return a set of properties on this relationship}
	 */
	public Set<PropertyDefinition> getProperties() {
		return Collections.unmodifiableSet(this.properties);
	}

}
