/*
 * Copyright (c) 2019-2022 "Neo4j,"
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

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.RelationshipBase;

/**
 * A variant of a {@link ModelBuilder} responsible for creating classes extending from
 * {@link RelationshipBase} that will in the end represent a static model of relationships
 * and their properties.
 *
 * @author Michael J. Simons
 * @soundtrack Queen - Queen Forever
 * @since 2021.1.0
 */
@API(status = EXPERIMENTAL, since = "2021.1.0")
public interface RelationshipModelBuilder extends ModelBuilder<RelationshipModelBuilder> {

	/**
	 * Start building a new {@link RelationshipModelBuilder}.
	 *
	 * @param configuration    Configuration of the generator
	 * @param packageName      The package name into the model should be generated
	 * @param relationshipType The type of the relationship
	 * @return The new builder
	 */
	static RelationshipModelBuilder create(Configuration configuration, String packageName, String relationshipType) {

		return create(configuration, packageName, relationshipType, null);
	}

	/**
	 * Start building a new {@link RelationshipModelBuilder}, including alternate name suggestions.
	 *
	 * @param configuration                Configuration of the generator
	 * @param packageName                  The package name into the model should be generated
	 * @param relationshipType             The type of the relationship
	 * @param alternateClassNameSuggestion An alternative suggestion for the class name
	 * @return The new builder
	 */
	static RelationshipModelBuilder create(Configuration configuration, String packageName, String relationshipType, String alternateClassNameSuggestion) {

		return RelationshipImplBuilder.create(configuration, packageName, relationshipType, alternateClassNameSuggestion);
	}

	/**
	 * Registers the start node of the relationship with this builder. This is an optional operation and required for
	 * the builder to create valid classes.
	 * <p>
	 * A wild card will be emitted for the generated class instead of a concrete class so that this relationship model
	 * suites several different relationships with the same type.
	 *
	 * @param startNode The new start node, may be null
	 * @return This builder
	 * @throws IllegalStateException When this builder has already been used to create Java class.
	 */
	RelationshipModelBuilder setStartNode(NodeModelBuilder startNode);

	/**
	 * Registers the end node of the relationship with this builder. This is an optional operation and required for
	 * the builder to create valid classes.
	 * <p>
	 * A wild card will be emitted for the generated class instead of a concrete class so that this relationship model
	 * suites several different relationships with the same type.
	 *
	 * @param endNode The new start node, may be null
	 * @return This builder
	 * @throws IllegalStateException When this builder has already been used to create Java class.
	 */
	RelationshipModelBuilder setEndNode(NodeModelBuilder endNode);
}
