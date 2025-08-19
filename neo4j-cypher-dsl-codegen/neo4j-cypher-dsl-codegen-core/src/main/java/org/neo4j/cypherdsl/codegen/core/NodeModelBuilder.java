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

import org.apiguardian.api.API;

import static org.apiguardian.api.API.Status.EXPERIMENTAL;

/**
 * Helper class to create static code describing labels and properties of a
 * {@code org.neo4j.cypherdsl.core.Node} object.
 *
 * @author Michael J. Simons
 * @since 2021.1.0
 */
@API(status = EXPERIMENTAL, since = "2021.1.0")
public interface NodeModelBuilder extends ModelBuilder<NodeModelBuilder> {

	/**
	 * Get a new instance of a node model builder. The instance can be modified as long as
	 * no {@code writeToXXX} method has been called.
	 * @param configuration the generators configuration
	 * @param packageName a package name, which can be null. In that case, the default
	 * package name from the configuration is taken
	 * @param suggestedTypeName a suggested type name
	 * @return a new instance without any labels or properties registered.
	 */
	static NodeModelBuilder create(Configuration configuration, String packageName, String suggestedTypeName) {

		return NodeImplBuilder.create(configuration, packageName, suggestedTypeName);
	}

	/**
	 * Adds a new label to this builder.
	 * @param newLabel the new label
	 * @return this builder
	 * @throws IllegalStateException when this builder has already been used to create
	 * Java class.
	 */
	NodeModelBuilder addLabel(String newLabel);

	/**
	 * Adds several new labels to this builder.
	 * @param newLabels the list of new labels
	 * @return this builder
	 * @throws IllegalStateException when this builder has already been used to create
	 * Java class.
	 */
	NodeModelBuilder addLabels(Collection<String> newLabels);

	/**
	 * Adds a relationship definition to this builder.
	 * @param definition the definition of a relationship
	 * @return this builder
	 * @throws IllegalStateException when this builder has already been used to create
	 * Java class.
	 */
	NodeModelBuilder addRelationshipDefinition(RelationshipPropertyDefinition definition);

	/**
	 * Adds a factory method for a relationship to this builder.
	 * @param definition the definition of the relationship
	 * @return this builder
	 * @throws IllegalStateException when this builder has already been used to create
	 * Java class.
	 * @since 2023.9.0
	 */
	NodeModelBuilder addRelationshipFactory(RelationshipFactoryDefinition definition);

	/**
	 * Adds a model this model should extend.
	 * @param baseModel the model to extend
	 * @return this builder
	 * @throws IllegalStateException when this builder has already been used to create
	 * Java class.
	 * @since 2023.9.2
	 */
	NodeModelBuilder setBaseNodeModel(NodeModelBuilder baseModel);

	/**
	 * Returns {@literal true} when this builder produces extensible modules.
	 * @return {@literal true} when this builder produces extensible modules
	 */
	boolean isExtensible();

	/**
	 * If true, this model will be extensible.
	 * @param extensible true if this model should be extensible
	 * @return this builder
	 * @throws IllegalStateException when this builder has already been used to create
	 * Java class.
	 * @since 2023.9.2
	 */
	NodeModelBuilder setExtensible(boolean extensible);

}
