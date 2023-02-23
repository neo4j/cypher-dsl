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
package org.neo4j.cypherdsl.core.fump;

import java.util.Collection;
import java.util.Set;
import java.util.stream.Collectors;

import org.neo4j.cypherdsl.core.fump.Token.Type;

/**
 * @author Michael J. Simons
 * @soundtrack Guns n' Roses - Use Your Illusion II
 * @since TBA
 */
public final class Things {

	private final Set<Token> tokens;

	private final Set<Property> properties;

	private final Set<PropertyCondition> propertyConditions;

	// TODO make private
	public Things(Set<Token> tokens, Set<Property> properties, Set<PropertyCondition> propertyConditions) {
		this.tokens = Set.copyOf(tokens);
		this.properties = Set.copyOf(properties);
		this.propertyConditions = Set.copyOf(propertyConditions);
	}

	/**
	 * Returns a collection of all tokens used in the analyzed statement.
	 *
	 * @return A collection of all tokens used
	 */
	public Collection<Token> getAllTokens() {
		return tokens;
	}

	private static boolean isNodeLabel(Token token) {
		return token.type() == Type.NODE_LABEL;
	}

	/**
	 * Returns a collection of all node labels used in the analyzed statement.
	 *
	 * @return A collection of all labels used
	 */
	public Collection<Token> getNodeLabels() {
		return tokens.stream().filter(Things::isNodeLabel).collect(Collectors.toSet());
	}

	private static boolean isRelationshipType(Token token) {
		return token.type() == Type.RELATIONSHIP_TYPE;
	}

	/**
	 * Returns a collection of all relationship types used in the analyzed statement.
	 *
	 * @return A collection of all types used
	 */
	public Collection<Token> getRelationshipTypes() {
		return tokens.stream().filter(Things::isRelationshipType).collect(Collectors.toSet());
	}

	/**
	 * Returns a collection of all properties resolved in the analyzed statement.
	 *
	 * @return A collection of all properties resolved
	 */
	public Collection<Property> getProperties() {
		return properties;
	}

	/**
	 * Returns a collection of all property based conditions in the statement involving the resolved properties.
	 *
	 * @return A collection of all conditions involving properties resolved in the statement
	 */
	public Collection<PropertyCondition> getPropertyConditions() {
		return propertyConditions;
	}
}
