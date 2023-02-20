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

import java.util.Objects;

import org.neo4j.cypherdsl.core.NodeLabel;

/**
 * @author Michael J. Simons
 * @param type  The type of this token
 * @param value The concrete value
 * @soundtrack Avenger - Prayers Of Steel
 * @since TBA
 */
public record Token(Type type, String value) {

	/**
	 * Turns a specific {@link NodeLabel label} into a more abstract token.
	 *
	 * @param label A label, must not be {@literal null}.
	 * @return A token
	 */
	public static Token label(NodeLabel label) {
		return new Token(Type.NODE_LABEL, Objects.requireNonNull(label, "Label must not be null.").getValue());
	}

	/**
	 * Turns a specific node label into a more abstract token.
	 *
	 * @param label A label, must not be {@literal null}.
	 * @return A token
	 */
	public static Token label(String label) {
		return new Token(Type.NODE_LABEL, Objects.requireNonNull(label, "Label must not be null."));
	}

	/**
	 * Turns a specific relationship type into a more abstract token
	 *
	 * @param type A string representing a type, must not be {@literal null}.
	 * @return A token
	 */
	public static Token type(String type) {
		return new Token(Type.RELATIONSHIP_TYPE, Objects.requireNonNull(type, "Type must not be null."));
	}

	/**
	 * The specific token type.
	 */
	enum Type {
		/**
		 * Represents a node label.
		 */
		NODE_LABEL,
		/**
		 * Represents a relationship type.
		 */
		RELATIONSHIP_TYPE
	}
}
