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
package org.neo4j.cypherdsl.core;

import org.apiguardian.api.API;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * Represents a literal with an optional content.
 *
 * @param <T> type of content
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public interface Literal<T> extends Expression {

	/**
	 * The string representation should be designed in such a way the a renderer can use
	 * it correctly in the given context of the literal, i.e. a literal containing a
	 * string should quote that string and escape all reserved characters.
	 * @return a string representation to be used literally in a cypher statement.
	 */
	String asString();

	/**
	 * Retrieves the actual content of this literal, might not be supported by all
	 * literals.
	 * @return the actual content of this literal
	 * @since 2023.4.0
	 */
	default T getContent() {
		throw new UnsupportedOperationException("Retrieving content not supported");
	}

	/**
	 * Thrown when a given object cannot be used as a Cypher-DSL-Literal.
	 *
	 * @since 2021.1.0
	 */
	@API(status = STABLE, since = "2021.1.0")
	final class UnsupportedLiteralException extends IllegalArgumentException {

		private static final long serialVersionUID = 864563506445498829L;

		/**
		 * Value holding the unsupported type.
		 */
		private final Class<?> unsupportedType;

		UnsupportedLiteralException(String message, Object unsupportedObject) {
			super(message);
			this.unsupportedType = unsupportedObject.getClass();
		}

		UnsupportedLiteralException(Object unsupportedObject) {
			super("Unsupported literal type: " + unsupportedObject.getClass());
			this.unsupportedType = unsupportedObject.getClass();
		}

		/**
		 * {@return the type that wasn't supported as literal}
		 */
		public Class<?> getUnsupportedType() {
			return this.unsupportedType;
		}

	}

}
