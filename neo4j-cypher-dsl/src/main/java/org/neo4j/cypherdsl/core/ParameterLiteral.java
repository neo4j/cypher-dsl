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
package org.neo4j.cypherdsl.core;

/**
 * Representation of a parameter literal. For internal use only.
 *
 * @author Michael J. Simons
 * @since 2025.2.0
 */
final class ParameterLiteral extends LiteralBase<Parameter<?>> {

	private ParameterLiteral(Parameter<?> content) {
		super(content);
	}

	static Literal<Parameter<?>> of(Parameter<?> content) {
		if (content.isAnon()) {
			throw new IllegalArgumentException("Anonymous parameters cannot be used as parameter literals");
		}
		return new ParameterLiteral(content);
	}

	@Override
	public String asString() {

		return "$" + content.getName();
	}

	@Override
	public Parameter<?> getContent() {
		return this.content;
	}

}
