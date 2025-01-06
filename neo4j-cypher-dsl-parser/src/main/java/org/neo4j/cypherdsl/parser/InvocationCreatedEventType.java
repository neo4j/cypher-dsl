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
package org.neo4j.cypherdsl.parser;

import org.neo4j.cypherdsl.core.Clause;
import org.neo4j.cypherdsl.core.Expression;

/**
 * The type of event emitted when creating a procedure call or function invocation.
 *
 * @author Michael J. Simons
 * @since 2022.8.6
 */
public enum InvocationCreatedEventType {

	/**
	 * When parsing a {@code CALL x.y(z)} like statement.
	 */
	ON_CALL(Clause.class),

	/**
	 * When parsing a {@code sin(x)} like statement.
	 */
	ON_INVOCATION(Expression.class);

	private final Class<?> typeProduced;

	InvocationCreatedEventType(Class<?> typeProduced) {
		this.typeProduced = typeProduced;
	}

	Class<?> getTypeProduced() {
		return typeProduced;
	}
}
