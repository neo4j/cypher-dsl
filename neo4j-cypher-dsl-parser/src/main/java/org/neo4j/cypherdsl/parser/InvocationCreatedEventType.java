/*
 * Copyright (c) "Neo4j"
 * Neo4j Sweden AB [http://neo4j.com]
 *
 * This file is part of Neo4j.
 *
 * Neo4j is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
