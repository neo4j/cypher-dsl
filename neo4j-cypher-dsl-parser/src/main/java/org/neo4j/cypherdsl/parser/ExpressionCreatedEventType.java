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

import static org.apiguardian.api.API.Status.STABLE;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.Expression;
import org.neo4j.cypherdsl.core.Operation;

/**
 * The type of the event when a new expression is parsed and instantiated. Callbacks are tied to this type.
 *
 * @author Michael J. Simons
 * @soundtrack Swiss + Die Andern - Linksradikaler Schlager
 * @since 2021.3.0
 */
@API(status = STABLE, since = "2021.3.0")
public enum ExpressionCreatedEventType {

	/**
	 * Fired for every new return item.
	 */
	ON_RETURN_ITEM(Expression.class),
	/**
	 * Fired when a property is set.
	 */
	ON_SET_PROPERTY(Operation.class),
	/**
	 * Fired when a label is defined.
	 */
	ON_SET_LABELS(Operation.class),
	/**
	 * Fired when a variable is defined.
	 */
	ON_SET_VARIABLE(Operation.class),
	/**
	 * Fired when a variable is added or redefined (set).
	 */
	ON_ADD_AND_SET_VARIABLE(Operation.class),
	/**
	 * Fired when parsing a property removal expression.
	 */
	ON_REMOVE_PROPERTY(Expression.class),
	/**
	 * Fired when parsing a label removing expression.
	 */
	ON_REMOVE_LABELS(Expression.class),
	/**
	 * Fired when parsing a new variable expression.
	 */
	ON_NEW_VARIABLE(Expression.class),
	/**
	 * Fired when parsing a new parameter expression.
	 */
	ON_NEW_PARAMETER(Expression.class),
	/**
	 * Fired when preparing a {@code DELETE x, y, z} clause.
	 */
	ON_DELETE_ITEM(Expression.class);

	private final Class<? extends Expression> typeProduced;

	ExpressionCreatedEventType(Class<? extends Expression> typeProduced) {
		this.typeProduced = typeProduced;
	}

	/**
	 * @return the actual type that will be produced, might be a specialization.
	 */
	Class<? extends Expression> getTypeProduced() {
		return typeProduced;
	}
}
