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

import static org.apiguardian.api.API.Status.EXPERIMENTAL;

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
@API(status = EXPERIMENTAL, since = "2021.3.0")
public enum ExpressionCreatedEventType {

	ON_RETURN_ITEM(Expression.class),
	ON_SET_PROPERTY(Operation.class),
	ON_SET_LABELS(Operation.class),
	ON_SET_VARIABLE(Operation.class),
	ON_ADD_AND_SET_VARIABLE(Operation.class),
	ON_REMOVE_PROPERTY(Expression.class),
	ON_REMOVE_LABELS(Expression.class);

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
