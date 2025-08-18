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

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.Expression;
import org.neo4j.cypherdsl.core.Operation;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * The type of the event when a new expression is parsed and instantiated. Callbacks are
 * tied to this type.
 *
 * @author Michael J. Simons
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
	ON_DELETE_ITEM(Expression.class),
	/**
	 * Fired when new literals are created.
	 */
	ON_NEW_LITERAL(Expression.class);

	private final Class<? extends Expression> typeProduced;

	ExpressionCreatedEventType(Class<? extends Expression> typeProduced) {
		this.typeProduced = typeProduced;
	}

	/**
	 * {@return the actual type that will be produced, might be a specialization}
	 */
	Class<? extends Expression> getTypeProduced() {
		return this.typeProduced;
	}

}
