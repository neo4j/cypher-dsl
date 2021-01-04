/*
 * Copyright (c) 2019-2021 "Neo4j,"
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

import static org.apiguardian.api.API.Status.EXPERIMENTAL;
import static org.apiguardian.api.API.Status.INTERNAL;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.support.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * A property that belongs to a property container (either Node or Relationship).
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = EXPERIMENTAL, since = "1.0")
public final class Property implements Expression {

	static Property create(Named parentContainer, String name) {

		SymbolicName requiredSymbolicName;
		try {
			requiredSymbolicName = parentContainer.getRequiredSymbolicName();
		} catch (IllegalStateException e) {
			throw new IllegalArgumentException(
				"A property derived from a node or a relationship needs a parent with a symbolic name.");
		}
		Assertions.hasText(name, "The properties name is required.");

		return new Property(requiredSymbolicName, new PropertyLookup((name)));
	}

	static Property create(Expression container, String name) {

		Assertions.notNull(container, "The property container is required.");
		Assertions.hasText(name, "The properties name is required.");

		return new Property(container, new PropertyLookup(name));

	}

	/**
	 * The expression describing the container.
	 */
	private final Expression container;

	/**
	 * The name of this property.
	 */
	private final PropertyLookup name;

	Property(Expression container, PropertyLookup name) {

		this.container = container;
		this.name = name;
	}

	/**
	 * @return The actual property being looked up.
	 */
	@API(status = INTERNAL)
	public PropertyLookup getName() {
		return name;
	}

	/**
	 * Creates an {@link Operation} setting this property to a new value. The property does not track the operations
	 * created with this method.
	 *
	 * @param expression expression describing the new value
	 * @return A new operation.
	 */
	public Operation to(Expression expression) {
		return Operations.set(this, expression);
	}

	@Override
	public void accept(Visitor visitor) {
		visitor.enter(this);
		this.container.accept(visitor);
		this.name.accept(visitor);
		visitor.leave(this);
	}
}
