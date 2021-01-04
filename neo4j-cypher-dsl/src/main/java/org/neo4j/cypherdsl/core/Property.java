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

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

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

	static Property create(Named parentContainer, String... names) {

		SymbolicName requiredSymbolicName;
		try {
			requiredSymbolicName = parentContainer.getRequiredSymbolicName();
		} catch (IllegalStateException e) {
			throw new IllegalArgumentException(
				"A property derived from a node or a relationship needs a parent with a symbolic name.");
		}

		return new Property(requiredSymbolicName, createListOfChainedNames(names));
	}

	static Property create(Expression container, String... names) {

		Assertions.notNull(container, "The property container is required.");

		return new Property(container, createListOfChainedNames(names));
	}

	private static List<PropertyLookup> createListOfChainedNames(String... names) {

		Assertions.notEmpty(names, "The properties name is required.");

		if (names.length == 1) {
			return Collections.singletonList(new PropertyLookup(names[0]));
		} else {
			return Arrays.stream(names).map(PropertyLookup::new)
				.collect(Collectors.collectingAndThen(Collectors.toList(), Collections::unmodifiableList));
		}
	}

	/**
	 * The expression describing the container.
	 */
	private final Expression container;

	/**
	 * The name of this property.
	 */
	private final List<PropertyLookup> names;

	Property(Expression container, List<PropertyLookup> names) {

		this.container = container;
		this.names = names;
	}

	/**
	 * @return The actual property being looked up. The order matters, so this will return a list, not a collection.
	 */
	@API(status = INTERNAL)
	public List<PropertyLookup> getNames() {
		return names;
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
		this.names.forEach(name -> name.accept(visitor));
		visitor.leave(this);
	}
}
