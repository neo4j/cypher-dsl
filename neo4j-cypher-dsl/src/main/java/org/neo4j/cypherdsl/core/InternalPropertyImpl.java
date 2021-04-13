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

import static org.apiguardian.api.API.Status.INTERNAL;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * An internal implementation of a {@link Property}.
 *
 * @author Michael J. Simons
 * @since 2021.1.10
 */
@API(status = INTERNAL, since = "2021.1.0")
final class InternalPropertyImpl implements Property {

	static Property create(Named parentContainer, String... names) {

		SymbolicName requiredSymbolicName = extractRequiredSymbolicName(parentContainer);
		return new InternalPropertyImpl(Optional.of(parentContainer), requiredSymbolicName, createListOfChainedNames(names), null);
	}

	static Property create(Expression containerReference, String... names) {

		Assertions.notNull(containerReference, "The property container is required.");
		return new InternalPropertyImpl(Optional.empty(), containerReference, createListOfChainedNames(names), null);
	}

	static Property create(Named parentContainer, Expression lookup) {

		SymbolicName requiredSymbolicName = extractRequiredSymbolicName(parentContainer);
		return new InternalPropertyImpl(Optional.of(parentContainer), requiredSymbolicName,
			Collections.singletonList(PropertyLookup.forExpression(lookup)), null);
	}

	static Property create(Expression containerReference, Expression lookup) {

		return new InternalPropertyImpl(Optional.empty(), containerReference,
			Collections.singletonList(PropertyLookup.forExpression(lookup)), null);
	}

	/**
	 * The reference to the container itself is optional.
	 */
	private final Named container;

	/**
	 * The expression pointing to the {@link #container} above is not.
	 */
	private final Expression containerReference;

	/**
	 * The name of this property.
	 */
	private final List<PropertyLookup> names;

	/**
	 * An optional, external (as in external to the graph) reference.
	 */
	private final String externalReference;

	InternalPropertyImpl(Optional<Named> container, Expression containerReference, List<PropertyLookup> names,
		String externalReference) {

		this.container = container.orElse(null);
		this.containerReference = containerReference;
		this.names = names;
		this.externalReference = externalReference;
	}

	@Override
	public List<PropertyLookup> getNames() {
		return names;
	}

	@Override
	public Named getContainer() {
		return container;
	}

	@Override
	public String getName() {
		return externalReference != null ?
			this.externalReference :
			this.names.stream().map(PropertyLookup::getPropertyKeyName)
				.map(SymbolicName::getValue)
				.collect(Collectors.joining("."));
	}

	@Override
	public Property referencedAs(String newReference) {

		return new InternalPropertyImpl(Optional.ofNullable(this.container), containerReference, this.names,
			newReference);
	}

	@Override
	public Operation to(Expression expression) {
		return Operations.set(this, expression);
	}

	@Override
	public void accept(Visitor visitor) {
		visitor.enter(this);
		this.containerReference.accept(visitor);
		this.names.forEach(name -> name.accept(visitor));
		visitor.leave(this);
	}

	private static List<PropertyLookup> createListOfChainedNames(String... names) {

		Assertions.notEmpty(names, "The properties name is required.");

		if (names.length == 1) {
			return Collections.singletonList(PropertyLookup.forName(names[0]));
		} else {
			return Arrays.stream(names).map(PropertyLookup::forName)
				.collect(Collectors.collectingAndThen(Collectors.toList(), Collections::unmodifiableList));
		}
	}

	private static SymbolicName extractRequiredSymbolicName(Named parentContainer) {
		try {
			return parentContainer.getRequiredSymbolicName();
		} catch (IllegalStateException e) {
			throw new IllegalArgumentException(
				"A property derived from a node or a relationship needs a parent with a symbolic name.");
		}
	}
}
