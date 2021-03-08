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
import java.util.Optional;
import java.util.stream.Collectors;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.support.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * A property. A property might belong to a container such as a {@link Node} or {@link Relationship}, but it's not uncommon
 * to extract single properties from maps or from various datatypes such as a duration returned from stored procedures.
 * The container can be retrieved via {@link #getContainer()} in case the property belongs to a node or relationship.
 * <p>
 * A property has always a reference to the name of the object it was extracted from (see {@link #containerReference}.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = EXPERIMENTAL, since = "1.0")
public final class Property implements Expression {

	static Property create(Named parentContainer, String... names) {

		SymbolicName requiredSymbolicName = extractRequiredSymbolicName(parentContainer);
		return new Property(Optional.of(parentContainer), requiredSymbolicName, createListOfChainedNames(names), null);
	}

	static Property create(Expression containerReference, String... names) {

		Assertions.notNull(containerReference, "The property container is required.");
		return new Property(Optional.empty(), containerReference, createListOfChainedNames(names), null);
	}

	static Property create(Named parentContainer, Expression lookup) {

		SymbolicName requiredSymbolicName = extractRequiredSymbolicName(parentContainer);
		return new Property(Optional.of(parentContainer), requiredSymbolicName,
			Collections.singletonList(PropertyLookup.forExpression(lookup)), null);
	}

	static Property create(Expression containerReference, Expression lookup) {

		return new Property(Optional.empty(), containerReference,
			Collections.singletonList(PropertyLookup.forExpression(lookup)), null);
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

	Property(Optional<Named> container, Expression containerReference, List<PropertyLookup> names, String externalReference) {

		this.container = container.orElse(null);
		this.containerReference = containerReference;
		this.names = names;
		this.externalReference = externalReference;
	}

	/**
	 * @return The actual property being looked up. The order matters, so this will return a list, not a collection.
	 */
	@API(status = INTERNAL)
	List<PropertyLookup> getNames() {
		return names;
	}

	@API(status = INTERNAL)
	Named getContainer() {
		return container;
	}

	/**
	 * Returns the concatenated names of the property or the {@link #externalReference} (See {@link #referencedAs(String)}) if set.
	 *
	 * @return A name to reference the property under in an external application
	 */
	@API(status = EXPERIMENTAL, since = "2021.1.0")
	public String getName() {
		return externalReference != null ? this.externalReference : this.names.stream().map(PropertyLookup::getPropertyKeyName)
			.map(SymbolicName::getValue)
			.collect(Collectors.joining("."));
	}

	/**
	 * Creates a new property with an external reference.
	 * @param newReference An arbitrary, external reference
	 * @return A new property
	 */
	@API(status = EXPERIMENTAL, since = "2021.1.0")
	public Property referencedAs(String newReference) {

		return new Property(Optional.ofNullable(this.container), containerReference, this.names, newReference);
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
		this.containerReference.accept(visitor);
		this.names.forEach(name -> name.accept(visitor));
		visitor.leave(this);
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
