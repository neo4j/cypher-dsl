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
package org.neo4j.cypherdsl.core;

import static org.apiguardian.api.API.Status.INTERNAL;
import static org.apiguardian.api.API.Status.STABLE;

import java.util.List;

import org.apiguardian.api.API;

/**
 * A property. A property might belong to a container such as a {@link Node} or {@link Relationship}, but it's not uncommon
 * to extract single properties from maps or from various datatypes such as a duration returned from stored procedures.
 * The container can be retrieved via {@link #getContainer()} in case the property belongs to a node or relationship.
 * <p>
 * A property has always a reference to the name of the object it was extracted from.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public non-sealed interface Property extends Expression, IdentifiableElement {

	/**
	 * Returns the concatenated names of the property or the external reference (See {@link #referencedAs(String)}) if set.
	 *
	 * @return A name to reference the property under in an external application
	 */
	@API(status = STABLE, since = "2021.1.0")
	String getName();

	/**
	 * @return The actual property being looked up. The order matters, so this will return a list, not a collection.
	 */
	List<PropertyLookup> getNames();

	/**
	 * @return The container "owning" this property.
	 */
	Named getContainer();

	/**
	 * @return A reference to the container owning this property
	 */
	@API(status = INTERNAL, since = "2023.1.0")
	default Expression getContainerReference() {
		if (getContainer() == null) {
			throw new UnsupportedOperationException();
		}
		return getContainer().getRequiredSymbolicName();
	}

	/**
	 * Creates a new property with an external reference.
	 *
	 * @param newReference An arbitrary, external reference
	 * @return A new property
	 */
	@API(status = STABLE, since = "2021.1.0")
	Property referencedAs(String newReference);

	/**
	 * Creates an {@link Operation} setting this property to a new value. The property does not track the operations
	 * created with this method.
	 *
	 * @param expression expression describing the new value
	 * @return A new operation.
	 */
	Operation to(Expression expression);
}
