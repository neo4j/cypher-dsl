/*
 * Copyright (c) 2019-2023 "Neo4j,"
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

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.apiguardian.api.API;
import org.apiguardian.api.API.Status;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.neo4j.cypherdsl.core.utils.Assertions;
import org.neo4j.cypherdsl.core.utils.LRUCache;

/**
 * A symbolic name to identify nodes, relationships and aliased items.
 * <p>
 * See <a href="https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/SchemaName.html">SchemaName</a>
 * <a href="https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/SymbolicName.html">SymbolicName</a>
 * <p>
 * While OpenCypher extends the <a href="https://unicode.org/reports/tr31/">UNICODE IDENTIFIER AND PATTERN SYNTAX</a>
 * with some characters, this DSL uses the same identifier Java itself uses for simplicity and until otherwise needed.
 *
 * @author Michael J. Simons
 * @author Andreas Berger
 * @since 1.0
 */
@API(status = Status.EXPERIMENTAL, since = "1.0")
public final class SymbolicName implements Expression, IdentifiableElement {

	private static final Map<String, SymbolicName> CACHE = Collections.synchronizedMap(new LRUCache<>(32));

	static SymbolicName of(String name) {

		Assertions.hasText(name, "Name must not be empty.");
		return CACHE.computeIfAbsent(name, SymbolicName::new);
	}

	static SymbolicName unsafe(String name) {

		Assertions.hasText(name, "Name must not be empty.");
		return new SymbolicName(name);
	}

	static SymbolicName unresolved() {

		return new SymbolicName(null);
	}

	private final String value;

	private SymbolicName(String value) {
		this.value = value;
	}

	/**
	 * @return The value of this symbolic name.
	 */
	@API(status = INTERNAL)
	public String getValue() {
		return value;
	}

	/**
	 * Creates a new symbolic name by concatenating {@code otherValue} to this names value.
	 * Returns {@literal this} if {@code otherValue} is empty.
	 *
	 * @param otherValue The value to concat.
	 * @return A new symbolic name
	 */
	@NotNull @Contract(pure = true)
	public SymbolicName concat(String otherValue) {

		Assertions.notNull(otherValue, "Value to concat must not be null.");
		if (otherValue.isEmpty()) {
			return this;
		}
		return SymbolicName.of(this.value + otherValue);
	}

	/**
	 * A list will never be a valid entry for a map projection, so this convenient method prevents trying to create one
	 * from a list of objects. It will delegate to {@link #project(Object...)} with the content of the list.
	 *
	 * @param entries A list of entries for the projection
	 * @return A map projection.
	 * @since 2021.0.0
	 */
	@NotNull @Contract(pure = true)
	public MapProjection project(List<Object> entries) {
		return project(entries.toArray());
	}

	/**
	 * Creates a map projection based on this node. The node needs a symbolic name for this to work.
	 * <p>
	 * Entries of type {@code String} in {@code entries} followed by an {@link Expression} will be treated as map keys
	 * pointing to the expression in the projection, {@code String} entries alone will be treated as property lookups on the node.
	 *
	 * @param entries A list of entries for the projection
	 * @return A map projection.
	 * @since 2021.0.0
	 */
	@NotNull @Contract(pure = true)
	public MapProjection project(Object... entries) {
		return MapProjection.create(this, entries);
	}

	@Override
	public String toString() {
		return value != null ? "SymbolicName{" +
			"name='" + value + '\'' +
			'}' : "Unresolved SymbolicName";
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) {
			return true;
		}
		if (o == null || getClass() != o.getClass()) {
			return false;
		}
		// Unresolved values are only equal to themselves
		if (value == null) {
			return false;
		}
		SymbolicName that = (SymbolicName) o;
		return value.equals(that.value);
	}

	@Override
	public int hashCode() {
		return value == null ? super.hashCode() : Objects.hash(value);
	}

	@NotNull
	@Override
	public Expression asExpression() {
		return this;
	}
}
