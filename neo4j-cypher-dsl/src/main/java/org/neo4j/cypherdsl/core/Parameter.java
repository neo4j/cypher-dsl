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

import static org.apiguardian.api.API.Status.STABLE;
import static org.apiguardian.api.API.Status.INTERNAL;

import java.util.Objects;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * Represents a named parameter inside a Cypher statement.
 *
 * @author Michael J. Simons
 * @author Andreas Berger
 * @param <T> The type of the parameter. Defaults to {@link Object} for a parameter without a value from which to derive
 *            the actual type.
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public final class Parameter<T> implements Expression {

	static final Object NO_VALUE = new Object();

	private final String name;
	private final T value;

	static Parameter<Object> create(String name) {
		return create(name, NO_VALUE);
	}

	static <T> Parameter<T> create(String name, T value) {

		Assertions.hasText(name, "The name of the parameter is required!");

		if (name.startsWith("$")) {
			return create(name.substring(1), value);
		}

		return new Parameter<>(name, value);
	}

	static <T> Parameter<T> anon(T value) {

		return new Parameter<>(null, value);
	}

	private Parameter(String name, T value) {

		this.name = name;
		this.value = value;
	}

	/**
	 * Query method to check if this is an anonymous parameter.
	 *
	 * @return True if this is an anonymous parameter
	 * @since 2021.1.0
	 */
	@API(status = STABLE, since = "2021.0.0")
	public boolean isAnon() {
		return this.name == null || this.name.trim().isEmpty();
	}

	/**
	 * @return The name of this parameter.
	 * @since 2023.1.0
	 */
	@API(status = STABLE, since = "2023.1.0")
	public String getName() {
		return name;
	}

	/**
	 * @param newValue The new value that should be bound by this parameter
	 * @return A new parameter with a bound value
	 * @since 2021.0.0
	 */
	@API(status = STABLE, since = "2021.0.0")
	public Parameter<?> withValue(Object newValue) {
		return create(name, newValue);
	}

	/**
	 * @return the value bound to this parameter
	 */
	@API(status = INTERNAL, since = "2021.1.0")
	public T getValue() {
		return value;
	}

	/**
	 * @return true if the Parameter has a bound value
	 */
	boolean hasValue() {
		return !Objects.equals(value, NO_VALUE);
	}

	@Override
	public String toString() {
		return RendererBridge.render(this);
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) {
			return true;
		}
		if (o == null || getClass() != o.getClass()) {
			return false;
		}
		if (this.isAnon()) {
			return super.equals(o);
		}
		Parameter<?> parameter = (Parameter<?>) o;
		return Objects.equals(name, parameter.name);
	}

	@Override
	public int hashCode() {
		return this.isAnon() ? super.hashCode() : Objects.hash(name);
	}
}
