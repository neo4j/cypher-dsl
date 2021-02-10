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

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.utils.Assertions;

import static org.apiguardian.api.API.Status.EXPERIMENTAL;
import static org.apiguardian.api.API.Status.INTERNAL;

/**
 * Represents a named parameter inside a Cypher statement.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = EXPERIMENTAL, since = "1.0")
public final class Parameter implements Expression {

	private static final Object NO_VALUE = new Object();

	private final String name;
	private final Object boundValue;

	static Parameter create(String name) {
		return create(name, NO_VALUE);
	}

	static Parameter create(String name, Object value) {

		Assertions.hasText(name, "The name of the parameter is required!");

		if (name.startsWith("$")) {
			return create(name.substring(1));
		}

		return new Parameter(name, value);
	}

	private Parameter(String name, Object value) {
		this.name = name;
		this.boundValue = value;
	}

	/**
	 * @return The name of this parameter.
	 */
	@API(status = INTERNAL)
	public String getName() {
		return name;
	}

	/**
	 * @return A new Parameter with a bound value
	 */
	@API(status = INTERNAL)
	public Parameter withValue(Object value) {
		return create(name, value);
	}

	/**
	 * @return the value bound to this parameter
	 */
	@API(status = INTERNAL)
	public Object getBoundValue() {
		return boundValue;
	}

	/**
	 * @return true if the Parameter has a bound value
	 */
	@API(status = INTERNAL)
	public boolean hasBoundValue() {
		return boundValue != NO_VALUE;
	}
}
