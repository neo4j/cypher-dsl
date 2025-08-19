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
package org.neo4j.cypherdsl.core.internal;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.Cypher;

import static org.apiguardian.api.API.Status.INTERNAL;

/**
 * An internal holder for a constant value that might be rendered as a parameter.
 *
 * @author Michael J. Simons
 * @since 2021.1.0
 */
@API(status = INTERNAL, since = "2021.1.0")
public final class ConstantParameterHolder {

	private final Object value;

	private final String literalValue;

	/**
	 * New instance for a reference to a constant parameter. An additional literal for the
	 * parameter will be generated.
	 * @param value the value of the constant parameter
	 */
	public ConstantParameterHolder(Object value) {
		this.value = value;
		this.literalValue = Cypher.literalOf(value).asString();
	}

	/**
	 * {@return the original value}
	 */
	public Object getValue() {
		return this.value;
	}

	/**
	 * {@return the value, but as a Cypher literal}
	 */
	public String asString() {
		return this.literalValue;
	}

}
