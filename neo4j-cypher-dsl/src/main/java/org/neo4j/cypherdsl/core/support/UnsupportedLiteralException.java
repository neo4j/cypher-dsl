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
package org.neo4j.cypherdsl.core.support;

import static org.apiguardian.api.API.Status.EXPERIMENTAL;

import org.apiguardian.api.API;

/**
 * Thrown when a given object cannot be used as a Cypher-DSL-Literal.
 *
 * @author Michael J. Simons
 * @soundtrack Helge Schneider - Heart Attack No. 1
 * @since 2021.1.0
 */
@API(status = EXPERIMENTAL, since = "2021.1.0")
public final class UnsupportedLiteralException extends IllegalArgumentException {

	private final Class<?> unsupportedType;

	public UnsupportedLiteralException(String message, Object unsupportedObject) {
		super(message);
		this.unsupportedType = unsupportedObject.getClass();
	}

	public UnsupportedLiteralException(Object unsupportedObject) {
		super("Unsupported literal type: " + unsupportedObject.getClass());
		this.unsupportedType = unsupportedObject.getClass();
	}

	public Class<?> getUnsupportedType() {
		return unsupportedType;
	}
}
