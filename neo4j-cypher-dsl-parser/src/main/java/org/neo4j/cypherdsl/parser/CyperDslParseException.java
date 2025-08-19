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
package org.neo4j.cypherdsl.parser;

import java.io.Serial;

import org.apiguardian.api.API;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * A runtime exception wrapping checked parsing exception into a sensible exception
 * hierarchy.
 *
 * @author Michael J. Simons
 * @since 2021.3.0
 */
@API(status = STABLE, since = "2021.3.0")
public final class CyperDslParseException extends RuntimeException {

	@Serial
	private static final long serialVersionUID = -3188559145717360828L;

	/**
	 * Creates a new Cypher-DSL specific exception with the given cause.
	 * @param cause original cause, being wrapped in a {@link RuntimeException}.
	 */
	public CyperDslParseException(Throwable cause) {
		super(cause);
	}

}
