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
 * Thrown when the parser detects a clause which is not yet supported.
 *
 * @author Michael J. Simons
 * @since 2021.3.0
 */
@API(status = STABLE, since = "2021.3.0")
public final class UnsupportedCypherException extends UnsupportedOperationException {

	@Serial
	private static final long serialVersionUID = 2871262762217922044L;

	/**
	 * Original input to the parser.
	 */
	private final String input;

	UnsupportedCypherException(String input, UnsupportedOperationException cause) {
		super(String.format("You used one Cypher construct not yet supported by the Cypher-DSL:%n%n\t%s%n%n"
				+ "Feel free to open an issue so that we might add support for it at https://github.com/neo4j-contrib/cypher-dsl/issues/new",
				input), cause);
		this.input = input;
	}

	/**
	 * {@return the original Cypher input handled to the parser}
	 */
	public String getInput() {
		return this.input;
	}

}
