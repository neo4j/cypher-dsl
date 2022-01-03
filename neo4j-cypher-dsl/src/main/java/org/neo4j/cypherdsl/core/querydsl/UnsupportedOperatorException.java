/*
 * Copyright (c) 2019-2022 "Neo4j,"
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
package org.neo4j.cypherdsl.core.querydsl;

import static org.apiguardian.api.API.Status.STABLE;

import org.apiguardian.api.API;

import com.querydsl.core.types.Operator;

/**
 * Thrown when a QueryDSL operator cannot be used with the Cypher-DSL predicate converter.
 *
 * @author Michael J. Simons
 * @soundtrack Fritz Kalkbrenner - Drown
 * @since 2021.1.0
 */
@API(status = STABLE, since = "2021.1.0")
public final class UnsupportedOperatorException extends IllegalArgumentException {

	private static final long serialVersionUID = 2025849674095086421L;
	private final Operator unsupportedOperator;

	public UnsupportedOperatorException(Operator unsupportedOperator) {
		super("The Cypher-DSL cannot use the Query-DSL operator " + unsupportedOperator);
		this.unsupportedOperator = unsupportedOperator;
	}

	public Operator getUnsupportedOperator() {
		return unsupportedOperator;
	}
}
