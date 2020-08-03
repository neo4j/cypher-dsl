/*
 * Copyright (c) 2019-2020 "Neo4j,"
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

import static org.apiguardian.api.API.Status.*;

import java.util.Arrays;

import org.apiguardian.api.API;

/**
 * Entrypoint for building procedure calls.
 *
 * @param <T> The type of the returned builder
 * @author Michael J. Simons
 * @soundtrack Erik Cohen - Nostalgie für die Zukunft
 * @since 2020.0.1
 */
@API(status = EXPERIMENTAL, since = "2020.0.1")
public interface ExposesCall<T> {

	/**
	 * Starts defining a procedure call of the procedure with the given qualified name.
	 *
	 * @param namespaceAndProcedure The procedure name of the procedure to call.
	 * @return An ongoing definition of a call
	 */
	T call(String... namespaceAndProcedure);

	/**
	 * Used to provide arguments to procedure calls.
	 *
	 * @param <T> The type of the next step
	 */
	interface ExposesWithArgs<T> {

		/**
		 * Adds the given arguments to the ongoing call and procedes.
		 *
		 * @param arguments The list of new arguments, might be null or empty.
		 * @return An oingoing standalone call on which yielded arguments might be configured.
		 */
		T withArgs(Expression... arguments);
	}

	/**
	 * Used to yield procedure result fields.
	 *
	 * @param <T> The type of the next step
	 */
	interface ExposesYield<T> {

		default T yield(String... yieldedItems) {

			SymbolicName[] names = new SymbolicName[0];
			if (yieldedItems != null) {
				names = Arrays.stream(yieldedItems).map(SymbolicName::create)
					.toArray(SymbolicName[]::new);
			}
			return yield(names);
		}

		T yield(SymbolicName... resultFields);

		T yield(AliasedExpression... aliasedResultFields);
	}

}
