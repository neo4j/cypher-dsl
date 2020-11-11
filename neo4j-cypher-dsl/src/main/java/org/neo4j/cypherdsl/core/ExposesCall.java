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

import static org.apiguardian.api.API.Status.EXPERIMENTAL;

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
		 * @return An ongoing standalone call on which yielded arguments might be configured.
		 */
		T withArgs(Expression... arguments);
	}

	/**
	 * Interface to allow creating an expression instead of a statement from an ongoing definition. To make this
	 * generate valid Cypher the stored procedure in question must be a valid function.
	 *
	 * @since 2020.1.2
	 */
	interface AsFunction {

		/**
		 * @return A function invocation that can be used as an expression, for example as a property or inside a condition.
		 */
		Expression asFunction();
	}

	/**
	 * Used to yield procedure result fields. There are no checks involved whether the procedure being called
	 * actually returns items with the given names.
	 *
	 * @param <T> The type of the next step
	 */
	interface ExposesYield<T> {

		/**
		 * Adds the given items to the {@literal YIELD} clause of the generated call.
		 *
		 * @param yieldedItems The list of items to be yielded.
		 * @return The ongoing standalone call to be configured.
		 */
		default T yield(String... yieldedItems) {

			SymbolicName[] names = new SymbolicName[0];
			if (yieldedItems != null) {
				names = Arrays.stream(yieldedItems).map(SymbolicName::of)
					.toArray(SymbolicName[]::new);
			}
			return yield(names);
		}

		/**
	     * Adds the given items to the {@literal YIELD} clause of the generated call.
		 *
		 * @param resultFields The list of result fields to be returned.
		 * @return The ongoing standalone call to be configured.
		 */
		T yield(SymbolicName... resultFields);

		/**
		 * Adds the given items to the {@literal YIELD} clause of the generated call and uses new
		 * aliases in the generated call.
		 *
		 * @param aliasedResultFields The list of result fields to be returned with new aliases given.
		 * @return The ongoing standalone call to be configured.
		 */
		T yield(AliasedExpression... aliasedResultFields);
	}

}
