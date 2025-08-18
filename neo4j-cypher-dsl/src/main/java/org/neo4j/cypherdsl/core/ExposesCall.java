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
package org.neo4j.cypherdsl.core;

import java.util.Arrays;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.annotations.CheckReturnValue;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * Entrypoint for building procedure calls.
 *
 * @param <T> the type of the returned builder
 * @author Michael J. Simons
 * @since 2020.0.1
 */
@API(status = STABLE, since = "2020.0.1")
public interface ExposesCall<T> {

	/**
	 * Starts defining a procedure call of the procedure with the given qualified name.
	 * @param namespaceAndProcedure the procedure name of the procedure to call.
	 * @return an ongoing definition of a call
	 */
	@CheckReturnValue
	T call(String... namespaceAndProcedure);

	/**
	 * Used to provide arguments to procedure calls.
	 *
	 * @param <T> the type of the next step
	 */
	interface ExposesWithArgs<T> {

		/**
		 * Adds the given arguments to the ongoing call and proceeds.
		 * @param arguments the list of new arguments, might be null or empty.
		 * @return an ongoing standalone call on which yielded arguments might be
		 * configured.
		 */
		@CheckReturnValue
		T withArgs(Expression... arguments);

	}

	/**
	 * Interface to allow creating an expression instead of a statement from an ongoing
	 * definition. To make this generate valid Cypher the stored procedure in question
	 * must be a valid function.
	 *
	 * @since 2020.1.2
	 */
	interface AsFunction {

		/**
		 * Returns a function invocation that can be used as an expression, for example as
		 * a property or inside a condition.
		 * @return a function invocation that can be used as an expression
		 */
		default Expression asFunction() {
			return asFunction(false);
		}

		/**
		 * Returns a function invocation that can be used as an expression, for example as
		 * a property or inside a condition.
		 * @param distinct set to true for adding the {@code DISTINCT} for any of the
		 * aggregating functions.
		 * @return a distinct function invocation that can be used as an expression, for
		 * example as a property or inside a condition.
		 * @since 2021.2.2
		 */
		Expression asFunction(boolean distinct);

	}

	/**
	 * Used to yield procedure result fields. There are no checks involved whether the
	 * procedure being called actually returns items with the given names.
	 *
	 * @param <T> the type of the next step
	 */
	interface ExposesYield<T> {

		/**
		 * Adds the given items to the {@literal YIELD} clause of the generated call.
		 * @param yieldedItems the list of items to be yielded.
		 * @return the ongoing standalone call to be configured.
		 */
		@CheckReturnValue
		default T yield(String... yieldedItems) {

			SymbolicName[] names = new SymbolicName[0];
			if (yieldedItems != null) {
				names = Arrays.stream(yieldedItems).map(SymbolicName::of).toArray(SymbolicName[]::new);
			}
			return this.yield(names);
		}

		/**
		 * Adds the given items to the {@literal YIELD} clause of the generated call.
		 * @param yieldedItems the list of named items to be yielded.
		 * @return the ongoing standalone call to be configured.
		 * @since 2020.1.4
		 */
		@CheckReturnValue
		default T yield(Named... yieldedItems) {

			SymbolicName[] names = new SymbolicName[0];
			if (yieldedItems != null) {
				names = Arrays.stream(yieldedItems).map(Named::getRequiredSymbolicName).toArray(SymbolicName[]::new);
			}
			return this.yield(names);
		}

		/**
		 * Adds the given items to the {@literal YIELD} clause of the generated call.
		 * @param resultFields the list of result fields to be returned.
		 * @return the ongoing standalone call to be configured.
		 */
		@CheckReturnValue
		T yield(SymbolicName... resultFields);

		/**
		 * Adds the given items to the {@literal YIELD} clause of the generated call and
		 * uses new aliases in the generated call.
		 * @param aliasedResultFields the list of result fields to be returned with new
		 * aliases given.
		 * @return the ongoing standalone call to be configured.
		 */
		@CheckReturnValue
		T yield(AliasedExpression... aliasedResultFields);

	}

}
