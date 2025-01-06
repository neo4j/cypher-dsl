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

import static org.apiguardian.api.API.Status.INTERNAL;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.internal.DefaultStatementContext;

/**
 * Context for while rendering a statement.
 *
 * @author Michael J. Simons
 * @soundtrack Various - Guardians Of The Galaxy: Awesome Mix Vol. 1
 * @since 2021.1.0
 */
@API(status = INTERNAL, since = "2021.1.0")
public sealed interface StatementContext permits DefaultStatementContext {

	/**
	 * Gets or creates the name of a parameter
	 *
	 * @param parameter The parameter whose name should be retrieved
	 * @return The name of the parameter or a generated name
	 */
	String getParameterName(Parameter<?> parameter);

	/**
	 * Resolves a {@link SymbolicName symbolic name} into a string: A symbolic name can be a placeholder without an actual
	 * value. In such cases a value is randomly generated and will stay constant for that name as long as the statement exists.
	 * In case the {@code symbolicName} has a constant value it will be returned,
	 *
	 * @param symbolicName the symbolic name to resolve
	 * @return a value for the given name
	 * @since 2023.0.3
	 */
	String resolve(SymbolicName symbolicName);

	/**
	 * Checks whether a given {@link SymbolicName symbolic name} has been resolved in this {@link StatementContext context}.
	 *
	 * @param symbolicName the symbolic name to check
	 * @return true if the given name has already been resolved in this context
	 * @since 2023.0.3
	 */
	boolean isResolved(SymbolicName symbolicName);
}
