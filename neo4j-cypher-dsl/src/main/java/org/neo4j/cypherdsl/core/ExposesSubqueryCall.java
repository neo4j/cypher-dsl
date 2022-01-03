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
package org.neo4j.cypherdsl.core;

import static org.apiguardian.api.API.Status.STABLE;

import org.apiguardian.api.API;
import org.jetbrains.annotations.NotNull;
import org.neo4j.cypherdsl.core.utils.CheckReturnValue;

/**
 * This exposes a call method taking in a statement that represents a valid, correlated subquery.
 *
 * @author Michael J. Simons
 * @soundtrack Die Ärzte - Seitenhirsch
 * @neo4j.version 4.0.0
 * @since 2020.1.2
 */
@API(status = STABLE, since = "2020.1.2")
@Neo4jVersion(minimum = "4.0.0")
public interface ExposesSubqueryCall {

	/**
	 * The {@link Statement subquery} parameter must be a valid subquery.
	 * <ul>
	 * <li>must end with a RETURN clause</li>
	 * <li>cannot refer to variables from the enclosing query</li>
	 * <li>cannot return variables with the same names as variables in the enclosing query</li>
	 * <li>All variables that are returned from a subquery are afterwards available in the enclosing query</li>
	 * </ul>
	 *
	 * @param statement The statement representing the subquery.
	 * @return An ongoing reading
	 */
	@NotNull @CheckReturnValue
	default StatementBuilder.OngoingReadingWithoutWhere call(Statement statement) {
		return call(statement, new IdentifiableElement[0]);
	}

	/**
	 * The {@link Statement subquery} parameter must be a valid subquery.
	 * <ul>
	 * <li>must end with a RETURN clause</li>
	 * <li>cannot refer to variables from the enclosing query</li>
	 * <li>cannot return variables with the same names as variables in the enclosing query</li>
	 * <li>All variables that are returned from a subquery are afterwards available in the enclosing query</li>
	 * </ul>
	 *
	 * @param statement The statement representing the subquery.
	 * @param imports   Additional things that should be imported into the subquery.
	 * @return An ongoing reading
	 * @since 2021.3.0
	 */
	@NotNull @CheckReturnValue
	default StatementBuilder.OngoingReadingWithoutWhere call(Statement statement, String... imports) {
		return call(statement, (IdentifiableElement[]) Expressions.createSymbolicNames(imports));
	}

	/**
	 * The {@link Statement subquery} parameter must be a valid subquery.
	 * <ul>
	 * <li>must end with a RETURN clause</li>
	 * <li>cannot refer to variables from the enclosing query</li>
	 * <li>cannot return variables with the same names as variables in the enclosing query</li>
	 * <li>All variables that are returned from a subquery are afterwards available in the enclosing query</li>
	 * </ul>
	 *
	 * @param statement The statement representing the subquery.
	 * @param imports   Additional things that should be imported into the subquery. {@link AliasedExpression aliased expressions}
	 *                  will automatically imported twice (once as WITH a, then WITH a AS alias).
	 * @return An ongoing reading
	 * @since 2021.3.0
	 */
	@NotNull @CheckReturnValue
	StatementBuilder.OngoingReadingWithoutWhere call(Statement statement, IdentifiableElement... imports);
}
