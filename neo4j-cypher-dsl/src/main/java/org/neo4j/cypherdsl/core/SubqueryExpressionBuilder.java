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

import org.apiguardian.api.API;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * Something that can build counting sub-queries. Might be used in the future for
 * existential sub-queries, too.
 *
 * @author Michael J. Simons
 * @since 2023.9.0
 */
@API(status = STABLE, since = "2023.9.0")
public interface SubqueryExpressionBuilder {

	/**
	 * Creates a {@literal COUNT} sub-query expressions from at least one pattern.
	 * @param requiredPattern one pattern is required
	 * @param patternElement optional pattern
	 * @return the immutable {@link CountExpression}
	 * @since 2023.9.0
	 */
	CountExpression count(PatternElement requiredPattern, PatternElement... patternElement);

	/**
	 * Creates a {@literal COUNT} with an inner {@literal UNION} sub-query.
	 * @param union the union that will be the source of the {@literal COUNT} sub-query
	 * @return the immutable {@link CountExpression}
	 * @since 2023.9.0
	 */
	CountExpression count(Statement.UnionQuery union);

	/**
	 * Creates a {@literal COLLECT} subquery from a statement, including its filters and
	 * conditions. The statement must return exactly one column. It must however not
	 * contain any updates. While it would render syntactically correct Cypher, Neo4j does
	 * not support updates inside counting sub-queries.
	 * <p>
	 * To avoid confusion, shadowing of imported variables is not allowed. An outside
	 * scope variable is shadowed when a newly introduced variable within the inner scope
	 * is defined with the same variable.
	 * @param statement the statement to be passed to {@code COLLECT{}}
	 * @return a collecting sub-query.
	 * @since 2023.9.0
	 */
	CollectExpression collect(Statement statement);

}
