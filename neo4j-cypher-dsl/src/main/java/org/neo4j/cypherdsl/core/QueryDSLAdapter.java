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
package org.neo4j.cypherdsl.core;

import static org.apiguardian.api.API.Status.EXPERIMENTAL;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.querydsl.CypherContext;
import org.neo4j.cypherdsl.core.querydsl.ToCypherFormatStringVisitor;

import com.querydsl.core.types.Path;
import com.querydsl.core.types.Predicate;

/**
 * This is a utility class to turn a several Query-DSL {@link com.querydsl.core.types.Expression expressions} into something
 * the Cypher-DSL can understand. It can only be used when `com.querydsl:querydsl-core` is on the classpath. While we
 * try our best to translate as many expression as possible into syntactically correct Cypher, we don't provide any guarantees
 * that the expressions and conditions generated are semantically correct in the context of the final query generated.
 *
 * @author Michael J. Simons
 * @soundtrack Paul Kalkbrenner - Berlin Calling
 * @since 2021.1.0
 */
@API(status = EXPERIMENTAL, since = "2021.1.0")
public final class QueryDSLAdapter implements ForeignAdapter<com.querydsl.core.types.Expression<?>> {

	private final ToCypherFormatStringVisitor toFormatStringVisitor;

	QueryDSLAdapter() {

		this.toFormatStringVisitor = new ToCypherFormatStringVisitor();
	}

	@Override
	public Condition asCondition(com.querydsl.core.types.Expression<?> expression) {

		if (!(expression instanceof Predicate)) {
			throw new IllegalArgumentException("Only Query-DSL predicates can be turned into Cypher-DSL's predicates.");
		}

		CypherContext context = new CypherContext();
		String formatString = expression.accept(toFormatStringVisitor, context);

		return new ExpressionCondition(Cypher.raw(formatString, context.getExpressions()));
	}

	@Override
	public Expression asExpression(com.querydsl.core.types.Expression<?> expression) {

		CypherContext context = new CypherContext();
		String formatString = expression.accept(toFormatStringVisitor, context);

		return Cypher.raw(formatString, context.getExpressions());
	}

	@Override
	public Node asNode(com.querydsl.core.types.Expression<?> expression) {

		if (!(expression instanceof Path<?>)) {
			throw new IllegalArgumentException("Only Query-DSL paths can be turned into nodes.");
		}

		Path<?> entityPath = (Path<?>) expression;
		return Cypher.node(entityPath.getRoot().getType().getSimpleName()).named(entityPath.getMetadata().getName());
	}

	@Override
	public Relationship asRelationship(com.querydsl.core.types.Expression<?> expression) {

		throw new UnsupportedOperationException("Not yet implemented.");
	}

	@Override
	public SymbolicName asName(com.querydsl.core.types.Expression<?> expression) {

		if (!(expression instanceof Path<?>)) {
			throw new IllegalArgumentException("Only Query-DSL paths can be turned into names.");
		}

		Path<?> entityPath = (Path<?>) expression;
		return Cypher.name(entityPath.getMetadata().getName());
	}
}
