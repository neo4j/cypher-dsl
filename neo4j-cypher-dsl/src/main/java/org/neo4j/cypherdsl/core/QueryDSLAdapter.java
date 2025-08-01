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
import org.neo4j.cypherdsl.build.annotations.RegisterForReflection;
import org.neo4j.cypherdsl.core.querydsl.CypherContext;
import org.neo4j.cypherdsl.core.querydsl.ToCypherFormatStringVisitor;

import com.querydsl.core.BooleanBuilder;
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
@API(status = INTERNAL, since = "2021.1.0")
@SuppressWarnings("unused")
@RegisterForReflection(allDeclaredConstructors = true)
final class QueryDSLAdapter implements ForeignAdapter<com.querydsl.core.types.Expression<?>> {

	private final com.querydsl.core.types.Expression<?> expression;

	QueryDSLAdapter(com.querydsl.core.types.Expression<?> expression) {
		this.expression = expression;
	}

	@Override
	public Condition asCondition() {

		if (!(expression instanceof Predicate)) {
			throw new IllegalArgumentException("Only Query-DSL predicates can be turned into Cypher-DSL's predicates.");
		}

		if (expression instanceof BooleanBuilder booleanBuilder && !booleanBuilder.hasValue()) {
			return Conditions.noCondition();
		}

		CypherContext context = new CypherContext();
		String formatString = expression.accept(ToCypherFormatStringVisitor.INSTANCE, context);

		return new ExpressionCondition(Cypher.raw(formatString, (Object[]) context.getExpressions()));
	}

	@Override
	public Expression asExpression() {

		CypherContext context = new CypherContext();
		String formatString = expression.accept(ToCypherFormatStringVisitor.INSTANCE, context);

		return Cypher.raw(formatString, (Object[]) context.getExpressions());
	}

	@Override
	public Node asNode() {

		if (!(expression instanceof Path<?> entityPath)) {
			throw new IllegalArgumentException("Only Query-DSL paths can be turned into nodes.");
		}

		return Cypher.node(entityPath.getRoot().getType().getSimpleName()).named(entityPath.getMetadata().getName());
	}

	@Override
	public Relationship asRelationship() {

		throw new UnsupportedOperationException("Not yet implemented.");
	}

	@Override
	public SymbolicName asName() {

		if (!(expression instanceof Path<?> entityPath)) {
			throw new IllegalArgumentException("Only Query-DSL paths can be turned into names.");
		}

		return Cypher.name(entityPath.getMetadata().getName());
	}
}
