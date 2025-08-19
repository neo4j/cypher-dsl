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

import java.util.List;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.DefaultStatementBuilder.OrderBuilder;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.internal.Distinct;
import org.neo4j.cypherdsl.core.utils.Assertions;

import static org.apiguardian.api.API.Status.INTERNAL;
import static org.apiguardian.api.API.Status.STABLE;

/**
 * See <a href=
 * "https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/Return.html">Return</a>.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public final class Return implements Clause {

	private final Distinct distinct;

	private final ReturnBody body;

	private final boolean raw;

	private Return(boolean raw, boolean distinct, ExpressionList returnItems, Order order, Skip skip, Limit limit) {
		this.distinct = (!raw && distinct) ? Distinct.INSTANCE : null;
		this.body = new ReturnBody(returnItems, order, skip, limit);
		this.raw = raw;
	}

	static Return create(boolean raw, boolean distinct, List<Expression> returnList, OrderBuilder orderBuilder) {

		if (returnList.isEmpty()) {
			return null;
		}

		if (raw) {
			String message = "A raw return must consist of exactly one raw expression.";
			Assertions.isTrue(returnList.size() == 1, message);
			Expression firstExpression = returnList.get(0);
			Assertions.isTrue(firstExpression instanceof RawLiteral
					|| firstExpression instanceof AliasedExpression ae && ae.getDelegate() instanceof RawLiteral,
					message);
		}

		ExpressionList returnItems = new ExpressionList(returnList);
		return new Return(raw, distinct, returnItems, orderBuilder.buildOrder().orElse(null), orderBuilder.getSkip(),
				orderBuilder.getLimit());
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		Visitable.visitIfNotNull(this.distinct, visitor);
		this.body.accept(visitor);
		visitor.leave(this);
	}

	/**
	 * {@return true if this clause has been created with proper, literal Cypher in place}
	 */
	@API(status = INTERNAL)
	public boolean isRaw() {
		return this.raw;
	}

	Distinct getDistinct() {
		return this.distinct;
	}

	ReturnBody getBody() {
		return this.body;
	}

	@Override
	public String toString() {
		return RendererBridge.render(this);
	}

}
