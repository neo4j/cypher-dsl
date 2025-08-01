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

import static org.apiguardian.api.API.Status.STABLE;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * An aliased expression, that deals with named expressions when accepting visitors.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public final class AliasedExpression implements Aliased, Expression, IdentifiableElement {

	private final Expression delegate;

	private final String alias;

	AliasedExpression(Expression delegate, String alias) {

		this.delegate = delegate;
		this.alias = alias;
	}

	@Override
	public String getAlias() {
		return alias;
	}

	/**
	 * This takes the originally aliased expression and re-aliases it. Aliases are not nested.
	 *
	 * @param newAlias The new alias to use
	 * @return A new aliased, expression.
	 */
	@Override
	public AliasedExpression as(String newAlias) {

		Assertions.hasText(newAlias, "The alias may not be null or empty.");
		return new AliasedExpression(this.delegate, newAlias);
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		Expressions.nameOrExpression(this.delegate).accept(visitor);
		visitor.leave(this);
	}

	Expression getDelegate() {
		return delegate;
	}

	@Override
	public Expression asExpression() {
		return this;
	}

	@Override
	public String toString() {
		return RendererBridge.render(this);
	}
}
