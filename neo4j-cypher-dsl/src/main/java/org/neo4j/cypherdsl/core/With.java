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
import org.neo4j.cypherdsl.core.ast.EnterResult;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.internal.Distinct;

import static org.apiguardian.api.API.Status.INTERNAL;
import static org.apiguardian.api.API.Status.STABLE;

/**
 * See <a href=
 * "https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/With.html">With</a>.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public final class With implements Visitable, Clause {

	private final Distinct distinct;

	private final ReturnBody body;

	private final Where where;

	With(Return returnClause, Where where) {
		this.distinct = returnClause.getDistinct();
		this.body = returnClause.getBody();
		this.where = where;
	}

	With(boolean distinct, ExpressionList returnItems, Order order, Skip skip, Limit limit, Where where) {
		this.distinct = distinct ? Distinct.INSTANCE : null;
		this.body = new ReturnBody(returnItems, order, skip, limit);
		this.where = where;
	}

	@Override
	public void accept(Visitor visitor) {

		if (visitor.enterWithResult(this) == EnterResult.CONTINUE) {
			Visitable.visitIfNotNull(this.distinct, visitor);
			this.body.accept(visitor);
			Visitable.visitIfNotNull(this.where, visitor);
		}
		visitor.leave(this);
	}

	@Override
	public String toString() {
		return RendererBridge.render(this);
	}

	@API(status = INTERNAL)
	public List<Expression> getItems() {
		return this.body.getReturnItems();
	}

}
