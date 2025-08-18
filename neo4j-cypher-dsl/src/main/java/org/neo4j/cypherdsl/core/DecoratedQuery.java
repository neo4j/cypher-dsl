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
import org.neo4j.cypherdsl.core.Statement.UseStatement;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;

import static org.apiguardian.api.API.Status.INTERNAL;

/**
 * A decorated statement. Used for <code>EXPLAIN</code> and <code>PROFILE</code>'d
 * queries.
 *
 * @author Michael J. Simons
 * @since 2020.1.2
 */
@API(status = INTERNAL, since = "2020.1.2")
sealed class DecoratedQuery extends AbstractStatement implements UseStatement {

	private final Visitable decoration;

	private final Statement target;

	private DecoratedQuery(Statement target, Visitable decoration) {
		this.decoration = decoration;
		this.target = target;
	}

	static DecoratedQuery explain(Statement target) {

		return DecoratedQuery.decorate(target, Decoration.EXPLAIN);
	}

	static DecoratedQuery profile(Statement target) {

		return DecoratedQuery.decorate(target, Decoration.PROFILE);
	}

	private static DecoratedQuery decorate(Statement target, Decoration decoration) {

		if (target instanceof DecoratedQuery decoratedQuery && !(decoratedQuery.decoration instanceof Use)) {
			throw new IllegalArgumentException("Cannot explain an already explained or profiled query.");
		}

		if (target instanceof ResultStatement && Decoration.PROFILE.equals(decoration)) {
			return new DecoratedQueryWithResult(target);
		}

		return new DecoratedQuery(target, decoration);
	}

	static DecoratedQuery decorate(Statement target, Use use) {

		if (target instanceof DecoratedQuery decoratedQuery) {
			String message;
			if (decoratedQuery.decoration instanceof Decoration decoration) {
				message = decoration.name() + (decoration.name().endsWith("E") ? "'" : "'e")
						+ "d statements are not supported inside USE clauses";
			}
			else { // Right now the only other decoration is the Use clause.
				message = "Nested USE clauses are not supported";
			}
			throw new IllegalArgumentException(message);
		}

		return new DecoratedQuery(target, use);
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		this.decoration.accept(visitor);
		this.target.accept(visitor);
		visitor.leave(this);
	}

	@Override
	public boolean doesReturnOrYield() {
		if (this.decoration instanceof Use) {
			return this.target.doesReturnOrYield();
		}
		return super.doesReturnOrYield();
	}

	private enum Decoration implements Visitable {

		EXPLAIN, PROFILE;

		@Override
		public String toString() {
			return RendererBridge.render(this);
		}

	}

	/**
	 * Only profiled queries can have a result statement.
	 */
	static final class DecoratedQueryWithResult extends DecoratedQuery implements ResultStatement {

		private DecoratedQueryWithResult(Statement target) {
			super(target, Decoration.PROFILE);
		}

	}

}
