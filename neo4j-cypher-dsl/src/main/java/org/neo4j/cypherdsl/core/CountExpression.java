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

import java.util.List;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;

/**
 * Added for supporting the Neo4j v5 parser.
 *
 * @author Michael J. Simons
 * @soundtrack The Prodigy - Invaders Must Die
 * @since 2023.0.0
 */
@API(status = STABLE, since = "2023.0.0")
@Neo4jVersion(minimum = "5.0")
public final class CountExpression implements SubqueryExpression, ExposesWhere<Expression> {

	private final ImportingWith importingWith;
	private final List<Visitable> fragments;
	private final Where innerWhere;

	static CountExpression count(Statement statement, IdentifiableElement... imports) {

		return new CountExpression(ImportingWith.of(imports), List.of(statement), null);
	}

	static CountExpression count(Visitable patternOrUnion) {
		return new CountExpression(new ImportingWith(), List.of(patternOrUnion), null);
	}

	static CountExpression count(With optionalWith, Visitable patternOrUnion) {
		return new CountExpression(new ImportingWith(optionalWith, null), List.of(patternOrUnion), null);
	}

	static CountExpression count(List<PatternElement> patternElements, Where innerWhere) {
		return new CountExpression(new ImportingWith(), patternElements, innerWhere);
	}

	private CountExpression(ImportingWith optionalWith, List<? extends Visitable> fragments, Where innerWhere) {

		var patternOrUnion = fragments.size() == 1 ? fragments.get(0) : null;

		if (patternOrUnion instanceof Statement.UnionQuery && innerWhere != null) {
			throw new IllegalArgumentException("Cannot use a UNION with a WHERE clause inside a COUNT {} expression");
		}
		this.importingWith = optionalWith;
		var imports = optionalWith.imports();

		if (imports != null && patternOrUnion instanceof Pattern pattern) {
			this.fragments = List.of(new Match(false, pattern, innerWhere, null));
			this.innerWhere = null;
		} else if (imports != null && patternOrUnion instanceof Match match) {
			this.fragments = List.of(new Match(false, match.pattern, innerWhere, null));
			this.innerWhere = null;
		} else {
			this.fragments = List.copyOf(fragments);
			this.innerWhere = innerWhere;
		}
	}

	/**
	 * Creates a new {@link CountExpression count expression} with additional conditions
	 *
	 * @param condition the condition to apply in the count expression
	 * @return A new {@link CountExpression}
	 */
	public CountExpression where(Condition condition) {

		if (fragments.size() == 1 && fragments.get(0) instanceof Statement) {
			throw new IllegalArgumentException(
				"This count expression is build upon a full statement, adding a condition to it is not supported.");
		}
		return new CountExpression(importingWith, fragments, new Where(condition));
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		importingWith.accept(visitor);
		fragments.forEach(v -> v.accept(visitor));
		Visitable.visitIfNotNull(innerWhere, visitor);
		visitor.leave(this);
	}
}
