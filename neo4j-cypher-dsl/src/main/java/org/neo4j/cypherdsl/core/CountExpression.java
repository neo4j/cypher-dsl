/*
 * Copyright (c) 2019-2023 "Neo4j,"
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
import java.util.Optional;

import org.apiguardian.api.API;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;

/**
 * Added for supporting the Neo5j parser.
 *
 * @author Michael J. Simons
 * @soundtrack The Prodigy - Invaders Must Die
 * @since 2023.0.0
 */
// TODO Consider an interface from the start
@API(status = STABLE, since = "2023.0.0")
public final class CountExpression implements Expression, ExposesWhere<Expression> {

	// TODO make private
	public static CountExpression of(List<PatternElement> elements, Optional<Expression> where) {
		return new CountExpression(new Pattern(elements), where
			.map(Expression::asCondition)
			.map(Where::new)
			.orElse(null)
		);
	}

	private final Visitable patternOrUnion;

	private final Where optionalWhere;

	CountExpression(Visitable patternOrUnion, @Nullable Where optionalWhere) {
		if (patternOrUnion instanceof Statement.UnionQuery && optionalWhere != null) {
			throw new IllegalArgumentException("Cannot use a UNION with a WHERE clause inside a COUNT {} expression");
		}
		this.patternOrUnion = patternOrUnion;
		this.optionalWhere = optionalWhere;
	}

	/**
	 * Creates a new {@link CountExpression count expression} with additional conditions
	 *
	 * @param condition the condition to apply in the count expression
	 * @return A new {@link CountExpression}
	 */
	@NotNull @Contract(pure = true)
	public CountExpression where(Condition condition) {

		return new CountExpression(patternOrUnion, new Where(condition));
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		this.patternOrUnion.accept(visitor);
		if (optionalWhere != null) {
			this.optionalWhere.accept(visitor);
		}
		visitor.leave(this);
	}
}
