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

import java.util.List;
import java.util.Optional;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitor;

/**
 * Added for supporting the Neo5j parser.
 *
 * @author Michael J. Simons
 * @soundtrack The Prodigy - Invaders Must Die
 * @since 2023.0.0
 */
@API(status = STABLE, since = "2023.0.0")
public final class CountExpression implements Expression {

	public static CountExpression of(List<PatternElement> elements, Optional<Expression> where) {
		return new CountExpression(new Pattern(elements), where
			.map(Expression::asCondition)
			.map(Where::new)
			.orElse(null)
		);
	}

	private final Pattern pattern;

	private final Where optionalWhere;

	private CountExpression(Pattern pattern, Where optionalWhere) {
		this.pattern = pattern;
		this.optionalWhere = optionalWhere;
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		this.pattern.accept(visitor);
		if (optionalWhere != null) {
			this.optionalWhere.accept(visitor);
		}
		visitor.leave(this);
	}
}
