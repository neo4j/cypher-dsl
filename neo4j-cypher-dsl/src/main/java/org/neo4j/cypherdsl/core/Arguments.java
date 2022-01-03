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

import static org.apiguardian.api.API.Status.INTERNAL;

import java.util.Optional;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.ProvidesAffixes;
import org.neo4j.cypherdsl.core.ast.TypedSubtree;
import org.neo4j.cypherdsl.core.ast.Visitable;

/**
 * Specialized list of expressions that represents the arguments of a procedure call.
 *
 * @author Michael J. Simons
 * @soundtrack Apocalyptica - Cell-0
 * @since 2020.0.1
 */
@API(status = INTERNAL, since = "2020.0.1")
final class Arguments extends TypedSubtree<Expression> implements ProvidesAffixes {

	Arguments(Expression... children) {
		super(children);
	}

	@Override
	protected Visitable prepareVisit(Expression child) {
		return Expressions.nameOrExpression(child);
	}

	@Override
	public Optional<String> getPrefix() {
		return Optional.of("(");
	}

	@Override
	public Optional<String> getSuffix() {
		return Optional.of(")");
	}
}
