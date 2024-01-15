/*
 * Copyright (c) 2019-2024 "Neo4j,"
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

import org.neo4j.cypherdsl.core.ast.TypedSubtree;
import org.neo4j.cypherdsl.core.ast.Visitable;

/**
 * Represents a list of expressions. When visited, the expressions are treated as named expression if they have declared
 * a symbolic name as variable or as unnamed expression when nameless.
 * <p>
 * Not to be mixed up with the actual {@link ListExpression}, which itself is an expression.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
class ExpressionList extends TypedSubtree<Expression> {

	ExpressionList(List<Expression> returnItems) {
		super(returnItems);
	}

	ExpressionList(Expression... children) {
		super(children);
	}

	@Override
	protected Visitable prepareVisit(Expression child) {
		return Expressions.nameOrExpression(child);
	}

	boolean isEmpty() {
		return super.children.isEmpty();
	}
}
