/*
 * Copyright (c) 2019-2026 "Neo4j,"
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitor;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * See <a href=
 * "https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/Set.html">Set</a>.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public final class Set extends AbstractClause implements UpdatingClause {

	private final ExpressionList setItems;

	Set(ExpressionList setItems) {
		this.setItems = setItems;
	}

	/**
	 * Creates a {@literal SET} clause based on the given updates. No runtime checks are
	 * whether those expressions are actually updates. Please be mindful here.
	 * @param update the update to apply
	 * @param more additional updates
	 * @return a {@link Set} clause
	 * @since 2023.4.0
	 */
	static Set set(Expression update, Expression... more) {

		if (more == null || more.length == 0) {
			return new Set(new ExpressionList(List.of(update)));
		}

		List<Expression> finalExpressionList = new ArrayList<>();
		finalExpressionList.add(update);
		Collections.addAll(finalExpressionList, more);

		return new Set(new ExpressionList(finalExpressionList));
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		this.setItems.accept(visitor);
		visitor.leave(this);
	}

}
