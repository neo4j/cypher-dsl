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
package org.neo4j.cypherdsl.core.internal;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.Expression;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;

import static org.apiguardian.api.API.Status.INTERNAL;

/**
 * Represents a pair of `when-then` expressions.
 *
 * @author Gerrit Meier
 * @author Michael J. Simons
 */
@API(status = INTERNAL, since = "1.0")
public final class CaseWhenThen implements Visitable {

	private final Expression whenExpression;

	private final Expression thenExpression;

	/**
	 * Creates w new instance.
	 * @param whenExpression intermediate when part
	 * @param thenExpression the then part to happen after matching when above
	 */
	public CaseWhenThen(Expression whenExpression, Expression thenExpression) {

		this.whenExpression = whenExpression;
		this.thenExpression = thenExpression;
	}

	@Override
	public void accept(Visitor visitor) {
		visitor.enter(this);
		this.whenExpression.accept(visitor);
		visitor.leave(this);
		this.thenExpression.accept(visitor);
	}

}
