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
package org.neo4j.cypherdsl.parser;

import java.util.Objects;

import org.neo4j.cypherdsl.core.Clause;
import org.neo4j.cypherdsl.core.Expression;
import org.neo4j.cypherdsl.core.Return;

/**
 * Essentially a union type.
 *
 * @param clause a clause that is wrapped
 * @param returnFactory or a return factory leading to a clause that is wrapped
 * @author Michael J. Simons
 * @since 2025.3.0
 */
record ClauseOrReturnFactory(Clause clause, ReturnFactory returnFactory) {

	ClauseOrReturnFactory(Clause clause) {
		this(clause, null);
	}

	ClauseOrReturnFactory(ReturnFactory returnFactory) {
		this(null, returnFactory);
	}

	Clause toClause() {
		return Objects.requireNonNullElseGet(this.clause,
				() -> this.returnFactory.create(new ReturnContext(ExpressionCreatedEventType.ON_RETURN_ITEM)));
	}

	/**
	 * Contains the event that ultimately should be fire when actually creating the
	 * expression.
	 *
	 * @param eventType the event to be fire.
	 */
	record ReturnContext(ExpressionCreatedEventType eventType) {
	}

	/**
	 * Needed to postpone creation of RETURN clause until we know whether we are part of
	 * WITH clause or not.
	 */
	interface ReturnFactory {

		Return create(ReturnContext context);

	}

	/**
	 * Needed to postpone creation of RETURN clause until we know whether we are part of
	 * WITH clause or not.
	 */
	interface ReturnExpressionFactory {

		Expression create(ReturnContext context);

	}
}
