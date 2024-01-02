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

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;

import java.util.Collections;

import org.junit.jupiter.api.Test;
import org.neo4j.cypherdsl.core.ast.Visitor;

/**
 * @author Michael J. Simons
 */
class ClausesTest {

	@Test
	void updatingClausesShouldBeCheckInForeach() {

		Clause anonymousClause = new Clause() {
			@Override public void accept(Visitor visitor) {
				Clause.super.accept(visitor);
			}
		};

		assertThatIllegalArgumentException().isThrownBy(() ->
			Clauses.forEach(SymbolicName.of("x"), Cypher.literalNull(), Collections.singletonList(anonymousClause))
		).withMessage(
			"Only updating clauses SET, REMOVE, CREATE, MERGE, DELETE, and FOREACH are allowed as clauses applied inside FOREACH.");
	}

	@Test
	void foreachShouldWork() {

		Clause foreach = Clauses.forEach(SymbolicName.of("x"), Cypher.literalNull(),
			Collections.singletonList(new Set(new ExpressionList())));
		assertThat(foreach).isNotNull();
	}
}
