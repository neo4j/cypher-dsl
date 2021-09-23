/*
 * Copyright (c) 2019-2021 "Neo4j,"
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

import java.util.Collection;

import org.junit.jupiter.api.Test;

class IdentifiableExpressionsIT {

	@Test
	void simpleWith() {
		Node b = Cypher.anyNode("b");
		Collection<Expression> variables = Cypher.match(Cypher.node("Label").named("a").relationshipTo(b))
			.with(Cypher.name("a"), b).getIdentifiableExpressions();

		assertThat(variables.stream().map(Cypher::format))
			.containsExactlyInAnyOrder("a", "b");
	}

	@Test
	void multipleHops() {
		Node b = Cypher.anyNode("b");
		SymbolicName a = Cypher.name("a");
		Collection<Expression> variables = Cypher.match(Cypher.node("Label").named("a").relationshipTo(b))
			.with(a, b)
			.optionalMatch(b.relationshipTo(Cypher.anyNode("c")))
			.with(a, Cypher.name("c"))
			.getIdentifiableExpressions();

		assertThat(variables.stream().map(Cypher::format))
			.containsExactlyInAnyOrder("a", "c");
	}

	@Test
	void complexWith() {
		Node b = Cypher.anyNode("b");
		Node a = Cypher.node("Label").named("a");
		Collection<Expression> variables = Cypher.match(a.relationshipTo(b))
			.with(a, b)
			.optionalMatch(b.relationshipTo(Cypher.anyNode("c")))
			.with(
				a,
				Cypher.name("c"),
				Cypher.caseExpression()
					.when(Cypher.name("c").isNull().or(Conditions.hasLabelsOrType(Cypher.name("c"), "Label")))
					.then(Cypher.literalTrue())
					.elseDefault(Cypher.literalFalse())
					.as("isNullOrLabel")
			)
			.getIdentifiableExpressions();

		assertThat(variables.stream().map(Cypher::format))
			.containsExactlyInAnyOrder("a", "c", "isNullOrLabel");
	}

	@Test
	void complexWithAsStatement() {
		Node b = Cypher.anyNode("b");
		Node a = Cypher.node("Label").named("a");
		Collection<Expression> variables = Cypher.match(a.relationshipTo(b))
			.with(a, b)
			.optionalMatch(b.relationshipTo(Cypher.anyNode("c")))
			.returning(
				a.getRequiredSymbolicName(),
				Cypher.name("c"),
				Cypher.caseExpression()
					.when(Cypher.name("c").isNull().or(Conditions.hasLabelsOrType(Cypher.name("c"), "Label")))
					.then(Cypher.literalTrue())
					.elseDefault(Cypher.literalFalse())
					.as("isNullOrLabel")
			)
			.build()
			.getIdentifiableExpressions();

		assertThat(variables.stream().map(Cypher::format))
			.containsExactlyInAnyOrder("a", "c", "isNullOrLabel");
	}

	@Test
	void noneExpressionsInReturn() {
		Node b = Cypher.anyNode("b");
		Node a = Cypher.node("Label").named("a");
		Collection<Expression> variables = Cypher.match(a.relationshipTo(b))
			.returning(a)
			.build()
			.getIdentifiableExpressions();

		assertThat(variables.stream().map(Cypher::format))
			.containsExactlyInAnyOrder("a");
	}

	@Test
	void nonIdentifiableItems() {
		Node b = Cypher.anyNode("b");
		Node a = Cypher.node("Label").named("a");
		Collection<Expression> variables = Cypher.match(a.relationshipTo(b))
			.with(Cypher.literalOf(1), b.as("007"))
			.getIdentifiableExpressions();

		assertThat(variables.stream().map(Cypher::format))
			.containsExactlyInAnyOrder("007");
	}

	@Test
	void onReturn() {
		Node b = Cypher.anyNode("b");
		Collection<Expression> variables = Cypher.returning(Cypher.literalOf(1), b.as("007"), b.property("f"))
			.getIdentifiableExpressions();

		assertThat(variables.stream().map(Cypher::format))
			.containsExactlyInAnyOrder("007", "b.f");
	}

	@Test
	void returnedProperties() {

		Node b = Cypher.anyNode("b");
		Collection<Expression> variables = Cypher
			.match(b)
			.returning(b.property("x"), b.as("007"))
			.getIdentifiableExpressions();

		assertThat(variables.stream().map(Cypher::format))
			.containsExactlyInAnyOrder("b.x", "007");
	}
}
