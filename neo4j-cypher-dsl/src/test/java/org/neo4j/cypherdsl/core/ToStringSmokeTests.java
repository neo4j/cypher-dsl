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

import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.neo4j.cypherdsl.core.ast.Visitable;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatNoException;

/**
 * @author Michael J. Simons
 */
class ToStringSmokeTests {

	static Stream<Arguments> toStringShouldWork() {

		return Stream.of(Arguments.of(Cypher.node("Person").named("n"), "(n:Person)"),
				Arguments.of(Cypher.node("Person")
					.named("p")
					.relationshipTo(Cypher.node("Movie").named("m"), "PLAYED_IN")
					.named("r"), "(p:Person)-[r:PLAYED_IN]->(m:Movie)"),
				Arguments.of(
						Cypher.node("Person")
							.named("p")
							.relationshipTo(Cypher.node("Movie").named("m"), "PLAYED_IN")
							.named("r")
							.relationshipFrom(Cypher.node("Person").named("d"), "DIRECTED"),
						"(p:Person)-[r:PLAYED_IN]->(m:Movie)<-[:DIRECTED]-(d:Person)"),
				Arguments.of(Cypher.elementId(Cypher.anyNode("n")), "elementId(n)"),
				Arguments.of(Cypher.call("db.labels").asFunction(), "db.labels()"),
				Arguments.of(Cypher.literalOf("aString"), "'aString'"), Arguments.of(Cypher.literalOf(1), "1"),
				Arguments.of(Cypher.parameter("p"), "$p"),
				Arguments.of(Cypher.anyNode("n").property("prop"), "n.prop"));
	}

	@ParameterizedTest
	@MethodSource
	void toStringShouldWork(Visitable visitable, String expected) {
		assertThat(visitable).hasToString(visitable.getClass().getSimpleName() + "{cypher=" + expected + "}");
	}

	@Test
	void mostConstructsShouldNotHaveDefaultToString() {
		var statement = IssueRelatedIT.createSomewhatComplexStatement();

		statement.accept(segment -> {
			if (segment.getClass().getPackage().getName().equals("org.neo4j.cypherdsl.core.internal")
					|| segment instanceof Statement) {
				return;
			}
			try {

				boolean hasOverwrittenToString = (segment.getClass()
					.getMethod("toString")
					.getDeclaringClass() != Object.class);
				assertThat(hasOverwrittenToString).isTrue();
				assertThatNoException().isThrownBy(segment::toString);
			}
			catch (NoSuchMethodException ex) {
				throw new RuntimeException(ex);
			}
		});
	}

}
