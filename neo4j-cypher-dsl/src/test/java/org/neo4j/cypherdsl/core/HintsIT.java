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
package org.neo4j.cypherdsl.core;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.assertThatIllegalStateException;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.neo4j.cypherdsl.core.renderer.Renderer;

/**
 * @author Michael J. Simons
 * @soundtrack Pearl Jam - Vitalogy
 */
class HintsIT {

	private static final Renderer cypherRenderer = Renderer.getDefaultRenderer();

	private Node liskov = Cypher.node("Scientist").named("liskov").withProperties("name", Cypher.literalOf("Liskov"));
	private Node wing = Cypher.node("Scientist").named("wing");
	private Node conway = Cypher.node("Scientist").named("conway").withProperties("name", Cypher.literalOf("Conway"));
	private Node cs = Cypher.node("Science").named("cs").withProperties("name", Cypher.literalOf("Computer Science"));

	@Nested
	class IndexHints {

		@Test
		void usingIndexShouldWork() {

			Statement statement = Cypher.match(liskov.relationshipTo(wing, "KNOWS").relationshipTo(cs, "RESEARCHED")
				.relationshipFrom(conway, "RESEARCHED"))
				.usingIndex(liskov.property("name"))
				.returning(liskov.property("born").as("column"))
				.build();

			assertThat(cypherRenderer.render(statement))
				.isEqualTo(
					"MATCH (liskov:`Scientist` {name: 'Liskov'})-[:`KNOWS`]->(wing:`Scientist`)-[:`RESEARCHED`]->(cs:`Science` {name: 'Computer Science'})<-[:`RESEARCHED`]-(conway:`Scientist` {name: 'Conway'}) "
					+ "USING INDEX liskov:`Scientist`(name) "
					+ "RETURN liskov.born AS column");
		}

		@Test
		void usingIndexSeekShouldWork() {

			Statement statement = Cypher.match(liskov.relationshipTo(wing, "KNOWS").relationshipTo(cs, "RESEARCHED")
				.relationshipFrom(conway, "RESEARCHED"))
				.usingIndexSeek(liskov.property("name"))
				.returning(liskov.property("born").as("column"))
				.build();

			assertThat(cypherRenderer.render(statement))
				.isEqualTo(
					"MATCH (liskov:`Scientist` {name: 'Liskov'})-[:`KNOWS`]->(wing:`Scientist`)-[:`RESEARCHED`]->(cs:`Science` {name: 'Computer Science'})<-[:`RESEARCHED`]-(conway:`Scientist` {name: 'Conway'}) "
					+ "USING INDEX SEEK liskov:`Scientist`(name) "
					+ "RETURN liskov.born AS column");
		}

		@Test
		void usingCompositeIndexesShouldWork() {

			Node m = Cypher.node("Method").named("m")
				.withProperties("name", Cypher.literalOf("Foo"), "type", Cypher.literalOf("Bar"));
			Statement statement = Cypher.match(m)
				.usingIndex(m.property("name"), m.property("type"))
				.returning(m)
				.build();

			assertThat(cypherRenderer.render(statement))
				.isEqualTo(
					"MATCH (m:`Method` {name: 'Foo', type: 'Bar'}) "
					+ "USING INDEX m:`Method`(name, type) "
					+ "RETURN m");
		}

		@Test
		void propertiesOfDifferentNodesCannotBeUsedOnTheSameHInt() {

			Node m = Cypher.node("Method").named("m")
				.withProperties("name", Cypher.literalOf("Foo"), "type", Cypher.literalOf("Bar"));

			assertThatIllegalStateException().isThrownBy(() -> Cypher.match(m)
				.usingIndex(m.property("name"), Cypher.node("Method").named("x").property("type"))
				.returning(m)
				.build())
				.withMessage("If you want to use more than one index on different nodes you must use multiple `USING INDEX` statements.");
		}

		@Test
		void nestedPropertiesAreNotAllowed() {

			Node m = Cypher.node("Method").named("m")
				.withProperties("name", Cypher.literalOf("Foo"), "type", Cypher.literalOf("Bar"));

			assertThatIllegalArgumentException().isThrownBy(() -> Cypher.match(m)
				.usingIndex(m.property("a", "name"))
				.returning(m)
				.build())
				.withMessage("One single property is required. Nested properties are not supported.");
		}

		@Test
		void multipleLabelsAreNotAllowed() {

			Node m = Cypher.node("Method", "Foo").named("m")
				.withProperties("name", Cypher.literalOf("Foo"), "type", Cypher.literalOf("Bar"));

			assertThatIllegalArgumentException().isThrownBy(() -> Cypher.match(m)
				.usingIndex(m.property("name"))
				.returning(m)
				.build())
				.withMessage("Exactly one label is required to define the index.");
		}

		@Test
		void propertiesMustReferenceANode() {

			Node m = Cypher.node("Method").named("m")
				.withProperties("name", Cypher.literalOf("Foo"), "type", Cypher.literalOf("Bar"));

			assertThatIllegalArgumentException().isThrownBy(() -> Cypher.match(m)
				.usingIndex(Cypher.property("m", "name"))
				.returning(m)
				.build())
				.withMessage("Cannot use a property without a reference to a container inside an index hint.");
		}

		@Neo4jVersion(minimum = "4.3")
		@Test
		void indexesOnRelationshipsShouldWork() {

			Node m = Cypher.node("Method").named("m")
				.withProperties("name", Cypher.literalOf("Foo"), "type", Cypher.literalOf("Bar"));

			Relationship r = m.relationshipTo(Cypher.node("Method"), "CALLS")
				.named("r")
				.withProperties("whatever", Cypher.literalTrue());

			Statement statement = Cypher.match(r)
				.usingIndex(r.property("whatever"))
				.returning(m)
				.build();

			assertThat(cypherRenderer.render(statement))
				.isEqualTo("MATCH (m:`Method` {name: 'Foo', type: 'Bar'})-[r:`CALLS` {whatever: true}]->(:`Method`) USING INDEX r:`CALLS`(whatever) RETURN m");
		}

		@Test
		void multipleIndizesShouldWork() {

			Statement statement = Cypher.match(liskov.relationshipTo(wing, "KNOWS").relationshipTo(cs, "RESEARCHED")
				.relationshipFrom(conway, "RESEARCHED"))
				.usingIndex(liskov.property("name"))
				.usingIndex(conway.property("name"))
				.returning(liskov.property("born").as("column"))
				.build();

			assertThat(cypherRenderer.render(statement))
				.isEqualTo(
					"MATCH (liskov:`Scientist` {name: 'Liskov'})-[:`KNOWS`]->(wing:`Scientist`)-[:`RESEARCHED`]->(cs:`Science` {name: 'Computer Science'})<-[:`RESEARCHED`]-(conway:`Scientist` {name: 'Conway'}) "
					+ "USING INDEX liskov:`Scientist`(name) "
					+ "USING INDEX conway:`Scientist`(name) "
					+ "RETURN liskov.born AS column");
		}
	}

	@Nested
	class ScanHints {

		@Test
		void usingScanHintsShouldWork() {

			Node s = Cypher.node("Scientist").named("s");
			Statement statement = Cypher.match(s)
				.usingScan(s)
				.where(s.property("born").lt(Cypher.literalOf(1939)))
				.returning(s.property("born").as("column"))
				.build();

			assertThat(cypherRenderer.render(statement))
				.isEqualTo(
					"MATCH (s:`Scientist`) "
					+ "USING SCAN s:`Scientist` "
					+ "WHERE s.born < 1939 "
					+ "RETURN s.born AS column");
		}

		@Test
		void scansCanNotBeUsedWithMultipleLabels() {

			Assertions.assertThatIllegalArgumentException()
				.isThrownBy(() -> Cypher.match(Cypher.node("Scientist").named("s"))
					.usingScan(Cypher.node("Scientist", "Other").named("s"))
					.returning(SymbolicName.of("s"))
					.build())
				.withMessage("Exactly one label is required for a SCAN hint.");
		}

		@Test
		void scansCanNotBeUsedWithoutLabels() {

			Assertions.assertThatIllegalArgumentException()
				.isThrownBy(() -> Cypher.match(Cypher.node("Scientist").named("s"))
					.usingScan(Cypher.anyNode())
					.returning(SymbolicName.of("s"))
					.build())
				.withMessage("Exactly one label is required for a SCAN hint.");
		}

		@Test
		void scansNeedANode() {

			Assertions.assertThatIllegalArgumentException()
				.isThrownBy(() -> Cypher.match(Cypher.node("Scientist").named("s"))
					.usingScan(null)
					.returning(SymbolicName.of("s"))
					.build())
				.withMessage("Cannot apply a SCAN hint without a node.");
		}
	}

	@Nested
	class JoinHints {

		@Test
		void singleJoinHintShouldWork() {
			Statement statement = Cypher.match(liskov.relationshipTo(wing, "KNOWS").relationshipTo(cs, "RESEARCHED")
				.relationshipFrom(conway, "RESEARCHED"))
				.usingIndex(liskov.property("name"))
				.usingIndex(conway.property("name"))
				.usingJoinOn(wing)
				.returning(liskov.property("born").as("column"))
				.build();

			assertThat(cypherRenderer.render(statement))
				.isEqualTo(
					"MATCH (liskov:`Scientist` {name: 'Liskov'})-[:`KNOWS`]->(wing:`Scientist`)-[:`RESEARCHED`]->(cs:`Science` {name: 'Computer Science'})<-[:`RESEARCHED`]-(conway:`Scientist` {name: 'Conway'}) "
					+ "USING INDEX liskov:`Scientist`(name) "
					+ "USING INDEX conway:`Scientist`(name) "
					+ "USING JOIN ON wing "
					+ "RETURN liskov.born AS column");
		}

		@Test
		void multipleJoinHintShouldWork() {
			Statement statement = Cypher.match(
				liskov
					.relationshipTo(
						Cypher.node("Scientist").named("wing").withProperties("name", Cypher.literalOf("Wing")),
						"KNOWS")
					.relationshipTo(cs, "RESEARCHED")
					.relationshipFrom(liskov, "RESEARCHED")
			)
				.usingIndex(liskov.property("name"))
				.usingJoinOn(liskov, cs)
				.returning(wing.property("born").as("column"))
				.build();

			assertThat(cypherRenderer.render(statement))
				.isEqualTo(
					"MATCH (liskov:`Scientist` {name: 'Liskov'})-[:`KNOWS`]->(wing:`Scientist` {name: 'Wing'})-[:`RESEARCHED`]->(cs:`Science` {name: 'Computer Science'})<-[:`RESEARCHED`]-(liskov) "
					+ "USING INDEX liskov:`Scientist`(name) "
					+ "USING JOIN ON liskov, cs "
					+ "RETURN wing.born AS column");
		}

		@Test
		void joinWithoutOtherIndexesShouldWork() {
			Statement statement = Cypher.match(liskov.relationshipTo(wing, "KNOWS").relationshipTo(cs, "RESEARCHED")
				.relationshipFrom(conway, "RESEARCHED"))
				.usingJoinOn(wing)
				.returning(liskov.property("born").as("column"))
				.build();

			assertThat(cypherRenderer.render(statement))
				.isEqualTo(
					"MATCH (liskov:`Scientist` {name: 'Liskov'})-[:`KNOWS`]->(wing:`Scientist`)-[:`RESEARCHED`]->(cs:`Science` {name: 'Computer Science'})<-[:`RESEARCHED`]-(conway:`Scientist` {name: 'Conway'}) "
					+ "USING JOIN ON wing "
					+ "RETURN liskov.born AS column");
		}

		@Test
		void joinRequiresNodeName() {

			Assertions.assertThatIllegalArgumentException()
				.isThrownBy(() -> Cypher.match(liskov.relationshipTo(wing, "KNOWS").relationshipTo(cs, "RESEARCHED")
					.relationshipFrom(conway, "RESEARCHED"))
					.usingJoinOn(new Node[0])
					.returning(liskov.property("born").as("column"))
					.build())
				.withMessage("At least one node is required to define a JOIN hint.");
		}

		@Test
		void joinRequiresSymbolicName() {

			Assertions.assertThatIllegalArgumentException()
				.isThrownBy(() -> Cypher.match(liskov.relationshipTo(wing, "KNOWS").relationshipTo(cs, "RESEARCHED")
					.relationshipFrom(conway, "RESEARCHED"))
					.usingJoinOn(new SymbolicName[0])
					.returning(liskov.property("born").as("column"))
					.build())
				.withMessage("At least one name is required to define a JOIN hint.");
		}
	}
}
