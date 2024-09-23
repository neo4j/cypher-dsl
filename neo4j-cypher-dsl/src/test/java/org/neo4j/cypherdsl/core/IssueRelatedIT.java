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

import java.time.ZonedDateTime;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Stream;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.MethodSource;
import org.neo4j.cypherdsl.core.renderer.Configuration;
import org.neo4j.cypherdsl.core.renderer.Dialect;
import org.neo4j.cypherdsl.core.renderer.Renderer;

/**
 * @author Michael J. Simons
 */
class IssueRelatedIT {

	private static final Renderer cypherRenderer = Renderer.getDefaultRenderer();
	private final Node person = Cypher.node("Person").named("person");

	@Test
	void gh266SizeShouldBeSupported() {

		Node node = Cypher.node("Node").named("node");
		String cypher = Cypher.match(node).returning(node.property("thing").size()).build().getCypher();
		assertThat(cypher).isEqualTo("MATCH (node:`Node`) RETURN size(node.thing)");
	}

	@Test
	void gh266HasSizeUtilityShouldWork() {

		Node node = Cypher.node("Node").named("node");
		String cypher = Cypher.match(node).where(node.property("thing").hasSize(Cypher.literalOf(0))).returning(Cypher.asterisk()).build().getCypher();
		assertThat(cypher).isEqualTo("MATCH (node:`Node`) WHERE size(node.thing) = 0 RETURN *");
	}

	@Test
	void gh115() {
		Node nodes = Cypher.node("Node").named("node").withProperties("id", Cypher.literalOf("node_42"));
		StatementBuilder.OngoingReadingWithoutWhere matchNodes = Cypher.match(nodes);

		NamedPath p = Cypher.path(Cypher.name("path")).get();
		Statement statement = matchNodes
			.call("apoc.path.spanningTree")
			.withArgs(
				nodes.getRequiredSymbolicName(),
				Cypher.mapOf(
					"relationshipFilter",
					Cypher.literalOf("<rel_filter>"),
					"labelFilter",
					Cypher.literalOf("<label_filter>")))
			.yield(p)
			.returningDistinct(nodes.getRequiredSymbolicName(),
				Cypher.collect(Cypher.relationships(p)).as("rels"),
				Cypher.collect(Cypher.nodes(p)).as("nodes")).build();

		assertThat(cypherRenderer.render(statement))
				.isEqualTo("MATCH (node:`Node` {id: 'node_42'}) "
						+ "CALL apoc.path.spanningTree(node, {relationshipFilter: '<rel_filter>', labelFilter: '<label_filter>'}) YIELD path "
						+ "RETURN DISTINCT node, collect(relationships(path)) AS rels, collect(nodes(path)) AS nodes");
	}

	@Test
	void gh70() {
		Node strawberry = Cypher.node("Fruit", Cypher.mapOf("kind", Cypher.literalOf("strawberry")));
		Statement statement = Cypher
			.match(strawberry).set(strawberry.property("color").to(Cypher.literalOf("red")))
			.build();

		assertThat(cypherRenderer.render(statement))
			.matches(
				"MATCH \\([a-zA-Z]*\\d{3}:`Fruit` \\{kind: 'strawberry'}\\) SET [a-zA-Z]*\\d{3}\\.color = 'red'");
	}

	@Test
	void gh167() {
		final Node app = Cypher.node("Location").named("app").withProperties("uuid", Cypher.parameter("app_uuid"));
		final Node locStart = Cypher.node("Location").named("loc_start");
		final Node resume = Cypher.node("Resume").named("r");
		final Node offer = Cypher.node("Offer").named("o");
		final Node startN = Cypher.node("ResumeNode").named("start_n");

		final Relationship aFl = app.relationshipFrom(locStart, "PART_OF").length(0, 3);
		final Relationship lFr = locStart.relationshipFrom(resume, "IN", "IN_ANALYTICS");

		@SuppressWarnings("deprecation") Statement statement = Cypher.match(aFl, lFr)
			.withDistinct(resume, locStart, app)
			.match(resume
				.relationshipTo(offer.withProperties("is_valid", Cypher.literalTrue()), "IN_COHORT_OF")
				.relationshipTo(Cypher.anyNode("app"), "IN")
			)
			.withDistinct(resume, locStart, app, offer)
			.match(offer.relationshipTo(startN, "FOR"))
			.where(Functions.id(startN).in(Cypher.parameter("start_ids")))
			.returningDistinct(resume, locStart, app, offer, startN).build();

		assertThat(cypherRenderer.render(statement))
			.isEqualTo(
				"MATCH (app:`Location` {uuid: $app_uuid})<-[:`PART_OF`*0..3]-(loc_start:`Location`), (loc_start)<-[:`IN`|`IN_ANALYTICS`]-(r:`Resume`) WITH DISTINCT r, loc_start, app MATCH (r)-[:`IN_COHORT_OF`]->(o:`Offer` {is_valid: true})-[:`IN`]->(app) WITH DISTINCT r, loc_start, app, o MATCH (o)-[:`FOR`]->(start_n:`ResumeNode`) WHERE id(start_n) IN $start_ids RETURN DISTINCT r, loc_start, app, o, start_n");
	}

	@Test
	void gh174() {
		final Node r = Cypher.node("Resume").named("r");
		final Node o = Cypher.node("Offer").named("o");

		Statement s = Cypher.match(r.relationshipTo(o, "FOR"))
			.where(r.hasLabels("LastResume").not())
			.and(
				Cypher.coalesce(o.property("valid_only"), Cypher.literalFalse()).isEqualTo(Cypher.literalFalse())
					.and(r.hasLabels("InvalidStatus").not())
					.or(o.property("valid_only").isTrue()
						.and(r.hasLabels("InvalidStatus"))))
			.returningDistinct(r, o)
			.build();

		assertThat(cypherRenderer.render(s))
			.isEqualTo(
				"MATCH (r:`Resume`)-[:`FOR`]->(o:`Offer`) WHERE (NOT (r:`LastResume`) AND ((coalesce(o.valid_only, false) = false AND NOT (r:`InvalidStatus`)) OR (o.valid_only = true AND r:`InvalidStatus`))) RETURN DISTINCT r, o");
	}

	@Test
	void gh184() {
		final Node r = Cypher.node("Resume").named("r");
		final Node u = Cypher.node("UserSearchable").named("u");
		final Node o = Cypher.node("Offer").named("o");

		Statement s = Cypher.match(r.relationshipFrom(u, "HAS"))
			.where(r.hasLabels("LastResume").not())
			.and(
				Cypher.coalesce(o.property("valid_only"), Cypher.literalFalse()).isEqualTo(Cypher.literalFalse())
					.and(r.hasLabels("InvalidStatus").not())
					.or(o.property("valid_only").isTrue()
						.and(r.hasLabels("ValidStatus")))
			)
			.and(r.property("is_internship").isTrue()
				.and(Cypher.size(r.relationshipTo(Cypher.anyNode(), "PART_OF")).isEmpty())
				.not())
			.and(r.property("is_sandwich_training").isTrue()
				.and(Cypher.size(r.relationshipTo(Cypher.anyNode(), "PART_OF")).isEmpty())
				.not())
			.returningDistinct(r, o)
			.build();

		assertThat(cypherRenderer.render(s))
				.isEqualTo("MATCH (r:`Resume`)<-[:`HAS`]-(u:`UserSearchable`) "
						+ "WHERE (NOT (r:`LastResume`) "
						+ "AND ((coalesce(o.valid_only, false) = false "
						+ "AND NOT (r:`InvalidStatus`)) "
						+ "OR (o.valid_only = true "
						+ "AND r:`ValidStatus`)) "
						+ "AND NOT ("
						+ "(r.is_internship = true AND size(size((r)-[:`PART_OF`]->())) = 0)"
						+ ") "
						+ "AND NOT ("
						+ "(r.is_sandwich_training = true AND size(size((r)-[:`PART_OF`]->())) = 0)"
						+ ")"
						+ ") RETURN DISTINCT r, o");
	}

	@Test
	void gh185() {
		final Node r = Cypher.node("Resume").named("r");
		final Node u = Cypher.node("UserSearchable").named("u");

		Statement s = Cypher.match(r.relationshipFrom(u, "HAS"))
			.where(Cypher.not(Cypher.exists(r.relationshipTo(u, "EXCLUDES"))))
			.returningDistinct(r)
			.build();

		assertThat(cypherRenderer.render(s))
			.isEqualTo(
				"MATCH (r:`Resume`)<-[:`HAS`]-(u:`UserSearchable`) WHERE NOT (exists((r)-[:`EXCLUDES`]->(u))) RETURN DISTINCT r");
	}

	@Test
	void gh187() {
		final Node r = Cypher.node("Resume").named("r");
		final Node u = Cypher.node("User").named("u");

		Statement s = Cypher.match(r.relationshipFrom(u, "HAS"))
			.with(Cypher.head(Cypher.collect(r.getRequiredSymbolicName())).as("r"))
			.returning(r)
			.build();

		assertThat(cypherRenderer.render(s))
			.isEqualTo("MATCH (r:`Resume`)<-[:`HAS`]-(u:`User`) WITH head(collect(r)) AS r RETURN r");
	}

	@Test
	void gh188() {
		final Node r = Cypher.node("Resume").named("r");
		final Node u = Cypher.node("User").named("u");

		Statement s = Cypher.match(r.relationshipFrom(u, "HAS"))
			.returning(Cypher.countDistinct(r.getRequiredSymbolicName()).as("r"))
			.build();

		assertThat(cypherRenderer.render(s))
			.isEqualTo("MATCH (r:`Resume`)<-[:`HAS`]-(u:`User`) RETURN count(DISTINCT r) AS r");
	}

	@Test
	void gh197() {

		// avg
		Statement s = Cypher.match(person)
			.returning(Cypher.avg(person.property("age")))
			.build();
		assertThat(cypherRenderer.render(s))
			.isEqualTo("MATCH (person:`Person`) RETURN avg(person.age)");

		// max/min
		final ListExpression list = Cypher.listOf(
			Cypher.literalOf(1),
			Cypher.literalOf("a"),
			Cypher.literalOf(null),
			Cypher.literalOf(0.2),
			Cypher.literalOf("b"),
			Cypher.literalOf("1"),
			Cypher.literalOf("99"));
		s = Cypher.unwind(list).as("val")
			.returning(Cypher.max(Cypher.name("val"))).build();
		assertThat(cypherRenderer.render(s))
			.isEqualTo("UNWIND [1, 'a', NULL, 0.2, 'b', '1', '99'] AS val RETURN max(val)");
		s = Cypher.unwind(list).as("val")
			.returning(Cypher.min(Cypher.name("val"))).build();
		assertThat(cypherRenderer.render(s))
			.isEqualTo("UNWIND [1, 'a', NULL, 0.2, 'b', '1', '99'] AS val RETURN min(val)");

		// percentileCont/percentileDisc
		s = Cypher.match(person)
			.returning(Cypher.percentileCont(person.property("age"), 0.4))
			.build();
		assertThat(cypherRenderer.render(s))
			.isEqualTo("MATCH (person:`Person`) RETURN percentileCont(person.age, 0.4)");
		s = Cypher.match(person)
			.returning(Cypher.percentileDisc(person.property("age"), 0.5))
			.build();
		assertThat(cypherRenderer.render(s))
			.isEqualTo("MATCH (person:`Person`) RETURN percentileDisc(person.age, 0.5)");

		// stDev/stDevP
		s = Cypher.match(person)
			.where(person.property("name").in(
				Cypher.listOf(Cypher.literalOf("A"), Cypher.literalOf("B"), Cypher.literalOf("C"))))
			.returning(Cypher.stDev(person.property("age")))
			.build();
		assertThat(cypherRenderer.render(s))
			.isEqualTo("MATCH (person:`Person`) WHERE person.name IN ['A', 'B', 'C'] RETURN stDev(person.age)");
		s = Cypher.match(person)
			.where(person.property("name").in(
				Cypher.listOf(Cypher.literalOf("A"), Cypher.literalOf("B"), Cypher.literalOf("C"))))
			.returning(Cypher.stDevP(person.property("age")))
			.build();
		assertThat(cypherRenderer.render(s))
			.isEqualTo("MATCH (person:`Person`) WHERE person.name IN ['A', 'B', 'C'] RETURN stDevP(person.age)");

		// sum
		s = Cypher.match(person)
			.with(Cypher.listOf(Cypher.mapOf(
				"type", person.getRequiredSymbolicName(),
				"nb", Cypher.sum(person.getRequiredSymbolicName())))
				.as("counts"))
			.returning(Cypher.sum(person.property("age")))
			.build();
		assertThat(cypherRenderer.render(s))
			.isEqualTo("MATCH (person:`Person`) WITH [{type: person, nb: sum(person)}] AS counts RETURN sum(person.age)");
	}

	@Test
	void gh200() {
		final Node r = Cypher.node("Resume").named("r");

		Statement s = Cypher.match(r)
			.with(r.getRequiredSymbolicName())
			.returningDistinct(r.getRequiredSymbolicName())
			.build();

		assertThat(cypherRenderer.render(s))
			.isEqualTo("MATCH (r:`Resume`) WITH r RETURN DISTINCT r");
	}

	@Test
	void gh204() {
		final Node a = Cypher.node("A").named("a");
		final Node b = Cypher.node("B").named("b");
		final Node c = Cypher.node("C").named("c");

		Statement s = Cypher.match(a.relationshipTo(b).relationshipTo(c).max(2))
			.returning(a)
			.build();

		assertThat(cypherRenderer.render(s))
			.isEqualTo("MATCH (a:`A`)-->(b:`B`)-[*..2]->(c:`C`) RETURN a");
	}

	@Test
	void gh245() {
		String expected = "MATCH (person:`Person`) RETURN person{alias: person.name}";

		Statement statement;
		statement = Cypher.match(person)
			.returning(person.project("alias", person.property("name")))
			.build();
		assertThat(cypherRenderer.render(statement)).isEqualTo(expected);

		statement = Cypher.match(person)
			.returning(person.project(person.property("name").as("alias")))
			.build();
		assertThat(cypherRenderer.render(statement)).isEqualTo(expected);
	}

	@Test
	void gh44() {

		Node n = Cypher.anyNode("n");
		Statement statement = Cypher.match(n)
			.returning(Cypher.collectDistinct(n).as("distinctNodes"))
			.build();
		assertThat(cypherRenderer.render(statement))
			.isEqualTo("MATCH (n) RETURN collect(DISTINCT n) AS distinctNodes");
	}

	@Test
	void gh84() {

		Node parent = Cypher.node("Parent").named("parent");
		Node child = Cypher.node("Child").named("child");
		Statement statement = Cypher.call("apoc.create.relationship")
			.withArgs(
				parent.getRequiredSymbolicName(),
				Cypher.literalOf("ChildEdge"),
				Cypher.mapOf(
					"score", Cypher.literalOf(0.33),
					"weight", Cypher.literalOf(1.7)
				),
				child.getRequiredSymbolicName()
			)
			.yield("rel").build();
		assertThat(cypherRenderer.render(statement))
			.isEqualTo(
				"CALL apoc.create.relationship(parent, 'ChildEdge', {score: 0.33, weight: 1.7}, child) YIELD rel");

	}

	@Test // GH-106
	void aliasesShouldBeEscapedIfNecessary() {

		AliasedExpression alias = Cypher.name("n").as("das ist ein Alias");
		Statement statement = Cypher.match(Cypher.anyNode().named("n"))
			.with(alias)
			.returning(alias).build();

		assertThat(cypherRenderer.render(statement))
			.isEqualTo("MATCH (n) WITH n AS `das ist ein Alias` RETURN `das ist ein Alias`");
	}

	@Test // GH-106
	void projectedPropertiesShouldBeEscapedIfNecessary() {

		Node node = Cypher.anyNode().named("n");
		Statement statement = Cypher.match(node)
			.returning(node.project("property 1", "property 2"))
			.build();

		assertThat(cypherRenderer.render(statement))
			.isEqualTo("MATCH (n) RETURN n{.`property 1`, .`property 2`}");
	}

	@Test // GH-106
	void mapKeysShouldBeEscapedIfNecessary() {

		Statement statement = Cypher
			.returning(Cypher.mapOf("key 1", Cypher.literalTrue(), "key 2", Cypher.literalFalse()))
			.build();

		assertThat(cypherRenderer.render(statement))
			.isEqualTo("RETURN {`key 1`: true, `key 2`: false}");
	}

	@Test // GH-121
	void aliasOnWrongPosition() {

		SymbolicName u = Cypher.name("u");
		SymbolicName rn = Cypher.name("rn");
		SymbolicName nn = Cypher.name("nn");

		Node rnNode = Cypher.node("SomeLabel").named(rn);
		var rnAliasedAsNN = rnNode.as("nn");
		Statement statement = Cypher.match(Cypher.node("User").named(u), rnNode, Cypher.node("SomeLabel").named(nn))
			.withDistinct(u, rn, rnAliasedAsNN)
			.returning(u, rn, rnAliasedAsNN)
			.build();

		assertThat(cypherRenderer.render(statement))
			.isEqualTo(
				"MATCH (u:`User`), (rn:`SomeLabel`), (nn:`SomeLabel`) WITH DISTINCT u, rn, rn AS nn RETURN u, rn, nn");
	}

	@Test // GH-123
	void propertiesOfFunctions() {

		Statement statement = Cypher.returning(Cypher.property(Cypher.datetime(), "epochSeconds")).build();
		assertThat(cypherRenderer.render(statement))
			.isEqualTo("RETURN datetime().epochSeconds");
	}

	@Test // GH-123
	void propertiesOfFunctionsInsideQuery() {

		var collectedThings = Cypher.collect(Cypher.name("n")).as("collectedThings");
		Statement statement = Cypher.match(Cypher.anyNode().named("n"))
			.with(collectedThings)
			.returning(Cypher.property(Cypher.last(collectedThings), "name")).build();
		assertThat(cypherRenderer.render(statement))
			.isEqualTo("MATCH (n) WITH collect(n) AS collectedThings RETURN last(collectedThings).name");
	}

	@Test
	void gh127() {

		SymbolicName key = Cypher.name("key");
		String dynamicPrefix = "properties.";
		Statement statement = Cypher
			.match(person)
			.returning(person.project(
				"json",
				Cypher.call("apoc.map.fromPairs")
					.withArgs(
						Cypher.listWith(key)
							.in(Cypher.call("keys").withArgs(person.getRequiredSymbolicName()).asFunction())
							.where(key.startsWith(Cypher.literalOf(dynamicPrefix)))
							.returning(
								Cypher.call("substring")
									.withArgs(key, Cypher.literalOf(dynamicPrefix.length())).asFunction(),
								person.property(key)
							)
					)
					.asFunction()
			)).build();

		assertThat(cypherRenderer.render(statement))
				.isEqualTo("MATCH (person:`Person`) "
						+ "RETURN person{"
						+ "json: apoc.map.fromPairs([key IN keys(person) WHERE key STARTS WITH 'properties.' | [substring(key, 11), "
						+ "person[key]]])"
						+ "}");

	}

	@Test // GH-133
	void allowSymbolicNamesAsCondition() {

		Node company = Cypher.node("Company").named("company");
		SymbolicName cond = Cypher.name("cond");
		StatementBuilder.OngoingReadingAndReturn cypher = Cypher
				.match(company)
				.where(Cypher.any(cond).in(Cypher
						.listBasedOn(company.relationshipTo(person, "WORKS_AT"))
						.returning(person.property("name").isEqualTo(Cypher.parameter("name"))))
						.where(cond.asCondition())
				)
				.returning(company);

		assertThat(cypherRenderer.render(cypher.build()))
				.isEqualTo("MATCH (company:`Company`) WHERE any(cond IN [(company)-[:`WORKS_AT`]->(person:`Person`) | person.name = $name] WHERE cond) RETURN company");
	}

	@Test // GH-131
	void projectSymbolicNames() {

		Node user = Cypher.node("User").named("user");
		Node userKnows = Cypher.node("User").named("userKnows");
		SymbolicName sortedElement = Cypher.name("sortedElement");

		PatternComprehension innerPatternComprehension = Cypher.listBasedOn(user.relationshipTo(userKnows, "KNOWS"))
			.returning(userKnows.project(
				"born",
				userKnows.property("born")
			));
		Statement statement = Cypher
			.match(user)
			.returning(
				user.project(
					"knows",
					Cypher.listWith(sortedElement)
						.in(innerPatternComprehension)
						.returning(sortedElement.project(
							"born",
							Cypher.mapOf(
								"formatted",
								Cypher.call("toString").withArgs(Cypher.property(sortedElement, "born")).asFunction()
							)
						))
				)).build();
		assertThat(cypherRenderer.render(statement))
			.isEqualTo(
				"MATCH (user:`User`) RETURN user{knows: [sortedElement IN [(user)-[:`KNOWS`]->(userKnows:`User`) | userKnows{born: userKnows.born}] | sortedElement{born: {formatted: toString(sortedElement.born)}}]}");
	}

	@Test // GH-128
	void relationshipPatternsAsCondition() {

		Statement statement = Cypher.match(person)
			.where(
				person.relationshipTo(Cypher.anyNode(), "A").asCondition().or(person.relationshipTo(Cypher.anyNode(), "B"))
			)
			.and(
				person.relationshipTo(Cypher.anyNode(), "C").asCondition()
					.or(
						person.relationshipTo(Cypher.anyNode(), "D").asCondition()
							.and(person.relationshipTo(Cypher.anyNode(), "E"))
					)
					.or(person.relationshipTo(Cypher.anyNode(), "F"))
			)
			.returning(person).build();

		String expected = (""
				+ "MATCH (person:`Person`) WHERE ("
				+ "  ("
				+ "      (person)-[:`A`]->() OR (person)-[:`B`]->()"
				+ "  ) AND ("
				+ "      ("
				+ "          (person)-[:`C`]->() OR ("
				+ "              (person)-[:`D`]->() AND (person)-[:`E`]->()"
				+ "          )"
				+ "      ) OR (person)-[:`F`]->())"
				+ ") RETURN person"
		).replaceAll("\\s{2,}", "");

		assertThat(cypherRenderer.render(statement)).isEqualTo(expected);
	}

	@Test // GH-142
	void pointShouldAcceptExpressionToo() {

		Parameter<?> location = Cypher.parameter("location");
		Property distance = Cypher.property(location, "distance");

		Expression point = Cypher.point(Cypher.property(location, "point"));

		Statement statement = Cypher
			.match(person)
			.where(Cypher.distance(person.property("location"), point).isEqualTo(distance))
			.returning(person).build();

		assertThat(cypherRenderer.render(statement))
			.isEqualTo("MATCH (person:`Person`) WHERE distance(person.location, point($location.point)) = $location.distance RETURN person");
	}

	@Test // GH-141
	void propertiesShouldBeExtractableFromExpressions() {

		Parameter<?> location = Cypher.parameter("location");

		Expression point = Cypher.call("point").withArgs(location.property("point")).asFunction();
		Property distance = Cypher.property(location, "distance");

		Statement statement = Cypher
			.match(person)
			.where(Cypher.distance(person.property("location"), point).isEqualTo(distance))
			.returning(person).build();

		assertThat(cypherRenderer.render(statement))
			.isEqualTo("MATCH (person:`Person`) WHERE distance(person.location, point($location.point)) = $location.distance RETURN person");
	}

	static Stream<Arguments> relpatternChainingArgs() {
		Stream.Builder<Arguments> arguments = Stream.builder();
		arguments.add(Arguments.of(true, 1, false,
			"MATCH (s)-[:`PART_OF`*0..1]->(:`Resume`)-[:`IS_PARALLEL`*0..1]->(:`Resume`)-[l:`LEADS_TO`]->(e) RETURN s, l, e"));
		arguments.add(Arguments.of(true, 2, false,
			"MATCH (s)-[:`PART_OF`*0..1]->(:`Resume`)-[:`IS_PARALLEL`*0..1]->(:`Resume`)-[l:`LEADS_TO`*2..2]->(e) RETURN s, l, e"));
		arguments.add(Arguments.of(true, 1, true,
			"MATCH (s)-[:`PART_OF`*0..1]->(:`Resume`)-[:`IS_PARALLEL`*0..1]->(:`Resume`)<-[l:`LEADS_TO`]-(e) RETURN s, l, e"));
		arguments.add(Arguments.of(true, 2, true,
			"MATCH (s)-[:`PART_OF`*0..1]->(:`Resume`)-[:`IS_PARALLEL`*0..1]->(:`Resume`)<-[l:`LEADS_TO`*2..2]-(e) RETURN s, l, e"));

		arguments.add(Arguments.of(false, 1, false, "MATCH (s)-[l:`LEADS_TO`]->(e) RETURN s, l, e"));
		arguments.add(Arguments.of(false, 2, false, "MATCH (s)-[l:`LEADS_TO`*2..2]->(e) RETURN s, l, e"));
		arguments.add(Arguments.of(false, 1, true, "MATCH (s)<-[l:`LEADS_TO`]-(e) RETURN s, l, e"));
		arguments.add(Arguments.of(false, 2, true, "MATCH (s)<-[l:`LEADS_TO`*2..2]-(e) RETURN s, l, e"));

		return arguments.build();
	}

	@Test
	void removeAllPropertiesShouldWork() {

		Node n = Cypher.node("DeleteMe").named("n");
		String cypher = Cypher.match(n)
			.set(n, Cypher.mapOf())
			.set(n.property("newProperty").to(Cypher.literalOf("aValue")))
			.returning(n)
			.build().getCypher();
		assertThat(cypher).isEqualTo("MATCH (n:`DeleteMe`) SET n = {} SET n.newProperty = 'aValue' RETURN n");
	}

	@Test // GH-168
	void containersMustBeMutatableByProperties() {

		Node nodeA = Cypher.node("Target").named("t");
		Node nodeB = Cypher.node("Source").named("s");
		String cypher = Cypher.match(nodeA, nodeB).mutate(nodeA, nodeB.property("whatever")).build().getCypher();
		assertThat(cypher).isEqualTo("MATCH (t:`Target`), (s:`Source`) SET t += s.whatever");
	}

	@ParameterizedTest // GH-152
	@MethodSource("relpatternChainingArgs")
	void relpatternChaining(boolean multihops, int length, boolean backward, String expected) {

		Node start = Cypher.anyNode("s");
		Node end = Cypher.anyNode("e");

		PatternElement result;
		if (multihops) {
			RelationshipChain leadsTo;
			leadsTo = start.relationshipTo(Cypher.node("Resume"), "PART_OF").length(0, 1)
				.relationshipTo(Cypher.node("Resume"), "IS_PARALLEL").length(0, 1);

			if (backward) {
				leadsTo = leadsTo.relationshipFrom(end, "LEADS_TO").named("l");
			} else {
				leadsTo = leadsTo.relationshipTo(end, "LEADS_TO").named("l");
			}
			if (length > 1) {
				leadsTo = leadsTo.length(length, length);
			}
			result = leadsTo;
		} else {
			Relationship leadsTo;

			if (backward) {
				leadsTo = start.relationshipFrom(end, "LEADS_TO").named("l");
			} else {
				leadsTo = start.relationshipTo(end, "LEADS_TO").named("l");
			}
			if (length > 1) {
				leadsTo = leadsTo.length(length, length);
			}
			result = leadsTo;
		}
		String cypher = Cypher.match(result).returning("s", "l", "e").build().getCypher();
		assertThat(cypher).isEqualTo(expected);
	}

	@Test // GH-187
	void returningRawShouldWork() {

		String userProvidedCypher = "MATCH (this)-[:LINK]-(o:Other) RETURN o";
		Node node = Cypher.node("Node").named("node");
		SymbolicName result = Cypher.name("result");
		Statement statement = Cypher
			.match(node)
			.call(Cypher
				.with(node)
				// https://neo4j.com/docs/cypher-manual/current/clauses/call-subquery/#subquery-correlated-importing
				// > Aliasing or expressions are not supported in importing WITH clauses - e.g. WITH a AS b or WITH a+1 AS b.
				.with(node.as("this"))
				.returningRaw(Cypher.raw(userProvidedCypher).as(result))
				.build()
			)
			.returning(result.project("foo", "bar"))
			.build();

		String cypher = Renderer.getRenderer(Configuration.prettyPrinting()).render(statement);
		assertThat(cypher).isEqualTo("""
			MATCH (node:Node)
			CALL {
			  WITH node
			  WITH node AS this
			  MATCH (this)-[:LINK]-(o:Other) RETURN o AS result
			}
			RETURN result {
			  .foo,
			  .bar
			}""");
	}

	@Test
	void veryRawCallShouldWork() {

		SymbolicName msg = Cypher.name("message");
		String statement = Cypher.unwind(Cypher.parameter("events")).as(msg)
			.with(
				msg.property("value").as("event"),
				msg.property("header").as("header"),
				msg.property("key").as("key"),
				msg.property("value").as("value")
			)
			.callRawCypher("WITH key, value CREATE (e:Event {key: key, value: value})")
			.build()
			.getCypher();

		assertThat(statement)
			.isEqualTo("UNWIND $events AS message WITH message.value AS event, message.header AS header, message.key AS key, message.value AS value CALL {WITH key, value CREATE (e:Event {key: key, value: value})}");
	}

	@Test // GH-190
	void mixedWithShouldMakeSense() {

		Node node = Cypher.node("Node").named("node");
		Expression someExpression = Cypher.literalFalse();

		Property ll = node.property("ll");
		Property l = node.property("l");

		AliasedExpression aCase = Cypher.caseExpression().when(ll.isNull())
			.then(l)
			.elseDefault(ll)
			.as("l");

		String cypher = Cypher.match(node).with(node, someExpression.as("f"), aCase)
			.returning(Cypher.asterisk()).build().getCypher();
		assertThat(cypher).isEqualTo(
			"MATCH (node:`Node`) WITH node, false AS f, CASE WHEN node.ll IS NULL THEN node.l ELSE node.ll END AS l RETURN *");
	}

	@Test // GH-190
	void withAliasOnTopLevel() {

		String cypher = Cypher.with(Cypher.literalFalse().as("f"))
			.returning(Cypher.asterisk()).build().getCypher();
		assertThat(cypher).isEqualTo("WITH false AS f RETURN *");
	}

	@Test // GH-190
	void mixedWithDistinctShouldMakeSense() {

		Node node = Cypher.node("Node").named("node");
		Expression someExpression = Cypher.literalFalse();

		Property ll = node.property("ll");
		Property l = node.property("l");

		AliasedExpression aCase = Cypher.caseExpression().when(ll.isNull())
			.then(l)
			.elseDefault(ll)
			.as("l");

		String cypher = Cypher.match(node).withDistinct(node, someExpression.as("f"), aCase)
			.returning(Cypher.asterisk()).build().getCypher();
		assertThat(cypher).isEqualTo(
			"MATCH (node:`Node`) WITH DISTINCT node, false AS f, CASE WHEN node.ll IS NULL THEN node.l ELSE node.ll END AS l RETURN *");
	}

	@Test // GH-190
	void heterogenAliasWithDistinctShouldMakeSense() {

		Node node = Cypher.node("Node").named("node");
		Expression someExpression = Cypher.literalFalse();

		String cypher = Cypher.match(node).withDistinct(someExpression.as("f"), Cypher.date().as("aDate"))
			.returning(Cypher.asterisk()).build().getCypher();
		assertThat(cypher).isEqualTo(
			"MATCH (node:`Node`) WITH DISTINCT false AS f, date() AS aDate RETURN *");
	}

	@Test // GH-190
	void symbolicNameAndAlias() {

		Node node = Cypher.node("Node").named("node");
		Expression someExpression = Cypher.literalFalse();

		String cypher = Cypher.match(node).with(Cypher.name("n"), someExpression.as("f"), Cypher.date().as("aDate"))
			.returning(Cypher.asterisk()).build().getCypher();
		assertThat(cypher).isEqualTo(
			"MATCH (node:`Node`) WITH n, false AS f, date() AS aDate RETURN *");
	}

	@Test // GH-192
	void patternExpressionsAreNotAllowedToIntroduceNewVariables() {

		Node bike = Cypher.node("Bike").named("b");
		Node owner = Cypher.node("Person").named("o");
		Relationship owns = owner.relationshipTo(bike, "OWNS").named("r");
		Property p = owns.property("x");

		Statement statement = Cypher.match(bike)
			.where(owns.asCondition())
			.with(bike)
			.match(owns)
			.returning(bike.property("f"), p)
			.build();
		assertThat(statement.getCypher()).isEqualTo("MATCH (b:`Bike`) WHERE (:`Person`)-[:`OWNS`]->(b) WITH b MATCH (o:`Person`)-[r:`OWNS`]->(b) RETURN b.f, r.x");

		statement = Cypher.match(owns)
			.where(owns.asCondition())
			.returning(owns)
			.build();
		assertThat(statement.getCypher()).isEqualTo("MATCH (o:`Person`)-[r:`OWNS`]->(b:`Bike`) WHERE (o)-[r]->(b) RETURN r");
	}

	@Test // GH-193
	void matchAfterYieldShouldWorkStandalone() {

		Node g = Cypher.node("Group").named("g");
		Node a = Cypher.node("Asset").named("a");
		Node d = Cypher.node("Device").named("d");
		SymbolicName node = Cypher.name("node");
		String cypher = Cypher.call("db.index.fulltext.queryNodes")
			.withArgs(Cypher.literalOf("livesearch"), Cypher.literalOf("*a*"))
			.yield(node)
			.match(g.relationshipTo(a, "GROUPS").relationshipFrom(Cypher.node("Deploy"), "ON")
				.relationshipFrom(d, "SCHEDULED"))
			.where(a.property("asset_id").isEqualTo(Cypher.property(node, "asset_id")))
			.withDistinct(Cypher.collect(d.project(d.property("sigfox_id"), a)).as("assetdata"))
			.returning("assetdata").build().getCypher();
		assertThat(cypher).isEqualTo(
			"CALL db.index.fulltext.queryNodes('livesearch', '*a*') "
			+ "YIELD node "
			+ "MATCH (g:`Group`)-[:`GROUPS`]->(a:`Asset`)<-[:`ON`]-(:`Deploy`)<-[:`SCHEDULED`]-(d:`Device`) "
			+ "WHERE a.asset_id = node.asset_id "
			+ "WITH DISTINCT collect(d{.sigfox_id, a}) AS assetdata RETURN assetdata");
	}

	@Test // GH-193
	void matchAfterYieldShouldWorkInQuery() {

		Node g = Cypher.node("Group").named("g");
		Node a = Cypher.node("Asset").named("a");
		Node d = Cypher.node("Device").named("d");
		SymbolicName node = Cypher.name("node");
		SymbolicName nameOfIndex = Cypher.name("nameOfIndex");
		String cypher = Cypher.with(Cypher.parameter("p").as(nameOfIndex))
				.call("db.index.fulltext.queryNodes")
				.withArgs(nameOfIndex, Cypher.literalOf("*a*"))
				.yield(node)
				.match(g.relationshipTo(a, "GROUPS").relationshipFrom(Cypher.node("Deploy"), "ON")
					.relationshipFrom(d, "SCHEDULED"))
				.where(a.property("asset_id").isEqualTo(Cypher.property(node, "asset_id")))
				.withDistinct(Cypher.collect(d.project(d.property("sigfox_id"), a)).as("assetdata"))
				.returning("assetdata").build().getCypher();
		assertThat(cypher).isEqualTo(
			"WITH $p AS nameOfIndex CALL db.index.fulltext.queryNodes(nameOfIndex, '*a*') "
			+ "YIELD node "
			+ "MATCH (g:`Group`)-[:`GROUPS`]->(a:`Asset`)<-[:`ON`]-(:`Deploy`)<-[:`SCHEDULED`]-(d:`Device`) "
			+ "WHERE a.asset_id = node.asset_id "
			+ "WITH DISTINCT collect(d{.sigfox_id, a}) AS assetdata RETURN assetdata");
	}

	@Test // GH-189
	void propertyForExpressionWithCollectionOfNames() {

		Node node = Cypher.node("Node").named("n");

		String cypher = Cypher.match(node).returning(Cypher.property(node.getRequiredSymbolicName(), Collections.singleton("name")))
		.build().getCypher();
		assertThat(cypher).isEqualTo("MATCH (n:`Node`) RETURN n.name");
	}

	@Test // GH-189
	void exposesWithBasedOnExpressionCollectionOnNext() {

		Node node = Cypher.node("Node").named("n");

		String cypher = Cypher.match(node)
				.with(Collections.singleton(node.getRequiredSymbolicName())) // DefaultStatementBuilder
				.with(Collections.singleton(node.getRequiredSymbolicName())) // DefaultStatementWithWithBuilder
				.call("my.procedure")
				.yield("x")
				.with(Collections.singleton(node.getRequiredSymbolicName())) // InQueryCallBuilder
				.returning(node).build().getCypher();

		assertThat(cypher).isEqualTo("MATCH (n:`Node`) WITH n WITH n CALL my.procedure() YIELD x WITH n RETURN n");
	}

	@Test // GH-189
	void exposesWithDistinctBasedOnExpressionCollectionOnNext() {

		Node node = Cypher.node("Node").named("n");

		String cypher = Cypher.match(node)
				.withDistinct(Collections.singleton(node.getRequiredSymbolicName())) // DefaultStatementBuilder
				.withDistinct(Collections.singleton(node.getRequiredSymbolicName())) // DefaultStatementWithWithBuilder
				.call("my.procedure")
				.yield("x")
				.withDistinct(Collections.singleton(node.getRequiredSymbolicName())) // InQueryCallBuilder
				.returning(node).build().getCypher();

		assertThat(cypher).isEqualTo(
				"MATCH (n:`Node`) WITH DISTINCT n WITH DISTINCT n CALL my.procedure() YIELD x WITH DISTINCT n RETURN n"
		);
	}

	@Test // GH-197
	void symbolicNamesInNotConditionsMustNotBeResolvedWhenConditionIsARelationshipPatternV1() {

		Node node = Cypher.node("Division").named("node");
		Statement q = Cypher.match(node)
			.withDistinct(node)
			.where(Cypher
				.not(Cypher.anyNode(node.getRequiredSymbolicName()).relationshipTo(Cypher.node("Department"), "IN")
					.relationshipTo(Cypher.node("Department"), "INSIDE")
					.properties("rel_property", Cypher.literalTrue())
					.relationshipTo(Cypher.node("Employee"), "EMPLOYS")
				)).returning(Cypher.asterisk()).build();

		assertThat(Renderer.getRenderer(Configuration.newConfig().build()).render(q)).isEqualTo(
			"MATCH (node:`Division`) WITH DISTINCT node WHERE NOT (node)-[:`IN`]->(:`Department`)-[:`INSIDE` {rel_property: true}]->(:`Department`)-[:`EMPLOYS`]->(:`Employee`) RETURN *");

		assertThat(Renderer.getRenderer(Configuration.newConfig().alwaysEscapeNames(false).build()).render(q)).isEqualTo(
			"MATCH (node:Division) WITH DISTINCT node WHERE NOT (node)-[:IN]->(:Department)-[:INSIDE {rel_property: true}]->(:Department)-[:EMPLOYS]->(:Employee) RETURN *");

		assertThat(Renderer.getRenderer(Configuration.prettyPrinting()).render(q))
			.isEqualTo("""
				MATCH (node:Division)
				WITH DISTINCT node
				WHERE NOT (node)-[:IN]->(:Department)-[:INSIDE {
				  rel_property: true
				}]->(:Department)-[:EMPLOYS]->(:Employee)
				RETURN *"""
			);
	}

	@Test // GH-197
	void symbolicNamesInNotConditionsMustNotBeResolvedWhenConditionIsARelationshipPatternV2() {

		Node node = Cypher.node("Division").named("node");
		Statement q = Cypher.match(node)
			.withDistinct(node)
			.where(Cypher
				.not(node.relationshipTo(Cypher.node("Department"), "IN")
					.relationshipTo(Cypher.node("Department"), "INSIDE")
					.properties("rel_property", Cypher.literalTrue())
					.relationshipTo(Cypher.node("Employee"), "EMPLOYS")
				)).returning(Cypher.asterisk()).build();

		assertThat(Renderer.getRenderer(Configuration.newConfig().build()).render(q)).isEqualTo(
			"MATCH (node:`Division`) WITH DISTINCT node WHERE NOT (node)-[:`IN`]->(:`Department`)-[:`INSIDE` {rel_property: true}]->(:`Department`)-[:`EMPLOYS`]->(:`Employee`) RETURN *");

		assertThat(Renderer.getRenderer(Configuration.newConfig().alwaysEscapeNames(false).build()).render(q)).isEqualTo(
			"MATCH (node:Division) WITH DISTINCT node WHERE NOT (node)-[:IN]->(:Department)-[:INSIDE {rel_property: true}]->(:Department)-[:EMPLOYS]->(:Employee) RETURN *");

		assertThat(Renderer.getRenderer(Configuration.prettyPrinting()).render(q))
			.isEqualTo("""
				MATCH (node:Division)
				WITH DISTINCT node
				WHERE NOT (node)-[:IN]->(:Department)-[:INSIDE {
				  rel_property: true
				}]->(:Department)-[:EMPLOYS]->(:Employee)
				RETURN *"""
			);
	}

	@Test // GH-197
	void symbolicNamesMustBeClearedAfterUnion() {

		Node actor = Cypher.node("Actor").named("n");
		Node movie = Cypher.node("Movie").named("n");
		Statement stmt = Cypher.unionAll(
			Cypher.match(actor).returning(actor.property("name").as("name")).build(),
			Cypher.match(movie).returning(movie.property("title").as("name")).build()
		);
		assertThat(stmt.getCypher()).isEqualTo("MATCH (n:`Actor`) RETURN n.name AS name UNION ALL MATCH (n:`Movie`) RETURN n.title AS name");
	}

	@Test // GH-203
	void patternComprehensionMustBeScoped() {
		Node testNode = Cypher.node("Department").named("d");
		Node user = Cypher.node("User").named("u");
		Statement query = Cypher.match(testNode).returning(testNode.project(
			Cypher.asterisk(),
			"firstname",
			Cypher.listBasedOn(testNode.relationshipTo(user)).returning(user.property("firstname")),
			"lastname",
			Cypher.listBasedOn(testNode.relationshipTo(user)).returning(user.property("lastname"))
		)).build();

		assertThat(query.getCypher()).isEqualTo(
			"MATCH (d:`Department`) RETURN d{.*, firstname: [(d)-->(u:`User`) | u.firstname], lastname: [(d)-->(u:`User`) | u.lastname]}");
	}

	@Test // GH-319
	void communitySite20220304() {

		SymbolicName nodes = Cypher.name("nodes");
		SymbolicName relations = Cypher.name("relations");
		SymbolicName second_nodes = Cypher.name("second_nodes");
		SymbolicName second_relations = Cypher.name("second_relations");

		NamedPath first_path = Cypher.path("p")
			.definedBy(
				Cypher.node("lookingType").relationshipFrom(Cypher.anyNode(), "specifiedRelation")
			);
		NamedPath second_path = Cypher.path("second_p")
			.definedBy(
				Cypher.anyNode("n")
					.relationshipTo(Cypher.anyNode().named(second_nodes), "otherRelation")
					.named(second_relations)
			);

		Statement inner = Cypher.unwind(nodes).as("n").with("n")
				.match(second_path)
				.returning(second_nodes, second_relations)
				.build();

		Statement completeStatement =
			Cypher
				.match(first_path)
				.with(Cypher.nodes(first_path).as(nodes), Cypher.relationships(first_path).as(relations))
				.call(inner, nodes)
				.returning(nodes, relations, Cypher.collect(second_nodes), Cypher.collect(second_relations))
			.build();

		Renderer renderer = Renderer.getRenderer(Configuration.prettyPrinting());
		String cypher = renderer.render(completeStatement);
		String expected = """
			MATCH p = (:lookingType)<-[:specifiedRelation]-()
			WITH nodes(p) AS nodes, relationships(p) AS relations
			CALL {
			  WITH nodes
			  UNWIND nodes AS n
			  WITH n
			  MATCH second_p = (n)-[second_relations:otherRelation]->(second_nodes)
			  RETURN second_nodes, second_relations
			}
			RETURN nodes, relations, collect(second_nodes), collect(second_relations)""";
		assertThat(cypher).isEqualTo(expected);
	}

	@ParameterizedTest // GH-319
	@EnumSource(Dialect.class)
	void subqueryWithRename(Dialect dialect) {

		SymbolicName nodes = Cypher.name("nodes");
		SymbolicName relations = Cypher.name("relations");

		NamedPath first_path = Cypher.path("p")
			.definedBy(
				Cypher.node("Target").relationshipFrom(Cypher.anyNode(), "REL")
			);

		Statement completeStatement =
			Cypher
				.match(first_path)
				.with(Cypher.nodes(first_path).as(nodes), Cypher.relationships(first_path).as(relations))
				.call(Cypher.returning(Cypher.name("x")).build(), nodes.as("x"))
				.returning(Cypher.asterisk())
				.build();

		Renderer renderer = Renderer.getRenderer(Configuration.newConfig().withDialect(dialect).withPrettyPrint(true).build());
		String cypher = renderer.render(completeStatement);
		String expected = """
			MATCH p = (:Target)<-[:REL]-()
			WITH nodes(p) AS nodes, relationships(p) AS relations
			CALL {
			  WITH nodes
			  WITH nodes AS x
			  RETURN x
			}
			RETURN *""";
		if (dialect == Dialect.NEO4J_5_23) {
			expected = expected
				.replace("CALL {", "CALL (nodes) {")
				.replace("  WITH nodes\n", "");
		}
		assertThat(cypher).isEqualTo(expected);
	}

	@Test // GH-319
	void subqueryWithoutImport() {

		SymbolicName nodes = Cypher.name("nodes");
		SymbolicName relations = Cypher.name("relations");

		NamedPath first_path = Cypher.path("p")
			.definedBy(
				Cypher.node("Target").relationshipFrom(Cypher.anyNode(), "REL")
			);

		Statement completeStatement =
			Cypher
				.match(first_path)
				.with(Cypher.nodes(first_path).as(nodes), Cypher.relationships(first_path).as(relations))
				.call(Cypher.returning(Cypher.literalOf(1)).build())
				.returning(Cypher.literalTrue())
				.build();

		Renderer renderer = Renderer.getRenderer(Configuration.prettyPrinting());
		String cypher = renderer.render(completeStatement);
		String expected = """
			MATCH p = (:Target)<-[:REL]-()
			WITH nodes(p) AS nodes, relationships(p) AS relations
			CALL {
			  RETURN 1
			}
			RETURN true""";
		assertThat(cypher).isEqualTo(expected);
	}

	@Test // GH-349
	void allowProcedureCallWithoutResult() {
		Node n = Cypher.anyNode("n");
		ResultStatement statement = Cypher.match(n)
			.call("apoc.util.validate")
				.withArgs(Cypher.exists(n.property("foo")), Cypher.literalOf("Error"), Cypher.listOf())
			.withoutResults()
			.returning(n)
			.build();
		assertThat(statement.getCypher()).isEqualTo("MATCH (n) CALL apoc.util.validate(exists(n.foo), 'Error', []) RETURN n");
	}

	@Test // GH-349
	void allowProcedureCallWithoutResultAndArguments() {
		Node n = Cypher.anyNode("n");
		ResultStatement statement = Cypher.match(n)
			.call("apoc.util.validate")
			.withoutResults()
			.returning(n)
			.build();
		assertThat(statement.getCypher()).isEqualTo("MATCH (n) CALL apoc.util.validate() RETURN n");
	}

	@Test // GH-349
	void wildValidate() {

		Statement statement = createSomewhatComplexStatement();
		Renderer renderer = Renderer.getRenderer(Configuration.newConfig().alwaysEscapeNames(false).build());
		assertThat(renderer.render(statement))
			.isEqualTo(""
				+ "CALL {"
				+ "CREATE (this0:Movie)"
				+ " SET this0.title = $this0_title"
				+ " WITH this0"
				+ " CALL {"
				+ "WITH this0"
				+ " CALL apoc.util.validate(NOT (any(r IN ['admin'] WHERE any(rr IN $auth.roles WHERE r = rr))), '@neo4j/graphql/FORBIDDEN', [0])"
				+ " MERGE (this0_genres_connectOrCreate0:Genre {name: $this0_genres_connectOrCreate0_node_name})"
				+ " ON CREATE"
				+ " SET this0_genres_connectOrCreate0.name = $this0_genres_connectOrCreate0_on_create_name"
				+ " MERGE (this0)-[this0_relationship_this0_genres_connectOrCreate0:IN_GENRE]->(this0_genres_connectOrCreate0)"
				+ " RETURN count(*)"
				+ "}"
				+ " RETURN this0"
				+ "}"
				+ " RETURN [this0{.title}] AS data");
	}

	public static Statement createSomewhatComplexStatement() {
		Node this0 = Cypher.node("Movie").named("this0");

		Condition validationCondition = Cypher.any("r")
			.in(Cypher.listOf(Cypher.literalOf("admin")))
			.where(Cypher.any("rr").in(Cypher.parameter("$auth.roles")).where(SymbolicName.of("r").eq(SymbolicName.of("rr"))));

		Node g = Cypher.node("Genre").named("this0_genres_connectOrCreate0")
			.withProperties(Cypher.mapOf("name", Cypher.parameter("this0_genres_connectOrCreate0_node_name")));

		Statement innerSubquery = Cypher
			.call("apoc.util.validate")
			.withArgs(validationCondition.not(), Cypher.literalOf("@neo4j/graphql/FORBIDDEN"), Cypher.listOf(Cypher.literalOf(0)))
			.withoutResults()
			.merge(g)
			.onCreate().set(g.property("name").to(Cypher.parameter("this0_genres_connectOrCreate0_on_create_name")))
			.merge(this0.relationshipTo(g, "IN_GENRE").named("this0_relationship_this0_genres_connectOrCreate0"))
			.returning(Cypher.count(Cypher.asterisk()))
			.build();

		Statement subquery = Cypher.create(this0)
			.set(this0.property("title").to(Cypher.parameter("this0_title")))
			.with(this0)
			.call(innerSubquery, this0)
			.returning(this0)
			.build();

		return Cypher
			.call(subquery)
			.returning(Cypher.listOf(Cypher.createProjection(this0.getRequiredSymbolicName(), "title")).as("data"))
			.build();
	}

	@Test // GH-362
	void nodeNeedsToBeRenderedTwiceWithPatternsInCondition() {

		Node n = Cypher.node("Node").named("n");
		Node p = Cypher.anyNode("p");
		SymbolicName x = Cypher.name("x");
		Condition cond1 = Cypher.none(p.getRequiredSymbolicName()).in(x)
			.where(
				p.relationshipTo(Cypher.anyNode(), "Y").asCondition()
					.or(p.relationshipTo(Cypher.anyNode(), "Z").asCondition())
			);
		Condition cond2 = Cypher.any(p.getRequiredSymbolicName()).in(x)
			.where(p.property("bar").eq(Cypher.literalTrue()));
		Statement s = Cypher.match(n)
			.with(Cypher.collect(n).as(x))
			.where(cond1.and(cond2))
			.returning(Cypher.count(n))
			.build();
		assertThat(s.getCypher()).isEqualTo("MATCH (n:`Node`) WITH collect(n) AS x WHERE (none(p IN x WHERE ((p)-[:`Y`]->() OR (p)-[:`Z`]->())) AND any(p IN x WHERE p.bar = true)) RETURN count(n)");
	}

	@Test // GH-350
	void compoundConditionShouldBeImmutableAsStatedInTheDocs() {
		Node n = Cypher.anyNode("n");
		Condition a = org.neo4j.cypherdsl.core.Cypher.noCondition();
		a = a.and(n.property("b").eq(Cypher.literalOf(1)));
		Condition temp = a;
		a = a.and(n.property("c").eq(Cypher.literalOf(2)));
		ResultStatement statement = Cypher
			.match(n)
			.where(temp)
			.returning(n)
			.build();
		assertThat(statement.getCypher()).isEqualTo("MATCH (n) WHERE n.b = 1 RETURN n");
	}

	@Test // GH-389
	void shouldRenderCorrectDateTimeCall() {

		Expression datetime = Cypher
			.datetime(toMap(ZonedDateTime.parse("2022-06-19T15:47:38.590917308Z[UTC]")));
		assertThat(Cypher.returning(datetime).build().getCypher()).isEqualTo(
			"RETURN datetime({year: 2022, month: 6, day: 19, hour: 15, minute: 47, second: 38, nanosecond: 590917308, timezone: 'UTC'})");
	}

	static MapExpression toMap(ZonedDateTime value) {
		return Cypher.mapOf(
			"year", Cypher.literalOf(value.getYear()),
			"month", Cypher.literalOf(value.getMonthValue()),
			"day", Cypher.literalOf(value.getDayOfMonth()),
			"hour", Cypher.literalOf(value.getHour()),
			"minute", Cypher.literalOf(value.getMinute()),
			"second", Cypher.literalOf(value.getSecond()),
			"nanosecond", Cypher.literalOf(value.getNano()),
			"timezone", Cypher.literalOf(value.getZone().toString())
		);
	}

	@Test // GH-388
	void shouldRenderSetOpOnNodeWithMap() {
		Node node = Cypher.node("CordraObject").named("existingNode")
			.withProperties("_id", Cypher.literalOf("test/55de0539eb1e14f26a04"));

		Statement statement = Cypher.merge(node)
			.set(node, Cypher.mapOf(
				"_id", Cypher.literalOf("test/55de0539eb1e14f26a04"),
				"_type", Cypher.literalOf("Movie"),
				"title", Cypher.literalOf("Top Gun"),
				"released", Cypher.literalOf(1986)
			))
			.returning(node)
			.build();

		assertThat(Renderer.getRenderer(Configuration.prettyPrinting()).render(statement))
			.isEqualTo("""
				MERGE (existingNode:CordraObject {
				  _id: 'test/55de0539eb1e14f26a04'
				})
				SET existingNode = {
				  _id: 'test/55de0539eb1e14f26a04',
				  _type: 'Movie',
				  title: 'Top Gun',
				  released: 1986
				}
				RETURN existingNode""");
	}

	@Test // GH-388
	void shouldProvideSetOperations() {
		Node node = Cypher.node("CordraObject").named("existingNode")
			.withProperties("_id", Cypher.literalOf("test/55de0539eb1e14f26a04"));

		Operation thisChangesEverything = node.set(Cypher.mapOf(
				"_id", Cypher.literalOf("test/55de0539eb1e14f26a04"),
				"_type", Cypher.literalOf("Movie"),
				"title", Cypher.literalOf("Top Gun"),
				"released", Cypher.literalOf(1986)
			));

		Statement statement = Cypher.merge(node)
			.set(thisChangesEverything)
			.returning(node)
			.build();

		assertThat(Renderer.getRenderer(Configuration.prettyPrinting()).render(statement))
			.isEqualTo("""
				MERGE (existingNode:CordraObject {
				  _id: 'test/55de0539eb1e14f26a04'
				})
				SET existingNode = {
				  _id: 'test/55de0539eb1e14f26a04',
				  _type: 'Movie',
				  title: 'Top Gun',
				  released: 1986
				}
				RETURN existingNode"""
			);
	}

	@Test // GH-388
	void shouldProvideSetOperationsForParameter() {
		Node node = Cypher.node("CordraObject").named("existingNode")
			.withProperties("_id", Cypher.literalOf("test/55de0539eb1e14f26a04"));

		Operation thisChangesEverything = node.set(Cypher.parameter("aNewMap"));

		Statement statement = Cypher.merge(node)
			.set(thisChangesEverything)
			.returning(node)
			.build();

		assertThat(Renderer.getRenderer(Configuration.prettyPrinting()).render(statement))
			.isEqualTo("""
				MERGE (existingNode:CordraObject {
				  _id: 'test/55de0539eb1e14f26a04'
				})
				SET existingNode = $aNewMap
				RETURN existingNode"""
			);
	}

	@Test // GH-419
	void aliasedElementsShouldBeCarriedForwardWithWithToo() {

		Node objectInstanceNode = Cypher.node("ObjectInstance").named("oi");
		Node attributeTypeNode = Cypher.node("AttributeType").named("att");
		Node attributeNode = Cypher.node("Attribute").named("at");
		SymbolicName attributeTypeAndValue = Cypher.name("attributeTypeAndValue");
		SymbolicName collection = Cypher.name("collection");

		SymbolicName oi = objectInstanceNode.getRequiredSymbolicName();
		Statement statement = Cypher.match(objectInstanceNode)
			.with(Cypher.mapOf(oi.getValue(), oi).as(collection))
			.unwind(Cypher.parameter("attributes")).as(attributeTypeAndValue)
			.with(attributeTypeAndValue, collection)
			.match(attributeTypeNode.withProperties("name", attributeTypeAndValue.property("name")).relationshipFrom(attributeNode, "OF_TYPE"))
			.with(attributeNode, collection.property("oi").as(oi))
			.match(objectInstanceNode.relationshipTo(attributeNode, "IS_IDENTIFIED_BY"))
			.returning(attributeNode)
			.build();

		String cypher = Renderer.getRenderer(
			Configuration.newConfig().withPrettyPrint(true).alwaysEscapeNames(true).build()).render(statement);
		assertThat(cypher)
			.isEqualTo("""
				MATCH (oi:`ObjectInstance`)
				WITH {
				  oi: oi
				} AS collection
				UNWIND $attributes AS attributeTypeAndValue
				WITH attributeTypeAndValue, collection
				MATCH (att:`AttributeType` {
				  name: attributeTypeAndValue.name
				})<-[:`OF_TYPE`]-(at:`Attribute`)
				WITH at, collection.oi AS oi
				MATCH (oi)-[:`IS_IDENTIFIED_BY`]->(at)
				RETURN at"""
			);
	}

	@Test
	void bbBoxManual() {

		Node location = Cypher.node("Location").named("loc");
		Expression sw = Cypher.point(Cypher.mapOf("longitude", Cypher.literalOf(2.592773), "latitude", Cypher.literalOf(46.346928)));
		Expression ne = Cypher.point(Cypher.mapOf("longitude", Cypher.literalOf(18.654785), "latitude", Cypher.literalOf(55.714735)));
		Expression withinBBox = Cypher.call("point.withinBBox")
			.withArgs(location.property("coordinates"), sw, ne).asFunction();

		Condition conditions = Cypher.noCondition();
		conditions = conditions.and(withinBBox.asCondition());

		String stmt = Cypher.match(location).where(conditions).returning(location).build().getCypher();
		assertThat(stmt).isEqualTo("MATCH (loc:`Location`) WHERE point.withinBBox(loc.coordinates, point({longitude: 2.592773, latitude: 46.346928}), point({longitude: 18.654785, latitude: 55.714735})) RETURN loc");
	}

	@Test
	void bbBox() {

		Node location = Cypher.node("Location").named("loc");
		Expression sw = Cypher.coordinate(2.592773, 46.346928);
		Expression ne = Cypher.coordinate(18.654785, 55.714735);
		Expression withinBBox = Cypher.withinBBox(location.property("coordinates"), sw, ne);

		Condition conditions = Cypher.noCondition();
		conditions = conditions.and(withinBBox.asCondition());

		String stmt = Cypher.match(location).where(conditions).returning(location).build().getCypher();
		assertThat(stmt).isEqualTo("MATCH (loc:`Location`) WHERE point.withinBBox(loc.coordinates, point({longitude: 2.592773, latitude: 46.346928}), point({longitude: 18.654785, latitude: 55.714735})) RETURN loc");
	}

	@Test // GH-490
	void shouldAllowAsteriskInYield() {
		Statement yield = Cypher.call("db.info").yield(Cypher.asterisk()).build();
		assertThat(Renderer.getDefaultRenderer().render(yield)).isEqualTo("CALL db.info() YIELD *");
	}

	@Test // GH-544
	void shouldAllowAsteriskInYieldAndArgs() {
		Statement yield = Cypher.call("dbms.listConfig").withArgs(Cypher.literalOf("port")).yield(Cypher.asterisk()).build();
		assertThat(Renderer.getDefaultRenderer().render(yield)).isEqualTo("CALL dbms.listConfig('port') YIELD *");
	}

	@Test // GH-505
	void testLabelRemoval() {
		Node node = Cypher.node("Wine").named("n");

		Operation removeOp = Cypher.removeLabel(node, "Drink");
		List<Expression> propertyExpressions = Collections.singletonList(removeOp);
		@SuppressWarnings("deprecation") StatementBuilder.OngoingReadingWithWhere ongoingReadingWithWhere = Cypher.match(node)
			.where(Functions.id(node).isEqualTo(Cypher.literalOf(1)));

		String expectedMessage = "REMOVE operations are not supported in a SET clause";
		assertThatIllegalArgumentException()
			.isThrownBy(() -> ongoingReadingWithWhere.set(propertyExpressions))
			.withMessage(expectedMessage);

		assertThatIllegalArgumentException()
			.isThrownBy(() -> ongoingReadingWithWhere.set(removeOp))
			.withMessage(expectedMessage);

		assertThatIllegalArgumentException()
			.isThrownBy(() -> Cypher.match(node).set(removeOp))
			.withMessage(expectedMessage);

		@SuppressWarnings("deprecation") String correctQuery = ongoingReadingWithWhere
			.remove(node, "Drink")
			.returning(Functions.id(node).as("id"))
			.build()
			.getCypher();

		assertThat(correctQuery).isEqualTo("MATCH (n:`Wine`) WHERE id(n) = 1 REMOVE n:`Drink` RETURN id(n) AS id");
	}

	@Test // GH-524
	void unwindWithoutWith() {

		SymbolicName id = Cypher.name("id");
		Node n = Cypher.node("Person").named("n");
		Renderer renderer = Renderer.getRenderer(Configuration.newConfig().withDialect(Dialect.NEO4J_5).build());
		Statement statement = Cypher.unwind(Cypher.parameter("ids"))
			.as(id)
			.match(n)
			.where(n.elementId().isEqualTo(id))
			.returning(n)
			.build();
		assertThat(renderer.render(statement)).isEqualTo("UNWIND $ids AS id MATCH (n:`Person`) WHERE elementId(n) = id RETURN n");
	}

	@Test // GH-547
	void mixedBagOfWith() {

		var cypher = Cypher.match(person)
			.with(person, Cypher.count(person.relationshipTo(Cypher.anyNode(), "ACTED_IN")).as("actedInDegree"))
			.returning(Cypher.asterisk())
			.build().getCypher();
		assertThat(cypher).isEqualTo("MATCH (person:`Person`) WITH person, COUNT { (person)-[:`ACTED_IN`]->() } AS actedInDegree RETURN *");
	}

	@Test // GH-553
	void allowCovariantForMakingDynamicCypherCreationEasier() {

		var patterns = List.of(Cypher.node("A").named("a"), Cypher.node("B").relationshipTo(Cypher.node("C"), "IS_RELATED").named("r"));
		var cypher = Cypher.match(patterns.stream().toList()).returning(Cypher.asterisk()).build().getCypher();
		assertThat(cypher).isEqualTo("MATCH (a:`A`), (:`B`)-[r:`IS_RELATED`]->(:`C`) RETURN *");
	}

	@Test // GH-585
	void rerenderShouldYieldSameResultOnSameRenderer() throws NoSuchFieldException, IllegalAccessException {

		var anonymous = Cypher.node("Person");
		var statement = Cypher.match(anonymous).delete(anonymous).build();
		var s1 = statement.getCypher();
		var s2 = statement.getCypher();
		var defaultRenderer = Renderer.getDefaultRenderer();
		var s3 = defaultRenderer.render(statement);
		assertThat(s1).isEqualTo(s2);
		assertThat(s2).isEqualTo(s3);

		// Nuke the cache
		var renderedStatementCache = defaultRenderer.getClass().getDeclaredField("renderedStatementCache");
		renderedStatementCache.setAccessible(true);
		((Map<?, ?>) renderedStatementCache.get(defaultRenderer)).clear();
		var s4 = defaultRenderer.render(statement);
		assertThat(s3).isEqualTo(s4);
	}

	@Test // GH-589
	void unionMustNotDestroyScope() {

		var actor = Cypher.node("Actor").named("a");
		var movie = Cypher.node("Movie").named("m");
		var serie = Cypher.node("Serie").named("s");
		var x = Cypher.name("x");
		Statement statement = Cypher
			.match(actor)
			.call(
				Cypher.union(
					Cypher.with(actor)
						.match(actor.relationshipTo(movie).named("ACTED_IN"))
						.returning(movie.as(x.getValue()))
						.build(),
					Cypher.with(actor)
						.match(actor.relationshipTo(serie).named("ACTED_IN"))
						.returning(serie.as(x.getValue()))
						.build()
				)
			).returning(x)
			.build();

		var expected = """
			MATCH (a:Actor)
			CALL {
			  WITH a
			  MATCH (a)-[ACTED_IN]->(m:Movie)
			  RETURN m AS x UNION
			  WITH a
			  MATCH (a)-[ACTED_IN]->(s:Serie)
			  RETURN s AS x
			}
			RETURN x""";
		var cypher = Renderer.getRenderer(Configuration.prettyPrinting())
				.render(statement);
		assertThat(cypher).isEqualTo(expected);
	}

	@Test // GH-595
	void callWithMostImportIntoScope() {
		SymbolicName var = Cypher.name("var");
		Node movie = Cypher.node("Movie").named("m");
		Node actor = Cypher.node("Actor").named("a");
		SymbolicName actors = Cypher.name("actors");
		Statement statement =
			Cypher.unwind(Cypher.parameter("x")).as(var)
				.call(
					Cypher.with(var)
						.create(movie)
						.returning(movie)
						.build()
				)
				.call(
					Cypher.with(movie)
						.match(movie.relationshipFrom(actor, "ACTED_IN"))
						.returning(Cypher.collect(actor).as(actors))
						.build()
				)
				.returning(movie.project(
					movie.property("title"),
					"actors", actors
				))
				.build();
		var expected = """
			UNWIND $x AS var
			CALL {
			  WITH var
			  CREATE (m:Movie)
			  RETURN m
			}
			CALL {
			  WITH m
			  MATCH (m)<-[:ACTED_IN]-(a:Actor)
			  RETURN collect(a) AS actors
			}
			RETURN m {
			  .title,
			  actors: actors
			}""";
		var cypher = Renderer.getRenderer(Configuration.prettyPrinting()).render(statement);
		assertThat(cypher).isEqualTo(expected);
	}

	static Stream<Arguments> conditionExpressionShouldWorkInReturn() {
		return Stream.of(
			Arguments.of(Cypher.returning(Cypher.literalOf(1), Cypher.literalTrue().asCondition()).build(), "RETURN 1, true"),
			Arguments.of(Cypher.returning(Cypher.literalOf(1), Cypher.literalTrue().asCondition().and(Cypher.literalFalse().asCondition())).build(), "RETURN 1, (true AND false)"),
			Arguments.of(Cypher.returning(Cypher.literalOf(1), Cypher.literalTrue().asCondition().or(Cypher.literalFalse().asCondition())).build(), "RETURN 1, (true OR false)")
		);
	}

	@ParameterizedTest(name = "{1}") // GH-605
	@MethodSource
	void conditionExpressionShouldWorkInReturn(Statement statement, String expected) {
		String cypher = statement.getCypher();
		assertThat(cypher).isEqualTo(expected);
	}

	@Test
	void manipulatingListExpressionsShouldBePossible() {
		var n = Cypher.node("Person").named("n");
		var x = Cypher.name("x");
		var rolesToAdd = Cypher.parameter("rolesToAdd");
		var rolesToRemove = Cypher.parameter("rolesToRemove");
		var cypher = Cypher.match(n)
			.returning(
				Cypher.listWith(x)
					.in(n.property("roles"))
					.where(x.in(rolesToAdd).and(Cypher.not(x.in(rolesToRemove))))
					.returning().add(rolesToAdd)
			)
			.build()
			.getCypher();
		assertThat(cypher).isEqualTo("MATCH (n:`Person`) RETURN ([x IN n.roles WHERE (x IN $rolesToAdd AND NOT (x IN $rolesToRemove))] + $rolesToAdd)");
	}

	@Test // GH-630
	void typesOfShouldWork() {

		var p = Cypher.path("p").definedBy(Cypher.node("Movie").relationshipFrom(Cypher.anyNode()).named("r"));
		var x = Cypher.name("x");
		var relationTypeCast = Cypher.listWith(x)
			.in(Cypher.relationships(p))
			.returning(Cypher.type(x));

		assertThat(Cypher.match(p).returning(relationTypeCast).build().getCypher())
			.isEqualTo("MATCH p = (:`Movie`)<-[r]-() RETURN [x IN relationships(p) | type(x)]");

		// Current workaround
		var relationTypeCast2 = Cypher.listWith(x)
			.in(Cypher.relationships(p))
			.returning(Cypher.call("type").withArgs(x).asFunction());
		assertThat(Cypher.match(p).returning(relationTypeCast2).build().getCypher())
			.isEqualTo("MATCH p = (:`Movie`)<-[r]-() RETURN [x IN relationships(p) | type(x)]");
	}

	@Test // GH-630
	void labelsOfShouldWork() {

		var p = Cypher.path("p").definedBy(Cypher.node("Movie").relationshipFrom(Cypher.anyNode()).named("r"));

		var nodes = Cypher.name("nodes");
		var node = Cypher.name("node");
		var labels = Cypher.name("labels");
		var label = Cypher.name("label");

		// The worst query ever to retrieve those labels. Don't do this at home.
		assertThat(Cypher.match(p)
			.with(Cypher.nodes(p).as(nodes))
			.unwind(nodes).as(node)
			.with(Cypher.labels(node).as(labels))
			.unwind(labels).as(label)
			.returningDistinct(label).build().getCypher())
			.isEqualTo("MATCH p = (:`Movie`)<-[r]-() WITH nodes(p) AS nodes UNWIND nodes AS node WITH labels(node) AS labels UNWIND labels AS label RETURN DISTINCT label");
	}

	@Test // GH-634
	void withAllShouldWork() {

		var this0 = Cypher.node("User").named("this");
		var x = Cypher.name("x");
		var stmt = Cypher.match(this0)
			.call(Cypher.with(Cypher.asterisk()).with(this0.as("x")).returning(Cypher.count(Cypher.asterisk()).as(x)).build())
			.returning(x, Cypher.count(Cypher.asterisk()))
			.build();

		var expected = "MATCH (this:`User`) CALL {WITH * WITH this AS x RETURN count(*) AS x} RETURN x, count(*)";
		assertThat(stmt.getCypher()).isEqualTo(expected);
	}

	@Test // GH-634
	void withAllAndThenSome() {

		var stmt = Cypher.match(Cypher.anyNode("n"))
			.with(Cypher.asterisk(), Cypher.count(Cypher.asterisk()).as("count"))
			.returning(Cypher.asterisk())
			.build();

		var expected = "MATCH (n) WITH *, count(*) AS count RETURN *";
		assertThat(stmt.getCypher()).isEqualTo(expected);
	}

	@Test // GH-642
	void mapLiteralShouldRenderProper() {

		var l = Cypher.literalOf(new TreeMap<>(Map.of("a", 1, "b", "c", "whatever", Cypher.literalOf(1))));
		assertThat(l)
			.hasToString("MapLiteral{cypher={a: 1, b: 'c', whatever: 1}}");
	}

	@Test // GH-694
	@SuppressWarnings("deprecation")
	void fullTextCallShouldWork() {


		var city = Cypher.anyNode("city");
		var node = Cypher.name("node");
		var score = Cypher.name("score");
		var statement = Cypher.match(Cypher.path("path").definedBy(Cypher.node("Person").named("p").relationshipTo(city, "TRAVELED_TO")))
			.where(
				Cypher.exists(
					Cypher.call("db.index.fulltext.queryNodes").withArgs(Cypher.literalOf("City"), Cypher.literalOf("ham*"))
						.yield(node, score)
						.with(Cypher.asterisk())
						.where(Functions.id(Cypher.anyNode(node)).eq(Functions.id(city)))
						.returning(node, score)
						.build()
				)
			).returning(Cypher.name("path")).build();
		assertThat(statement.getCypher()).isEqualTo("""
			MATCH path = (p:`Person`)-[:`TRAVELED_TO`]->(city)
			WHERE EXISTS {
			CALL db.index.fulltext.queryNodes('City', 'ham*')
			YIELD node, score
			WITH *
			WHERE id(node) = id(city)
			RETURN node, score
			}
			RETURN path""".replace("\n", " "));
	}

	@Test // GH-712
	void relationshipChainsMustActivelyEnterRelationshipsDuringVisit() {

		var tom = Cypher.node("Person").named("person0")
			.withProperties("name", Cypher.literalOf("Tom Hanks"));
		var movie = Cypher.node("Movie").named("movie0");
		var coActors = Cypher.node("Person").named("person1");
		var path0 = Cypher.path("path0");
		var path1 = Cypher.path("path1");

		var statement = Cypher
			.match(path0.definedBy(tom.relationshipTo(movie, "ACTED_IN").named("acted_in0").relationshipFrom(coActors, "ACTED_IN")))
			.match(path1.definedBy(tom.relationshipTo(movie, "ACTED_IN").named("acted_in0").relationshipFrom(coActors, "ACTED_IN")))
			.returning("path0", "path1")
			.build();

		var cypher = Renderer.getRenderer(Configuration.prettyPrinting()).render(statement);
		assertThat(cypher).isEqualTo("""
			MATCH path0 = (person0:Person {
			  name: 'Tom Hanks'
			})-[acted_in0:ACTED_IN]->(movie0:Movie)<-[:ACTED_IN]-(person1:Person)
			MATCH path1 = (person0)-[acted_in0]->(movie0)<-[:ACTED_IN]-(person1)
			RETURN path0, path1""");
	}

	@Test // GH-749
	void nonBuilderRelationshipMethodShouldWork() {

		var n1 = Cypher.node("N1");
		var n2 = Cypher.node("N2");

		assertThat(Cypher.match(n1.relationshipWith(n2, Relationship.Direction.LTR, "X").named("x")).returning(Cypher.asterisk()).build().getCypher())
			.isEqualTo("MATCH (:`N1`)-[x:`X`]->(:`N2`) RETURN *");
		assertThat(Cypher.match(n1.relationshipWith(n2, Relationship.Direction.RTL, "X").named("x")).returning(Cypher.asterisk()).build().getCypher())
			.isEqualTo("MATCH (:`N1`)<-[x:`X`]-(:`N2`) RETURN *");
		assertThat(Cypher.match(n1.relationshipWith(n2, Relationship.Direction.UNI, "X").named("x")).returning(Cypher.asterisk()).build().getCypher())
			.isEqualTo("MATCH (:`N1`)-[x:`X`]-(:`N2`) RETURN *");
	}

	@Test // GH-826
	void identifiablesCreatedInSubqueriesMustBeRecognizedAsSeenToo() {

		var n1 = Cypher.node("Foo").named("n1");
		var n2 = Cypher.node("Bar").named("n2");
		var resultStatement = Cypher
			.create(n1)
			.with(n1)
			.call(
				Cypher.with(n1)
					.merge(n2.withProperties("foo", Cypher.literalOf("x")))
					.create(n1.relationshipTo(n2, "NESTED"))
					.returning(Cypher.count(n2).as("foo_2"))
					.build()
			)
			.returning(Cypher.literalTrue())
			.build();

		assertThat(resultStatement.getCypher())
			.isEqualTo(
				"CREATE (n1:`Foo`) WITH n1 CALL {WITH n1 MERGE (n2:`Bar` {foo: 'x'}) CREATE (n1)-[:`NESTED`]->(n2) RETURN count(n2) AS foo_2} RETURN true");
	}

	@Test // GH-832
	void sequentialExistingSubqueriesShouldNotHaveScopingIssues() {

		var n1 = Cypher.node("Foo").named("n1");
		var n2 = Cypher.node("Bar").named("n2");
		var resultStatement = Cypher.match(n1)
			.where(
				Cypher.match(n1.relationshipTo(n2)).where(n2.property("bar").isFalse()).asCondition()
					.or(Cypher.match(n1.relationshipTo(n2)).where(n2.property("foo").isTrue()).asCondition())
			)
			.returning(Cypher.literalTrue())
			.build();

		assertThat(resultStatement.getCypher())
			.isEqualTo("MATCH (n1:`Foo`) "
					+ "WHERE (EXISTS {"
					+ " MATCH (n1)-->(n2:`Bar`)"
					+ " WHERE n2.bar = false "
					+ "} "
					+ "OR EXISTS {"
					+ " MATCH (n1)-->(n2:`Bar`)"
					+ " WHERE n2.foo = true "
					+ "}) "
					+ "RETURN true");
	}

	@Test // GH-838
	void aliasMustBeIncludedInSubqueries1() {
		var n1 = Cypher.node("Foo").named("n1");
		var point = Cypher.name("point");
		var resultStatement = Cypher.match(n1)
			.returning(n1.project("points", Cypher.collect(
				Cypher.unwind(n1.property("points")).as(point)
					.returning(Cypher.listOf(point.property("x"), point.property("y")))
					.build()
			)))
			.build();

		assertThat(resultStatement.getCypher())
			.isEqualTo("MATCH (n1:`Foo`) "
					+ "RETURN n1{"
					+ "points: COLLECT {"
					+ " UNWIND n1.points AS point"
					+ " RETURN [point.x, point.y]"
					+ " }"
					+ "}");
	}

	@Test // GH-838
	void aliasMustBeIncludedInSubqueries2() {
		var n1 = Cypher.node("Foo").named("n1");
		var point = Cypher.name("point");
		var points = Cypher.name("points");
		var resultStatement = Cypher.match(n1)
			.returning(n1.project("points", org.neo4j.cypherdsl.core.Cypher.collect(
				Cypher
					.with(n1.property("points").as(points))
					.unwind(points).as(point)
					.returning(Cypher.listOf(point.property("x"), point.property("y")))
					.build()
			)))
			.build();

		assertThat(resultStatement.getCypher())
			.isEqualTo("MATCH (n1:`Foo`) "
					   + "RETURN n1{"
					   + "points: COLLECT {"
					   + " WITH n1.points AS points"
					   + " UNWIND points AS point"
					   + " RETURN [point.x, point.y]"
					   + " }"
					   + "}");
	}

	@Test // GH-838
	void aliasMustBeIncludedInSubqueries3() {
		var n1 = Cypher.node("Foo").named("n1");
		var n2 = Cypher.node("Bar").named("n2");
		var point = Cypher.name("point");
		var resultStatement = Cypher.match(n1)
			.returning(n1.project("points", Cypher.collect(
				Cypher.unwind(n1.property("points")).as(point)
					.match(n2)
					.where(n2.property("loc").eq(point))
					.returning(
						n2.project(n2.property("y").as("x"), Cypher.collect(Cypher.match(Cypher.node("FooBar").named("fb")).returning(Cypher.name("fb").as("foo")).build()).as("y"))
					)
					.build()
			)))
			.build();

		assertThat(resultStatement.getCypher())
			.isEqualTo("MATCH (n1:`Foo`) "
					   + "RETURN n1{"
					   + "points: COLLECT {"
					   + " UNWIND n1.points AS point"
					   + " MATCH (n2:`Bar`)"
					   + " WHERE n2.loc = point"
					   + " RETURN n2{x: n2.y, y: COLLECT { MATCH (fb:`FooBar`) RETURN fb AS foo }}"
					   + " }"
					   + "}");
	}

	@Test // GH-533
	void additionalUnitSubqueries() {

		var createThis1 = Cypher.name("create_this1");
		var createVar0 = Cypher.name("create_var0");
		var createVar2 = Cypher.name("create_var2");
		var createThis5 = Cypher.name("create_this5");
		var createVar3 = Cypher.name("create_var3");
		var stmt = Cypher.unwind(Cypher.parameter("create_param0")).as(createVar0)
			.call(
				Cypher.with(createVar0)
					.create(Cypher.node("Movie").named(createThis1))
					.set(createThis1.property("id").to(createVar0.property("id")))
					.with(createThis1, createVar0)
					.call(
						Cypher.with(createThis1, createVar0)
							.unwind(createVar0.property("actors").property("create")).as(createVar2)
							.with(createVar2.property("node").as(createVar3), createVar2.property("edge").as("create_var4"),
								createThis1)
							.create(Cypher.node("Actor").named(createThis5))
							.set(createThis5.property("name").to(createVar3.property("name")))
							.merge(Cypher.anyNode(createThis1).relationshipFrom(Cypher.anyNode(createThis5), "ACTED_IN").named("create_this6"))
							.build()
					)
					.returning(createThis1)
					.build()
			).returning(Cypher.collect(createThis1.project("id")).as("data"))
			.build();

		String cypher = Renderer.getRenderer(Configuration.prettyPrinting()).render(stmt);
		assertThat(cypher)
			.isEqualTo("""
				UNWIND $create_param0 AS create_var0
				CALL {
				  WITH create_var0
				  CREATE (create_this1:Movie)
				  SET create_this1.id = create_var0.id
				  WITH create_this1, create_var0
				  CALL {
				    WITH create_this1, create_var0
				    UNWIND create_var0.actors.create AS create_var2
				    WITH create_var2.node AS create_var3, create_var2.edge AS create_var4, create_this1
				    CREATE (create_this5:Actor)
				    SET create_this5.name = create_var3.name
				    MERGE (create_this1)<-[create_this6:ACTED_IN]-(create_this5)
				  }
				  RETURN create_this1
				}
				RETURN collect(create_this1 {
				  .id
				}) AS data""");
	}

	@Test // GH-903
	void variablesOfListPredicatesMustNotBeScoped() {

		var expected = """
			MATCH (this:`Movie`)
			WHERE single(this0 IN [(this)-[this1:`IN_GENRE`]->(this0:`Genre`) WHERE this0.name = $param0 | 1] WHERE true)
			RETURN this{.actorCount} AS this""";


		var movie = Cypher.node("Movie").named("this");
		var genre = Cypher.node("Genre").named("this0");
		var rel = movie.relationshipTo(genre, "IN_GENRE").named("this1");
		var statement = Cypher.match(movie)
			.where(Cypher.single("this0").in(Cypher.listBasedOn(rel).where(genre.property("name").eq(Cypher.parameter("param0"))).returning(Cypher.literalOf(1))).where(Cypher.isTrue()))
			.returning(movie.project("actorCount").as("this")).build();
		assertThat(statement.getCypher())
			.isEqualTo(expected.replace("\n", " "));
	}

	@Test // GH-999
	void subQueryFromParserScope() {
		var cfg = Configuration.newConfig()
			.withPrettyPrint(true)
			.withGeneratedNames(true)
			.build();
		var renderer = Renderer.getRenderer(cfg);

		var named = Cypher.node("Movie").named("m");
		var stmt = Cypher.call(Cypher.create(named).returning("m").build())
			.call(Cypher.returning(named.project(Cypher.property(named.getRequiredSymbolicName(), "title")).as("movies")).build(), named)
			.returning("movies").build();
		var normalized = renderer.render(stmt);
		assertThat(normalized).isEqualTo("""
			CALL {
			  CREATE (v0:Movie)
			  RETURN v0
			}
			CALL {
			  WITH v0
			  RETURN v0 {
			    .title
			  } AS v1
			}
			RETURN v1""");
	}

	@Test // GH-1014
	void patternExpressionMustNotIntroduceNames() {

		var userIdParam = Cypher.parameter("userId");

		var book = Cypher.node("Book").named("b");
		var userActivity = Cypher.node("UserSuggestionActivity")
			.named("ua")
			.withProperties("userId", userIdParam);


		// ua has not been used before
		var rel = book.relationshipBetween(userActivity);
		var cypher = Cypher.match(book)
			.where(Cypher.not(Cypher.exists(rel)))
			.returning(Cypher.asterisk())
			.build().getCypher();

		assertThat(cypher).isEqualTo("MATCH (b:`Book`) WHERE NOT (exists((b)--(:`UserSuggestionActivity` {userId: $userId}))) RETURN *");

		// b has not been used before
		cypher = Cypher.match(userActivity)
			.where(Cypher.not(Cypher.exists(rel)))
			.returning(Cypher.asterisk())
			.build().getCypher();

		assertThat(cypher).isEqualTo("MATCH (ua:`UserSuggestionActivity` {userId: $userId}) WHERE NOT (exists((:`Book`)--(ua))) RETURN *");

		// Both have been used before, example does not make much sense, but still
		cypher = Cypher.match(book.relationshipFrom(userActivity, "WROTE"))
			.where(Cypher.not(Cypher.exists(rel)))
			.returning(Cypher.asterisk())
			.build().getCypher();

		assertThat(cypher).isEqualTo("MATCH (b:`Book`)<-[:`WROTE`]-(ua:`UserSuggestionActivity` {userId: $userId}) WHERE NOT (exists((b)--(ua))) RETURN *");

		// All expressions
		cypher = Cypher.match(book)
			.returning(Cypher.size(rel))
			.build().getCypher();

		assertThat(cypher).isEqualTo("MATCH (b:`Book`) RETURN size((b)--(:`UserSuggestionActivity` {userId: $userId}))");
	}

	@Test
	void generatedNamesWithDeeplyNestedSubquerie() {
		var m = Cypher.node("Movie").named("this");
		var a = Cypher.node("Actor").named("this1");
		var edge = Cypher.name("edge");
		var this1 = Cypher.name("this1");
		var edges = Cypher.name("edges");
		var stmt = Cypher.match(m)
			.call(
				Cypher.match(m.relationshipFrom(a, "ACTED_IN").named("this0"))
				.with(Cypher.collect(Cypher.mapOf("node", this1)).as(edges))
				.with(edges, Cypher.size(edges).as("totalCount"))
				.call(
					Cypher
						.unwind(edges).as(edge)
						.with(edge.property("node").as(this1))
						.call(Cypher.match(Cypher.anyNode(this1).relationshipTo(Cypher.node("Movie").named("this3"), "ACTED_IN").named("this2")).returning(Cypher.asterisk()).build(), this1).build(),
					edges
				).build(), m)
			.returning(Cypher.asterisk()).build();

		var renderer = Renderer.getRenderer(Configuration.newConfig()
			.withPrettyPrint(true)
			.withGeneratedNames(true)
			.build());

		var expected = """
			MATCH (v0:Movie)
			CALL {
			  WITH v0
			  MATCH (v0)<-[v1:ACTED_IN]-(v2:Actor)
			  WITH collect( {
			    node: v2
			  }) AS v3
			  WITH v3, size(v3) AS v4
			  CALL {
			    WITH v3
			    UNWIND v3 AS v5
			    WITH v5.node AS v6
			    CALL {
			      WITH v6
			      MATCH (v6)-[v0:ACTED_IN]->(v1:Movie)
			      RETURN *
			    }
			  }
			}
			RETURN *""";
		assertThat(renderer.render(stmt)).isEqualTo(expected);
	}

	@Nested
	class Chaining {

		@Test
		void afterYieldingCalls() {
			StatementBuilder.OngoingStandaloneCallWithReturnFields statementPart1 = Cypher.call(
					"db.index.vector.queryNodes")
				.withArgs(
					Cypher.parameter("indexName"),
					Cypher.parameter("numberOfNearestNeighbours"),
					Cypher.parameter("embeddingValue")
				)
				.yield("node", "score");

			// statement 2
			var node = Cypher.anyNode().named("node");
			var relatedNode = Cypher.anyNode().named("relatedNode");
			var relationship = node.relationshipTo(relatedNode, "CONNECTED_TO");
			var statementPart2 = Cypher.match(relationship).returning(relatedNode.property("value").as("value"));

			var newStatement = statementPart1.andThen(statementPart2.build()).build();

			assertThat(newStatement.getCypher()).isEqualToIgnoringWhitespace("""
				CALL db.index.vector.queryNodes($indexName, $numberOfNearestNeighbours, $embeddingValue)
				YIELD node, score
				MATCH (node)-[:`CONNECTED_TO`]->(relatedNode) RETURN relatedNode.value AS value""");
		}
	}
}
