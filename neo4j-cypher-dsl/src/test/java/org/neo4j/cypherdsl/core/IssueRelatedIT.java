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

import org.junit.jupiter.api.Test;
import org.neo4j.cypherdsl.core.renderer.Renderer;

/**
 * @author Michael J. Simons
 */
class IssueRelatedIT {

	private static final Renderer cypherRenderer = Renderer.getDefaultRenderer();

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
				Functions.collect(Functions.relationships(p)).as("rels"),
				Functions.collect(Functions.nodes(p)).as("nodes")).build();

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
				"MATCH \\([a-zA-Z]*\\d{3}:`Fruit` \\{kind: 'strawberry'\\}\\) SET [a-zA-Z]*\\d{3}\\.color = 'red'");
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

		Statement statement = Cypher.match(aFl, lFr)
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
				"MATCH (app:`Location` {uuid: $app_uuid})<-[:`PART_OF`*0..3]-(loc_start:`Location`), (loc_start)<-[:`IN`|`IN_ANALYTICS`]-(r:`Resume`) WITH DISTINCT r, loc_start, app MATCH (r)-[:`IN_COHORT_OF`]->(o:`Offer` {is_valid: true})-[:`IN`]->(app) WITH DISTINCT r, loc_start, app, o MATCH (o:`Offer`)-[:`FOR`]->(start_n:`ResumeNode`) WHERE id(start_n) IN $start_ids RETURN DISTINCT r, loc_start, app, o, start_n");
	}

	@Test
	void gh174() {
		final Node r = Cypher.node("Resume").named("r");
		final Node o = Cypher.node("Offer").named("o");

		Statement s = Cypher.match(r.relationshipTo(o, "FOR"))
			.where(r.hasLabels("LastResume").not())
			.and(
				Functions.coalesce(o.property("valid_only"), Cypher.literalFalse()).isEqualTo(Cypher.literalFalse())
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
				Functions.coalesce(o.property("valid_only"), Cypher.literalFalse()).isEqualTo(Cypher.literalFalse())
					.and(r.hasLabels("InvalidStatus").not())
					.or(o.property("valid_only").isTrue()
						.and(r.hasLabels("ValidStatus")))
			)
			.and(r.property("is_internship").isTrue()
				.and(Functions.size(r.relationshipTo(Cypher.anyNode(), "PART_OF")).isEmpty())
				.not())
			.and(r.property("is_sandwich_training").isTrue()
				.and(Functions.size(r.relationshipTo(Cypher.anyNode(), "PART_OF")).isEmpty())
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
			.where(Conditions.not(Predicates.exists(r.relationshipTo(u, "EXCLUDES"))))
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
			.with(Functions.head(Functions.collect(r.getRequiredSymbolicName())).as("r"))
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
			.returning(Functions.countDistinct(r.getRequiredSymbolicName()).as("r"))
			.build();

		assertThat(cypherRenderer.render(s))
			.isEqualTo("MATCH (r:`Resume`)<-[:`HAS`]-(u:`User`) RETURN count(DISTINCT r) AS r");
	}

	@Test
	void gh197() {
		final Node n = Cypher.node("Person").named("n");

		// avg
		Statement s = Cypher.match(n)
			.returning(Functions.avg(n.property("age")))
			.build();
		assertThat(cypherRenderer.render(s))
			.isEqualTo("MATCH (n:`Person`) RETURN avg(n.age)");

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
			.returning(Functions.max(Cypher.name("val"))).build();
		assertThat(cypherRenderer.render(s))
			.isEqualTo("UNWIND [1, 'a', NULL, 0.2, 'b', '1', '99'] AS val RETURN max(val)");
		s = Cypher.unwind(list).as("val")
			.returning(Functions.min(Cypher.name("val"))).build();
		assertThat(cypherRenderer.render(s))
			.isEqualTo("UNWIND [1, 'a', NULL, 0.2, 'b', '1', '99'] AS val RETURN min(val)");

		// percentileCont/percentileDisc
		s = Cypher.match(n)
			.returning(Functions.percentileCont(n.property("age"), 0.4))
			.build();
		assertThat(cypherRenderer.render(s))
			.isEqualTo("MATCH (n:`Person`) RETURN percentileCont(n.age, 0.4)");
		s = Cypher.match(n)
			.returning(Functions.percentileDisc(n.property("age"), 0.5))
			.build();
		assertThat(cypherRenderer.render(s))
			.isEqualTo("MATCH (n:`Person`) RETURN percentileDisc(n.age, 0.5)");

		// stDev/stDevP
		s = Cypher.match(n)
			.where(n.property("name").in(
				Cypher.listOf(Cypher.literalOf("A"), Cypher.literalOf("B"), Cypher.literalOf("C"))))
			.returning(Functions.stDev(n.property("age")))
			.build();
		assertThat(cypherRenderer.render(s))
			.isEqualTo("MATCH (n:`Person`) WHERE n.name IN ['A', 'B', 'C'] RETURN stDev(n.age)");
		s = Cypher.match(n)
			.where(n.property("name").in(
				Cypher.listOf(Cypher.literalOf("A"), Cypher.literalOf("B"), Cypher.literalOf("C"))))
			.returning(Functions.stDevP(n.property("age")))
			.build();
		assertThat(cypherRenderer.render(s))
			.isEqualTo("MATCH (n:`Person`) WHERE n.name IN ['A', 'B', 'C'] RETURN stDevP(n.age)");

		// sum
		s = Cypher.match(n)
			.with(Cypher.listOf(Cypher.mapOf(
				"type", n.getRequiredSymbolicName(),
				"nb", Functions.sum(n.getRequiredSymbolicName())))
				.as("counts"))
			.returning(Functions.sum(n.property("age")))
			.build();
		assertThat(cypherRenderer.render(s))
			.isEqualTo("MATCH (n:`Person`) WITH [{type: n, nb: sum(n)}] AS counts RETURN sum(n.age)");
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
		String expected = "MATCH (p:`Person`) RETURN p{alias: p.name}";

		Node n = Cypher.node("Person").named("p");
		Statement statement;
		statement = Cypher.match(n)
			.returning(n.project("alias", n.property("name")))
			.build();
		assertThat(cypherRenderer.render(statement)).isEqualTo(expected);

		statement = Cypher.match(n)
			.returning(n.project(n.property("name").as("alias")))
			.build();
		assertThat(cypherRenderer.render(statement)).isEqualTo(expected);
	}

	@Test
	void gh44() {

		Node n = Cypher.anyNode("n");
		Statement statement = Cypher.match(n)
			.returning(Functions.collectDistinct(n).as("distinctNodes"))
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

		Node node = Cypher.anyNode().named("n");
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
		AliasedExpression rnAliasedAsNN = rnNode.as("nn");
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

		Statement statement = Cypher.returning(Cypher.property(Functions.datetime(), "epochSeconds")).build();
		assertThat(cypherRenderer.render(statement))
			.isEqualTo("RETURN datetime().epochSeconds");
	}

	@Test // GH-123
	void propertiesOfFunctionsInsideQuery() {

		Expression collectedThings = Functions.collect(Cypher.name("n")).as("collectedThings");
		Statement statement = Cypher.match(Cypher.anyNode().named("n"))
			.with(collectedThings)
			.returning(Cypher.property(Functions.last(collectedThings), "name")).build();
		assertThat(cypherRenderer.render(statement))
			.isEqualTo("MATCH (n) WITH collect(n) AS collectedThings RETURN last(collectedThings).name");
	}

	@Test
	void gh127() {

		Node node = Cypher.node("Person").named("person");
		SymbolicName key = Cypher.name("key");
		String dynamicPrefix = "properties.";
		Statement statement = Cypher
			.match(node)
			.returning(node.project(
				"json",
				Cypher.call("apoc.map.fromPairs")
					.withArgs(
						Cypher.listWith(key)
							.in(Cypher.call("keys").withArgs(node.getRequiredSymbolicName()).asFunction())
							.where(key.startsWith(Cypher.literalOf(dynamicPrefix)))
							.returning(
								Cypher.call("substring")
									.withArgs(key, Cypher.literalOf(dynamicPrefix.length())).asFunction(),
								node.property(key)
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
		Node person = Cypher.anyNode().named("person");
		StatementBuilder.OngoingReadingAndReturn cypher = Cypher
				.match(company)
				.where(Predicates.any(cond).in(Cypher
						.listBasedOn(company.relationshipTo(person, "WORKS_AT"))
						.returning(person.property("name").isEqualTo(Cypher.parameter("name"))))
						.where(cond.asCondition())
				)
				.returning(company);

		assertThat(cypherRenderer.render(cypher.build()))
				.isEqualTo("MATCH (company:`Company`) WHERE any(cond IN [(company)-[:`WORKS_AT`]->(person) | person.name = $name] WHERE cond) RETURN company");
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

		Node node = Cypher.node("Person").named("person");
		Statement statement = Cypher.match(node)
			.where(
				node.relationshipTo(Cypher.anyNode(), "A").asCondition().or(node.relationshipTo(Cypher.anyNode(), "B"))
			)
			.and(
				node.relationshipTo(Cypher.anyNode(), "C").asCondition()
					.or(
						node.relationshipTo(Cypher.anyNode(), "D").asCondition()
							.and(node.relationshipTo(Cypher.anyNode(), "E"))
					)
					.or(node.relationshipTo(Cypher.anyNode(), "F"))
			)
			.returning(node).build();

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

		Node person = Cypher.node("Person").named("person");

		Parameter location = Cypher.parameter("location");
		Property distance = Cypher.property(location, "distance");

		Expression point = Functions.point(Cypher.property(location, "point"));

		Statement statement = Cypher
			.match(person)
			.where(Functions.distance(person.property("location"), point).isEqualTo(distance))
			.returning(person).build();

		assertThat(cypherRenderer.render(statement))
			.isEqualTo("MATCH (person:`Person`) WHERE distance(person.location, point($location.point)) = $location.distance RETURN person");
	}
}
