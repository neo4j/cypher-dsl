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
package org.neo4j.cypherdsl.parser;

import java.util.EnumSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.Statement;
import org.neo4j.cypherdsl.core.TreeNode;
import org.neo4j.cypherdsl.core.renderer.Configuration;
import org.neo4j.cypherdsl.core.renderer.Renderer;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Tests combining parser and builder and demonstrating comparisons
 */
class ComparisonIT {

	static Stream<Arguments> generatedNamesShouldBeGood() {
		return Stream.of(Arguments.of("""
				MATCH
					(charlie:Person {name: 'Charlie Sheen'}),
					(rob:Person {name: 'Rob Reiner'})
				CREATE (rob)-[:`TYPE INCLUDING A SPACE`]->(charlie)
				""", """
				MATCH
					(v0:Person {name: 'Charlie Sheen'}),
					(v1:Person {name: 'Rob Reiner'})
				CREATE (v1)-[:`TYPE INCLUDING A SPACE`]->(v0)
				""", false), Arguments.of("""
				MATCH (actor:Person {name: 'Charlie Sheen'})-[:ACTED_IN]->(movie:Movie)
				RETURN actor{.name, .realName, movies: collect(movie{.title, .year})};
				""", """
				MATCH (v0:Person {name: 'Charlie Sheen'})-[:ACTED_IN]->(v1:Movie)
				RETURN v0{.name, .realName, movies: collect(v1{.title, .year})};
				""", false), Arguments.of("""
				match (n:Person)
				call {
				match (n:Movie {title: 'The Matrix'}) where n.released >= 1900 return n as m
				}
				return n.name
				""", """
				match (v0:Person)
				call {
				match (v0:Movie {title: 'The Matrix'}) where v0.released >= 1900 return v0 as v1
				}
				return v0.name
				""", false), Arguments.of("""
				MATCH (n:Person {name: 'Tom Hanks'})
				CALL {
					WITH n
					MATCH (m:Movie)<-[:ACTED_IN]-(n)
					WHERE (m.released >= 1900
					AND n.born = 1956)
					RETURN m
				}
				RETURN n.name, m.title
				""", """
				MATCH (v0:Person {name: 'Tom Hanks'})
				CALL {
					WITH v0
					MATCH (v1:Movie)<-[:ACTED_IN]-(v0)
					WHERE (v1.released >= 1900
					AND v0.born = 1956)
					RETURN v1
				}
				RETURN v0.name, v1.title
				""", false), Arguments.of("""
				UNWIND $foo AS input
				CALL {
					WITH input
					CREATE (v0:Movie)
					SET v0.title = input.title
					RETURN v0
				}
				RETURN count(*)
				""", """
				UNWIND $p0 AS v0
				CALL {
					WITH v0
					CREATE (v1:Movie)
					SET v1.title = v0.title
					RETURN v1
				}
				RETURN count(*)
				""", false), Arguments.of("""
				MATCH (this:Movie)
				CALL {
					WITH this
					MATCH (this_actorsAggregate_this1:Actor)-[this_actorsAggregate_this0:ACTED_IN]->(this)
					RETURN {
						min: min(this_actorsAggregate_this0.screentime),
						max: max(this_actorsAggregate_this0.screentime),
						average: avg(this_actorsAggregate_this0.screentime),
						sum: sum(this_actorsAggregate_this0.screentime)
					} AS this_actorsAggregate_var2
				}
				RETURN this {
					actorsAggregate: {
						edge: {
							screentime: this_actorsAggregate_var2
						}
					}
				} AS this
				""", """
				MATCH (v0:Movie)
				CALL {
					WITH v0
					MATCH (v1:Actor)-[v2:ACTED_IN]->(v0)
					RETURN {
						min: min(v2.screentime),
						max: max(v2.screentime),
						average: avg(v2.screentime),
						sum: sum(v2.screentime)
					} AS v3
				}
				RETURN v0 {
					actorsAggregate: {
						edge: {
							screentime: v3
						}
					}
				} AS v0
				""", false), Arguments.of("""
				MATCH (this:Movie)
				CALL {
					WITH this
					MATCH (person:Person)-[edge:ACTED_IN]->(this)
					WITH *
					WHERE person.name CONTAINS $param0
					RETURN count(person) AS this_actorsAggregate_var0
				}
				CALL {
					WITH this
					MATCH (person:Person)-[edge:DIRECTED]->(this)
					WITH *
					WHERE person.name CONTAINS $param1
					RETURN count(person) AS this_directorsAggregate_var0
				}
				RETURN this {
					.title,
					actorsAggregate: {
						count: this_actorsAggregate_var0
					},
					directorsAggregate: {
						count: this_directorsAggregate_var0
					}
				} AS this""", """
				MATCH (v0:Movie)
				CALL {
					WITH v0
					MATCH (v1:Person)-[v2:ACTED_IN]->(v0)
					WITH *
					WHERE v1.name CONTAINS $p0
					RETURN count(v1) AS v3
				}
				CALL {
					WITH v0
					MATCH (v1:Person)-[v2:DIRECTED]->(v0)
					WITH *
					WHERE v1.name CONTAINS $p1
					RETURN count(v1) AS v4
				}
				RETURN v0 {
					.title,
					actorsAggregate: {
						count: v3
					},
					directorsAggregate: {
						count: v4
					}
				} AS v0
				""", false), Arguments.of("""
				MATCH (this:Movie)
				CALL {
					WITH this
					MATCH (this_actorsAggregate_this1:Person)-[this_actorsAggregate_this0:ACTED_IN]->(this)
					WITH *
					WHERE this_actorsAggregate_this1.name CONTAINS $this_actorsAggregate_param0
					RETURN count(this_actorsAggregate_this1) AS this_actorsAggregate_var2
				}
				CALL {
					WITH this
					MATCH (this_directorsAggregate_this1:Person)-[this_directorsAggregate_this0:DIRECTED]->(this)
					WITH *
					WHERE this_directorsAggregate_this1.name CONTAINS $this_directorsAggregate_param0
					RETURN count(this_directorsAggregate_this1) AS this_directorsAggregate_var2
				}
				RETURN this {
					.title,
					actorsAggregate: {
						count: this_actorsAggregate_var2
					},
					directorsAggregate: {
						count: this_directorsAggregate_var2
					}
				} AS this""", """
				MATCH (v0:Movie)
				CALL {
					WITH v0
					MATCH (v1:Person)-[v2:ACTED_IN]->(v0)
					WITH *
					WHERE v1.name CONTAINS $p0
					RETURN count(v1) AS v3
				}
				CALL {
					WITH v0
					MATCH (v1:Person)-[v2:DIRECTED]->(v0)
					WITH *
					WHERE v1.name CONTAINS $p1
					RETURN count(v1) AS v4
				}
				RETURN v0 {
					.title,
					actorsAggregate: {
						count: v3
					},
					directorsAggregate: {
						count: v4
					}
				} AS v0
				""", false), Arguments.of("""
				MATCH (this:Post)
				CALL {
					WITH this
					MATCH (this1:User)-[this0:LIKES]->(this)
					RETURN any(var2 IN collect(this0.someBigInt) WHERE var2 = $param0) AS var3
				}
				WITH *
				WHERE var3 = true
				RETURN this {
					.content
				} AS this
				""", """
				MATCH (v0:Post)
				CALL {
					WITH v0
					MATCH (v1:User)-[v2:LIKES]->(v0)
					RETURN any(v3 IN collect(v2.someBigInt) WHERE v3 = $p0) AS v4
				}
				WITH *
				WHERE v4 = true
				RETURN v0 {
					.content
				} AS v0""", false),
				Arguments.of("MATCH (n:Movie)<-[:ACTED_IN]-(p:Person)",
						"MATCH (v0:`Person`)-[:`ACTED_IN`]->(v1:`Movie`)", true),
				Arguments.of("MATCH (a:A)-[:R]->(b:B)-[:R2]->(c:C) RETURN *",
						"MATCH (v0:`A`)-[:`R`]->(v1:`B`)-[:`R2`]->(v2:`C`) RETURN *", true),
				Arguments.of("MATCH (a:A)-[:R]->(b:B)-[:R2]->(c:C), (x) --> (y) RETURN *",
						"MATCH (v0:`A`)-[:`R`]->(v1:`B`)-[:`R2`]->(v2:`C`), (v3)-->(v4) RETURN *", true),
				Arguments.of("MATCH (a:A)-[:R]->(b:B), (b)-[:R2]->(c:C) RETURN *",
						"MATCH (v0:`A`)-[:`R`]->(v1:`B`), (v1)-[:`R2`]->(v2:`C`) RETURN *", true),
				Arguments.of("MATCH (a:A)<-[:R]-(b:B)-[:R2]->(c:C), (x) --> (y) RETURN *",
						"MATCH (v0:`B`)-[:`R`]->(v1:`A`), (v0)-[:`R2`]->(v2:`C`), (v3)-->(v4) RETURN *", true),
				Arguments.of("MATCH (u:U)<-[:R3]-(a:A)<-[:R]-(b:B)-[:R2]->(c:C), (x) --> (y) RETURN *",
						"MATCH (v0:`A`)-[:`R3`]->(v1:`U`), (v2:`B`)-[:`R`]->(v0), (v2)-[:`R2`]->(v3:`C`), (v4)-->(v5) RETURN *",
						true),
				Arguments.of("MATCH (a:A)<-[:FOO]-(b)-[:BAR]->(c) RETURN *",
						"MATCH (v0)-[:`FOO`]->(v1:`A`), (v0)-[:`BAR`]->(v2) RETURN *", true),
				Arguments.of("MATCH (a:A)<-[:FOO]-(b:B)<-[:BAR]-(c:C) RETURN *",
						"MATCH (v0:`B`)-[:`FOO`]->(v1:`A`), (v2:`C`)-[:`BAR`]->(v0) RETURN *", true));
	}

	static Stream<Arguments> generatedNamesShouldBeConfigurable() {
		return Stream.of(Arguments.of(EnumSet.allOf(Configuration.GeneratedNames.class), """
				MATCH (v0:Movie)
				WHERE v0.released = $p0
				CALL {
					WITH v0
					MATCH (v1:Actor)-[v2:ACTED_IN]->(v0)
					RETURN {
						min: min(v2.screentime),
						max: max(v2.screentime),
						average: avg(v2.screentime),
						sum: sum(v2.screentime)
					} AS v3,
					any(v4 IN collect(v1.someBigInt) WHERE v4 = $p1) AS v5
				}
				RETURN v0 {
					actorsAggregate: {
						edge: {
							screentime: v3
						}
					}
				} AS v0
				"""), Arguments.of(EnumSet.of(Configuration.GeneratedNames.ENTITY_NAMES), """
				MATCH (v0:Movie)
				WHERE v0.released = $releaseYear
				CALL {
					WITH v0
					MATCH (v1:Actor)-[v2:ACTED_IN]->(v0)
					RETURN {
						min: min(v2.screentime),
						max: max(v2.screentime),
						average: avg(v2.screentime),
						sum: sum(v2.screentime)
					} AS this_actorsAggregate_var2,
					any(foo IN collect(v1.someBigInt) WHERE foo = $param0) AS blerg
				}
				RETURN v0 {
					actorsAggregate: {
						edge: {
							screentime: this_actorsAggregate_var2
						}
					}
				} AS this
				"""), Arguments.of(EnumSet.of(Configuration.GeneratedNames.PARAMETER_NAMES), """
				MATCH (this:Movie)
				WHERE this.released = $p0
				CALL {
					WITH this
					MATCH (this_actorsAggregate_this1:Actor)-[this_actorsAggregate_this0:ACTED_IN]->(this)
					RETURN {
						min: min(this_actorsAggregate_this0.screentime),
						max: max(this_actorsAggregate_this0.screentime),
						average: avg(this_actorsAggregate_this0.screentime),
						sum: sum(this_actorsAggregate_this0.screentime)
					} AS this_actorsAggregate_var2,
					any(foo IN collect(this_actorsAggregate_this1.someBigInt) WHERE foo = $p1) AS blerg
				}
				RETURN this {
					actorsAggregate: {
						edge: {
							screentime: this_actorsAggregate_var2
						}
					}
				} AS this
				"""), Arguments.of(EnumSet.of(Configuration.GeneratedNames.ALL_ALIASES), """
				MATCH (this:Movie)
				WHERE this.released = $releaseYear
				CALL {
					WITH this
					MATCH (this_actorsAggregate_this1:Actor)-[this_actorsAggregate_this0:ACTED_IN]->(this)
					RETURN {
						min: min(this_actorsAggregate_this0.screentime),
						max: max(this_actorsAggregate_this0.screentime),
						average: avg(this_actorsAggregate_this0.screentime),
						sum: sum(this_actorsAggregate_this0.screentime)
					} AS v0,
					any(foo IN collect(this_actorsAggregate_this1.someBigInt) WHERE foo = $param0) AS v1
				}
				RETURN this {
					actorsAggregate: {
						edge: {
							screentime: v0
						}
					}
				} AS v2
				"""), Arguments.of(EnumSet.of(Configuration.GeneratedNames.INTERNAL_ALIASES_ONLY), """
				MATCH (this:Movie)
				WHERE this.released = $releaseYear
				CALL {
					WITH this
					MATCH (this_actorsAggregate_this1:Actor)-[this_actorsAggregate_this0:ACTED_IN]->(this)
					RETURN {
						min: min(this_actorsAggregate_this0.screentime),
						max: max(this_actorsAggregate_this0.screentime),
						average: avg(this_actorsAggregate_this0.screentime),
						sum: sum(this_actorsAggregate_this0.screentime)
					} AS v0,
					any(foo IN collect(this_actorsAggregate_this1.someBigInt) WHERE foo = $param0) AS v1
				}
				RETURN this {
					actorsAggregate: {
						edge: {
							screentime: v0
						}
					}
				} AS this
				"""), Arguments.of(EnumSet.complementOf(EnumSet.of(Configuration.GeneratedNames.ALL_ALIASES)), """
				MATCH (v0:Movie)
				WHERE v0.released = $p0
				CALL {
					WITH v0
					MATCH (v1:Actor)-[v2:ACTED_IN]->(v0)
					RETURN {
						min: min(v2.screentime),
						max: max(v2.screentime),
						average: avg(v2.screentime),
						sum: sum(v2.screentime)
					} AS v3,
					any(v4 IN collect(v1.someBigInt) WHERE v4 = $p1) AS v5
				}
				RETURN v0 {
					actorsAggregate: {
						edge: {
							screentime: v3
						}
					}
				} AS this
				"""));
	}

	static boolean areSemanticallyEquivalent(Statement statement1, Statement statement2) {

		var cfg = Configuration.newConfig().withGeneratedNames(true).build();
		var renderer = Renderer.getRenderer(cfg);
		var cypher1 = renderer.render(statement1);
		var cypher2 = renderer.render(statement2);

		return cypher1.equals(cypher2);
	}

	static boolean areSemanticallyEquivalent(Statement statement1, Map<String, Object> args1, Statement statement2,
			Map<String, Object> args2) {

		if (!areSemanticallyEquivalent(statement1, statement2)) {
			return false;
		}

		var mapping1 = statement1.getCatalog().getRenamedParameters();
		var mapping2 = statement2.getCatalog().getRenamedParameters();
		for (Map.Entry<String, String> entry : mapping1.entrySet()) {
			String key1 = entry.getKey();
			String mapped = entry.getValue();

			String key2 = mapping2.entrySet()
				.stream()
				.filter(e -> e.getValue().equals(mapped))
				.map(Map.Entry::getKey)
				.findFirst()
				.orElseThrow();
			if (!args1.get(key1).equals(args2.get(key2))) {
				return false;
			}
		}

		return true;
	}

	@ParameterizedTest
	@MethodSource
	void generatedNamesShouldBeGood(String in, String expected, boolean alwaysCreateRelationshipsLTR) {
		var stmt = Renderer.getRenderer(Configuration.newConfig().withGeneratedNames(true).build())
			.render(CypherParser.parseStatement(in,
					Options.newOptions().alwaysCreateRelationshipsLTR(alwaysCreateRelationshipsLTR).build()));
		assertThat(stmt).isEqualTo(CypherParser.parseStatement(expected).getCypher());
	}

	@ParameterizedTest
	@MethodSource
	void generatedNamesShouldBeConfigurable(Set<Configuration.GeneratedNames> config, String expected) {

		var in = """
				MATCH (this:Movie)
				WHERE this.released = $releaseYear
				CALL {
					WITH this
					MATCH (this_actorsAggregate_this1:Actor)-[this_actorsAggregate_this0:ACTED_IN]->(this)
					RETURN {
						min: min(this_actorsAggregate_this0.screentime),
						max: max(this_actorsAggregate_this0.screentime),
						average: avg(this_actorsAggregate_this0.screentime),
						sum: sum(this_actorsAggregate_this0.screentime)
					} AS this_actorsAggregate_var2,
					any(foo IN collect(this_actorsAggregate_this1.someBigInt) WHERE foo = $param0) AS blerg
				}
				RETURN this {
					actorsAggregate: {
						edge: {
							screentime: this_actorsAggregate_var2
						}
					}
				} AS this
				""";

		var stmt = Renderer.getRenderer(Configuration.newConfig().withGeneratedNames(config).build())
			.render(CypherParser.parseStatement(in));
		assertThat(stmt).isEqualTo(CypherParser.parseStatement(expected).getCypher());
	}

	@Test
	void shouldDetectEquivalentStatements() {

		var stmt1 = CypherParser.parse("Match (x:Movie) where x.title = $param1 RETURN x");
		var stmt2 = Cypher.match(Cypher.node("Movie").named("y"))
			.where(Cypher.name("y").property("title").eq(Cypher.parameter("foo")))
			.returning(Cypher.name("y"))
			.build();

		assertThat(areSemanticallyEquivalent(stmt1, stmt2)).isTrue();
	}

	@Test
	void shouldDetectEquivalentStatementsWithAnonParameters() {

		var stmt1 = Cypher.match(Cypher.node("Movie").named("x"))
			.where(Cypher.name("x").property("title").eq(Cypher.anonParameter("foo")))
			.returning(Cypher.name("x"))
			.build();
		var stmt2 = Cypher.match(Cypher.node("Movie").named("y"))
			.where(Cypher.name("y").property("title").eq(Cypher.anonParameter("bar")))
			.returning(Cypher.name("y"))
			.build();

		assertThat(areSemanticallyEquivalent(stmt1, stmt2)).isTrue();
	}

	@Test
	void shouldDetectNonEquivalentStatements() {

		var stmt1 = CypherParser.parse("Match (x:movie) where x.title = $param1 RETURN x");
		var stmt2 = Cypher.match(Cypher.node("Movie").named("y"))
			.where(Cypher.name("y").property("title").eq(Cypher.parameter("foo")))
			.returning(Cypher.name("y"))
			.build();

		assertThat(areSemanticallyEquivalent(stmt1, stmt2)).isFalse();
	}

	@Test
	void shouldDetectEquivalentStatementsWithArgs() {

		var stmt1 = CypherParser.parse("Match (x:Movie) where x.title = $param1 RETURN x");
		var stmt2 = Cypher.match(Cypher.node("Movie").named("y"))
			.where(Cypher.name("y").property("title").eq(Cypher.parameter("foo")))
			.returning(Cypher.name("y"))
			.build();

		assertThat(areSemanticallyEquivalent(stmt1, Map.of("param1", "The Matrix"), stmt2, Map.of("foo", "The Matrix")))
			.isTrue();
	}

	@Test
	void shouldDetectNonEquivalentStatementsWithArgs() {

		var stmt1 = CypherParser.parse("Match (x:Movie) where x.title = $param1 RETURN x");
		var stmt2 = Cypher.match(Cypher.node("Movie").named("y"))
			.where(Cypher.name("y").property("title").eq(Cypher.parameter("foo")))
			.returning(Cypher.name("y"))
			.build();

		assertThat(areSemanticallyEquivalent(stmt1, Map.of("param1", "The Matrix"), stmt2,
				Map.of("foo", "Matrix Resurrections")))
			.isFalse();
	}

	@Test
	void treeCanBeUsedToAnalyseStatements() {

		var stmt = CypherParser.parse("""
				MATCH (n:Person {name: 'Tom Hanks'})
				CALL {
				  WITH n
				  MATCH (m:Movie)<-[:ACTED_IN]-(n)
				  WHERE (m.released >= 1900
					AND n.born = 1956)
				  RETURN m
				}
				RETURN n.name, m.title
				""");

		var target = new StringBuilder();
		TreeNode.from(stmt)
			.printTo(target::append,
					node -> (node.getValue() instanceof Statement s) ? s.getCypher() : node.getValue().toString());
		assertThat(target).hasToString(
				"""
						MATCH (n:`Person` {name: 'Tom Hanks'}) CALL {WITH n MATCH (m:`Movie`)<-[:`ACTED_IN`]-(n) WHERE (m.released >= 1900 AND n.born = 1956) RETURN m} RETURN n.name, m.title
						├── Match{cypher=MATCH (n:Person {name: 'Tom Hanks'})}
						│   └── Pattern{cypher=(n:Person {name: 'Tom Hanks'})}
						│       └── InternalNodeImpl{cypher=(n:Person {name: 'Tom Hanks'})}
						│           ├── SymbolicName{cypher=n}
						│           ├── NodeLabel{value='Person'}
						│           └── Properties{cypher={name: 'Tom Hanks'}}
						│               └── MapExpression{cypher={name: 'Tom Hanks'}}
						│                   └── KeyValueMapEntry{cypher=name: 'Tom Hanks'}
						│                       └── StringLiteral{cypher='Tom Hanks'}
						├── Subquery{cypher=CALL {WITH n MATCH (m:Movie)<-[:ACTED_IN]-(n) WHERE (m.released >= 1900 AND n.born = 1956) RETURN m}}
						│   └── WITH n MATCH (m:`Movie`)<-[:`ACTED_IN`]-(n) WHERE (m.released >= 1900 AND n.born = 1956) RETURN m
						│       ├── With{cypher=WITH n}
						│       │   └── ReturnBody{cypher=n}
						│       │       └── ExpressionList{cypher=n}
						│       │           └── SymbolicName{cypher=n}
						│       ├── Match{cypher=MATCH (m:Movie)<-[:ACTED_IN]-(n) WHERE (m.released >= 1900 AND n.born = 1956)}
						│       │   ├── Pattern{cypher=(m:Movie)<-[:ACTED_IN]-(n)}
						│       │   │   └── InternalRelationshipImpl{cypher=(m:Movie)<-[:ACTED_IN]-(n)}
						│       │   │       ├── InternalNodeImpl{cypher=(m:Movie)}
						│       │   │       │   ├── SymbolicName{cypher=m}
						│       │   │       │   └── NodeLabel{value='Movie'}
						│       │   │       ├── Details{cypher=<-[:ACTED_IN]-}
						│       │   │       │   └── RelationshipTypes{values=[ACTED_IN]}
						│       │   │       └── InternalNodeImpl{cypher=(n)}
						│       │   │           └── SymbolicName{cypher=n}
						│       │   └── Where{cypher=WHERE (m.released >= 1900 AND n.born = 1956)}
						│       │       └── CompoundCondition{cypher=(m.released >= 1900 AND n.born = 1956)}
						│       │           ├── Comparison{cypher=m.released >= 1900}
						│       │           │   ├── InternalPropertyImpl{cypher=m.released}
						│       │           │   │   ├── SymbolicName{cypher=m}
						│       │           │   │   └── PropertyLookup{cypher=.released}
						│       │           │   │       └── SymbolicName{cypher=released}
						│       │           │   ├── Operator{cypher=>=}
						│       │           │   └── NumberLiteral{cypher=1900}
						│       │           ├── Operator{cypher=AND}
						│       │           └── Comparison{cypher=n.born = 1956}
						│       │               ├── InternalPropertyImpl{cypher=n.born}
						│       │               │   ├── SymbolicName{cypher=n}
						│       │               │   └── PropertyLookup{cypher=.born}
						│       │               │       └── SymbolicName{cypher=born}
						│       │               ├── Operator{cypher==}
						│       │               └── NumberLiteral{cypher=1956}
						│       └── Return{cypher=RETURN m}
						│           └── ReturnBody{cypher=m}
						│               └── ExpressionList{cypher=m}
						│                   └── SymbolicName{cypher=m}
						└── Return{cypher=RETURN n.name, m.title}
						    └── ReturnBody{cypher=n.name, m.title}
						        └── ExpressionList{cypher=n.name, m.title}
						            ├── InternalPropertyImpl{cypher=n.name}
						            │   ├── SymbolicName{cypher=n}
						            │   └── PropertyLookup{cypher=.name}
						            │       └── SymbolicName{cypher=name}
						            └── InternalPropertyImpl{cypher=m.title}
						                ├── SymbolicName{cypher=m}
						                └── PropertyLookup{cypher=.title}
						                    └── SymbolicName{cypher=title}
						""");
	}

	@Test // GH-963
	void scopingAndNormalizingShouldWork() {
		var cypher = """
				CALL {
					CREATE (this0:Movie)
					WITH *
					CALL {
						WITH this0
						return true
					}
					RETURN this0
				} RETURN this0
				""";
		var cfg = Configuration.newConfig().withPrettyPrint(true).withGeneratedNames(true).build();
		var renderer = Renderer.getRenderer(cfg);
		var parseOptions = Options.newOptions().createSortedMaps(true).build();
		var normalized = renderer.render(CypherParser.parse(cypher, parseOptions));
		assertThat(normalized).isEqualTo("""
				CALL {
				  CREATE (v0:Movie)
				  WITH *
				  CALL {
				    WITH v0
				    RETURN true
				  }
				  RETURN v0
				}
				RETURN v0""");
	}

	@Test // GH-1147
	void shouldNotRecognizeLocallyScopedElementsFromExistentialSubqueries() {
		var cypher = """
				MATCH (this:Series)
				WHERE ((EXISTS {
				MATCH (this)-[edge:MANUFACTURER]->(this0:Manufacturer)
					WHERE (this0.name = $param0 AND edge.current = $param1)
				}
				OR EXISTS {
					MATCH (this)-[edge:MANUFACTURER]->(this1:Manufacturer)
					WHERE (this1.name = $param2 AND edge.current = $param3)
				})
				AND EXISTS {
					MATCH (this)-[edge:BRAND]->(this2:Brand)
					WHERE (this2.name = $param4 AND edge.current = $param5)
				})
				RETURN this""";
		var renderer = Renderer
			.getRenderer(Configuration.newConfig().withPrettyPrint(true).withGeneratedNames(true).build());
		var normalized = renderer.render(CypherParser.parse(cypher));
		assertThat(normalized).isEqualTo("""
				MATCH (v0:Series)
				WHERE ((EXISTS {
				      MATCH (v0)-[v1:MANUFACTURER]->(v2:Manufacturer)
				      WHERE (v2.name = $p0
				        AND v1.current = $p1)
				    }
				    OR EXISTS {
				      MATCH (v0)-[v1:MANUFACTURER]->(v2:Manufacturer)
				      WHERE (v2.name = $p2
				        AND v1.current = $p3)
				    })
				  AND EXISTS {
				    MATCH (v0)-[v1:BRAND]->(v2:Brand)
				    WHERE (v2.name = $p4
				      AND v1.current = $p5)
				  })
				RETURN v0""");
	}

}
