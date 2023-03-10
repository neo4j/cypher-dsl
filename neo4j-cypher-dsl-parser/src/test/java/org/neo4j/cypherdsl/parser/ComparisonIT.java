/*
 * Copyright (c) "Neo4j"
 * Neo4j Sweden AB [http://neo4j.com]
 *
 * This file is part of Neo4j.
 *
 * Neo4j is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.neo4j.cypherdsl.parser;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Map;
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

/**
 * Tests combining parser and builder and demonstrating comparisons
 */
class ComparisonIT {

	static Stream<Arguments> generatedNamesShouldBeGood() {
		return Stream.of(
			Arguments.of(
				"""
					MATCH
						(charlie:Person {name: 'Charlie Sheen'}),
						(rob:Person {name: 'Rob Reiner'})
					CREATE (rob)-[:`TYPE INCLUDING A SPACE`]->(charlie)
					""",
				"""
					MATCH
						(v0:Person {name: 'Charlie Sheen'}),
						(v1:Person {name: 'Rob Reiner'})
					CREATE (v1)-[:`TYPE INCLUDING A SPACE`]->(v0)
					"""
			),
			Arguments.of(
				"""
					MATCH (actor:Person {name: 'Charlie Sheen'})-[:ACTED_IN]->(movie:Movie)
					RETURN actor{.name, .realName, movies: collect(movie{.title, .year})};
					""",
				"""
					MATCH (v0:Person {name: 'Charlie Sheen'})-[:ACTED_IN]->(v1:Movie)
					RETURN v0{.name, .realName, movies: collect(v1{.title, .year})};
					"""
			),
			Arguments.of(
				"""
					match (n:Person)
					call {
					match (n:Movie {title: 'The Matrix'}) where n.released >= 1900 return n as m
					}
					return n.name
					""",
				"""
					match (v0:Person)
					call {
					match (v0:Movie {title: 'The Matrix'}) where v0.released >= 1900 return v0 as m
					}
					return v0.name
					"""
			),
			Arguments.of(
				"""
					MATCH (n:Person {name: 'Tom Hanks'})
					CALL {
						WITH n
						MATCH (m:Movie)<-[:ACTED_IN]-(n)
						WHERE (m.released >= 1900
						AND n.born = 1956)
						RETURN m
					}
					RETURN n.name, m.title
					""",
				"""
					MATCH (v0:Person {name: 'Tom Hanks'})
					CALL {
						WITH v0
						MATCH (v1:Movie)<-[:ACTED_IN]-(v0)
						WHERE (v1.released >= 1900
						AND v0.born = 1956)
						RETURN v1
					}
					RETURN v0.name, v1.title
					"""
			)
		);
	}

	@ParameterizedTest
	@MethodSource
	void generatedNamesShouldBeGood(String in, String expected) {
		var stmt = Renderer.getRenderer(Configuration.newConfig().useGeneratedNames(true).build())
			.render(CypherParser.parseStatement(in));
		assertThat(stmt).isEqualTo(CypherParser.parseStatement(expected).getCypher());
	}

	@Test
	void shouldDetectEquivalentStatements() {

		var stmt1 = CypherParser.parse("Match (x:Movie) where x.title = $param1 RETURN x");
		var stmt2 = Cypher.match(Cypher.node("Movie").named("y")).where(Cypher.name("y").property("title").eq(Cypher.parameter("foo"))).returning(Cypher.name("y")).build();

		assertThat(areSemanticallyEquivalent(stmt1, stmt2)).isTrue();
	}

	@Test
	void shouldDetectEquivalentStatementsWithAnonParameters() {

		var stmt1 = Cypher.match(Cypher.node("Movie").named("x")).where(Cypher.name("x").property("title").eq(Cypher.anonParameter("foo"))).returning(Cypher.name("x")).build();
		var stmt2 = Cypher.match(Cypher.node("Movie").named("y")).where(Cypher.name("y").property("title").eq(Cypher.anonParameter("bar"))).returning(Cypher.name("y")).build();

		assertThat(areSemanticallyEquivalent(stmt1, stmt2)).isTrue();
	}

	@Test
	void shouldDetectNonEquivalentStatements() {

		var stmt1 = CypherParser.parse("Match (x:movie) where x.title = $param1 RETURN x");
		var stmt2 = Cypher.match(Cypher.node("Movie").named("y")).where(Cypher.name("y").property("title").eq(Cypher.parameter("foo"))).returning(Cypher.name("y")).build();

		assertThat(areSemanticallyEquivalent(stmt1, stmt2)).isFalse();
	}

	@Test
	void shouldDetectEquivalentStatementsWithArgs() {

		var stmt1 = CypherParser.parse("Match (x:Movie) where x.title = $param1 RETURN x");
		var stmt2 = Cypher.match(Cypher.node("Movie").named("y")).where(Cypher.name("y").property("title").eq(Cypher.parameter("foo"))).returning(Cypher.name("y")).build();

		assertThat(areSemanticallyEquivalent(stmt1, Map.of("param1", "The Matrix"), stmt2, Map.of("foo", "The Matrix"))).isTrue();
	}

	@Test
	void shouldDetectNonEquivalentStatementsWithArgs() {

		var stmt1 = CypherParser.parse("Match (x:Movie) where x.title = $param1 RETURN x");
		var stmt2 = Cypher.match(Cypher.node("Movie").named("y")).where(Cypher.name("y").property("title").eq(Cypher.parameter("foo"))).returning(Cypher.name("y")).build();

		assertThat(areSemanticallyEquivalent(stmt1, Map.of("param1", "The Matrix"), stmt2, Map.of("foo", "Matrix Resurrections"))).isFalse();
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
		TreeNode.from(stmt).printTo(target::append, node -> node.getValue() instanceof Statement s ? s.getCypher() : node.getValue().toString());
		assertThat(target)
			.isEqualToNormalizingNewlines(
				"""
				└── MATCH (n:`Person` {name: 'Tom Hanks'}) CALL {WITH n MATCH (m:`Movie`)<-[:`ACTED_IN`]-(n) WHERE (m.released >= 1900 AND n.born = 1956) RETURN m} RETURN n.name, m.title
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
				    │       │   └── ExpressionList{cypher=n}
				    │       │       └── SymbolicName{cypher=n}
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
				    │           └── ExpressionList{cypher=m}
				    │               └── SymbolicName{cypher=m}
				    └── Return{cypher=RETURN n.name, m.title}
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

	static boolean areSemanticallyEquivalent(Statement statement1, Statement statement2) {

		var cfg = Configuration.newConfig().useGeneratedNames(true).build();
		var renderer = Renderer.getRenderer(cfg);
		var cypher1 = renderer.render(statement1);
		var cypher2 = renderer.render(statement2);

		return cypher1.equals(cypher2);
	}

	static boolean areSemanticallyEquivalent(Statement statement1, Map<String, Object> args1, Statement statement2, Map<String, Object> args2) {

		if (!areSemanticallyEquivalent(statement1, statement2)) {
			return false;
		}

		var mapping1 = statement1.getCatalog().getRenamedParameters();
		var mapping2 = statement2.getCatalog().getRenamedParameters();
		for (Map.Entry<String, String> entry : mapping1.entrySet()) {
			String key1 = entry.getKey();
			String mapped = entry.getValue();

			String key2 = mapping2.entrySet().stream().filter(e -> e.getValue().equals(mapped))
				.map(Map.Entry::getKey).findFirst().orElseThrow();
			if (!args1.get(key1).equals(args2.get(key2))) {
				return false;
			}
		}

		return true;
	}
}
