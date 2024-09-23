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
package org.neo4j.cypherdsl.parser;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.neo4j.cypherdsl.core.Statement;
import org.neo4j.cypherdsl.core.renderer.Configuration;
import org.neo4j.cypherdsl.core.renderer.Renderer;

class ParserIssuesIT {

	@Test // GH-1076
	void parsingDeeplyNestedSubqueriesThenNormalizeShouldWork() {
		var cypher = """
			MATCH (this:Movie)
			WHERE this.title = $param0
			CALL {
				WITH this
				MATCH (this)<-[this0:ACTED_IN]-(this1:Actor)
				WITH collect( {
					node: this1,
					relationship: this0
				}) AS edges
				WITH edges, size(edges) AS totalCount
				CALL {
					WITH edges
					UNWIND edges AS edge
					WITH edge.node AS this1, edge.relationship AS this0
					CALL {
						WITH this1
						MATCH (this1)-[this2:ACTED_IN]->(this3:Movie)
						WITH collect( {
							node: this3,
							relationship: this2
						}) AS edges
						WITH edges, size(edges) AS totalCount
						RETURN {
							totalCount: totalCount
						} AS var9
					}
					RETURN collect( {
						properties: {
							screenTime: this0.screenTime,
							__resolveType: 'ActedIn'
						},
						node: {
							name: this1.name,
							moviesConnection: var9,
							__resolveType: 'Actor'
						}
					}) AS var10
				}
				RETURN {
					edges: var10,
					totalCount: totalCount
				} AS var11
			}
			RETURN this {
				.title,
				actorsConnection: var11
			} AS this
			""";
		var renderer = Renderer.getRenderer(Configuration.newConfig()
			.withPrettyPrint(true)
			.withGeneratedNames(true)
			.build());
		var normalized = renderer.render(CypherParser.parse(cypher, Options.defaultOptions()));
		assertThat(normalized).isEqualTo("""
			MATCH (v0:Movie)
			WHERE v0.title = $p0
			CALL {
			  WITH v0
			  MATCH (v0)<-[v1:ACTED_IN]-(v2:Actor)
			  WITH collect( {
			    node: v2,
			    relationship: v1
			  }) AS v3
			  WITH v3, size(v3) AS v4
			  CALL {
			    WITH v3
			    UNWIND v3 AS v5
			    WITH v5.node AS v6, v5.relationship AS v7
			    CALL {
			      WITH v6
			      MATCH (v6)-[v0:ACTED_IN]->(v1:Movie)
			      WITH collect( {
			        node: v1,
			        relationship: v0
			      }) AS v2
			      WITH v2, size(v2) AS v4
			      RETURN {
			        totalCount: v4
			      } AS v8
			    }
			    RETURN collect( {
			      properties: {
			        screenTime: v7.screenTime,
			        __resolveType: 'ActedIn'
			      },
			      node: {
			        name: v6.name,
			        moviesConnection: v8,
			        __resolveType: 'Actor'
			      }
			    }) AS v9
			  }
			  RETURN {
			    edges: v9,
			    totalCount: v4
			  } AS v10
			}
			RETURN v0 {
			  .title,
			  actorsConnection: v10
			} AS v5""");
	}

	@Test // GH-1075
	void unionPartsMustBeExportedToScope() {

		var cypher = """
			MATCH (this:Actor)
			CALL {
				WITH this
				CALL {
					WITH this
					MATCH (this)-[this0:ACTED_IN]->(this1:Movie)
					WITH {
						properties: {
							screenTime: this0.screenTime
						},
						node: {
							__resolveType: 'Movie',
								title: this1.title
						}
					} AS edge
					RETURN edge
					UNION
					WITH this
					MATCH (this)-[this2:ACTED_IN]->(this3:Series)
					WITH {
						properties: {
							screenTime: this2.screenTime
						},
						node: {
							__resolveType: 'Series',
								episodes: this3.episodes,
								title: this3.title
						}
					} AS edge
					RETURN edge
				}
				WITH collect(edge) AS edges
				RETURN {
					edges: edges
				} AS var4
			}
			RETURN this {
				.name,
				actedInConnection: var4
			} AS this
			""";
		var renderer = Renderer.getRenderer(Configuration.newConfig()
			.withPrettyPrint(true)
			.withGeneratedNames(true)
			.build());
		var normalized = renderer.render(CypherParser.parse(cypher));

		assertThat(normalized).isEqualTo("""
			MATCH (v0:Actor)
			CALL {
			  WITH v0
			  CALL {
			    WITH v0
			    MATCH (v0)-[v1:ACTED_IN]->(v2:Movie)
			    WITH {
			      properties: {
			        screenTime: v1.screenTime
			      },
			      node: {
			        __resolveType: 'Movie',
			        title: v2.title
			      }
			    } AS v3
			    RETURN v3 UNION
			    WITH v0
			    MATCH (v0)-[v4:ACTED_IN]->(v5:Series)
			    WITH {
			      properties: {
			        screenTime: v4.screenTime
			      },
			      node: {
			        __resolveType: 'Series',
			        episodes: v5.episodes,
			        title: v5.title
			      }
			    } AS v6
			    RETURN v6
			  }
			  WITH collect(v6) AS v1
			  RETURN {
			    edges: v1
			  } AS v2
			}
			RETURN v0 {
			  .name,
			  actedInConnection: v2
			} AS v3""");
	}

	@Test
	void projectionsShouldBeSortedToo() {
		var cypher = """
			MATCH (user:User)
			return user {
			   .title,
			   b: 'B',
			   .*,
			   a: 'A'
			 } AS user
			""";

		Statement statement = CypherParser.parse(cypher, Options.newOptions()
			.createSortedMaps(true) // this has no effect on the projection
			.build());

		var renderer = Renderer.getRenderer(Configuration.newConfig()
			.withPrettyPrint(true)
			.withGeneratedNames(true)
			.build());
		var normalized = renderer.render(statement);
		assertThat(normalized).isEqualTo("""
			MATCH (v0:User)
			RETURN v0 {
			  a: 'A',
			  b: 'B',
			  .*,
			  .title
			} AS v1""");
	}
}
