/*
 * Copyright (c) 2019-2020 "Neo4j,"
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

import static org.neo4j.cypherdsl.core.Cypher.*;

import java.util.stream.Stream;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.neo4j.cypherdsl.core.renderer.Renderer;

/**
 * @author Michael J. Simons
 */
class FunctionsIT {

	private static final Renderer cypherRenderer = Renderer.getDefaultRenderer();

	@ParameterizedTest(name = "{0}")
	@MethodSource("functionsToTest")
	void functionShouldBeRenderedAsExpected(FunctionInvocation functionInvocation, String expected) {

		Assertions.assertThat(cypherRenderer.render(Cypher.returning(functionInvocation).build())).isEqualTo(expected);
	}

	private static Stream<Arguments> functionsToTest() {
		Node n = Cypher.node("Node").named("n");
		Node m = Cypher.node("Node2").named("m");
		Relationship r = n.relationshipTo(m).named("r");
		Expression e1 = Cypher.name("e1");
		Expression e2 = Cypher.name("e2");
		FunctionInvocation p1 = Functions.point(Cypher.mapOf("latitude", literalOf(1), "longitude", literalOf(2)));
		FunctionInvocation p2 = Functions.point(Cypher.mapOf("latitude", literalOf(3), "longitude", literalOf(4)));

		// NOTE: Not all of those return valid Cypher statements. They are used only for integration testing the function calls so far.
		return Stream.of(
			Arguments.of(Functions.id(n), "RETURN id(n)"),
			Arguments.of(Functions.id(r), "RETURN id(r)"),
			Arguments.of(Functions.labels(n), "RETURN labels(n)"),
			Arguments.of(Functions.type(r), "RETURN type(r)"),
			Arguments.of(Functions.count(n), "RETURN count(n)"),
			Arguments.of(Functions.countDistinct(n), "RETURN count(DISTINCT n)"),
			Arguments.of(Functions.count(e1), "RETURN count(e1)"),
			Arguments.of(Functions.countDistinct(e1), "RETURN count(DISTINCT e1)"),
			Arguments.of(Functions.coalesce(e1, e2), "RETURN coalesce(e1, e2)"),
			Arguments.of(Functions.toLower(e1), "RETURN toLower(e1)"),
			Arguments.of(Functions.size(e1), "RETURN size(e1)"),
			Arguments.of(Functions.size(r), "RETURN size((n:`Node`)-[r]->(m:`Node2`))"),
			Arguments.of(Functions.exists(e1), "RETURN exists(e1)"),
			Arguments.of(Functions.distance(p1, p2),
				"RETURN distance(point({latitude: 1, longitude: 2}), point({latitude: 3, longitude: 4}))"),
			Arguments.of(Functions.avg(e1), "RETURN avg(e1)"),
			Arguments.of(Functions.avgDistinct(e1), "RETURN avg(DISTINCT e1)"),
			Arguments.of(Functions.collect(e1), "RETURN collect(e1)"),
			Arguments.of(Functions.collectDistinct(e1), "RETURN collect(DISTINCT e1)"),
			Arguments.of(Functions.collect(n), "RETURN collect(n)"),
			Arguments.of(Functions.collectDistinct(n), "RETURN collect(DISTINCT n)"),
			Arguments.of(Functions.max(e1), "RETURN max(e1)"),
			Arguments.of(Functions.maxDistinct(e1), "RETURN max(DISTINCT e1)"),
			Arguments.of(Functions.min(e1), "RETURN min(e1)"),
			Arguments.of(Functions.minDistinct(e1), "RETURN min(DISTINCT e1)"),
			Arguments.of(Functions.percentileCont(e1, 0.4), "RETURN percentileCont(e1, 0.4)"),
			Arguments.of(Functions.percentileContDistinct(e1, 0.4), "RETURN percentileCont(DISTINCT e1, 0.4)"),
			Arguments.of(Functions.percentileDisc(e1, 0.4), "RETURN percentileDisc(e1, 0.4)"),
			Arguments.of(Functions.percentileDiscDistinct(e1, 0.4), "RETURN percentileDisc(DISTINCT e1, 0.4)"),
			Arguments.of(Functions.stDev(e1), "RETURN stDev(e1)"),
			Arguments.of(Functions.stDevDistinct(e1), "RETURN stDev(DISTINCT e1)"),
			Arguments.of(Functions.stDevP(e1), "RETURN stDevP(e1)"),
			Arguments.of(Functions.stDevPDistinct(e1), "RETURN stDevP(DISTINCT e1)"),
			Arguments.of(Functions.sum(e1), "RETURN sum(e1)"),
			Arguments.of(Functions.sumDistinct(e1), "RETURN sum(DISTINCT e1)"),
			Arguments.of(Functions.range(literalOf(1), literalOf(3)), "RETURN range(1, 3)"),
			Arguments.of(Functions.range(literalOf(1), literalOf(3), literalOf(2)), "RETURN range(1, 3, 2)"),
			Arguments.of(Functions.head(e1), "RETURN head(e1)"),
			Arguments.of(Functions.last(e1), "RETURN last(e1)"),
			Arguments.of(Functions.nodes(Cypher.path("p").definedBy(r)), "RETURN nodes(p)"),
			Arguments.of(Functions.shortestPath(r), "RETURN shortestPath((n:`Node`)-[r]->(m:`Node2`))")
		);
	}
}
