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

import java.util.Arrays;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.annotations.CheckReturnValue;
import org.neo4j.cypherdsl.core.utils.Assertions;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * A step exposing a several {@code using} methods that are provide entry points of adding
 * advanced query hints. Read more about that topic here:
 * <a href="https://neo4j.com/docs/cypher-manual/current/query-tuning/using/">Planner
 * hints and the USING keyword</a>. Note that those hints are specific to Neo4j and not
 * part of openCypher.
 *
 * @author Michael J. Simons
 * @since 2021.0.0
 */
@API(status = STABLE, since = "2021.0.0")
public interface ExposesHints {

	/**
	 * Applies an INDEX hint for one or more properties.
	 * <p>
	 * Index hints are used to specify which index, if any, the planner should use as a
	 * starting point. This can be beneficial in cases where the index statistics are not
	 * accurate for the specific values that the query at hand is known to use, which
	 * would result in the planner picking a non-optimal index.
	 * <p>
	 * Read more about SCAN hints <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/query-tuning/using/#query-using-index-hint">here</a>.
	 * @param properties one or properties that makes up the index. The properties must
	 * belong to the same node.
	 * @return a statement using an INDEX hint.
	 */
	@CheckReturnValue
	StatementBuilder.OngoingReadingWithoutWhere usingIndex(Property... properties);

	/**
	 * Applies an INDEX SEEL hint for one or more properties.
	 * <p>
	 * Index hints are used to specify which index, if any, the planner should use as a
	 * starting point. This can be beneficial in cases where the index statistics are not
	 * accurate for the specific values that the query at hand is known to use, which
	 * would result in the planner picking a non-optimal index.
	 * <p>
	 * Read more about SCAN hints <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/query-tuning/using/#query-using-index-hint">here</a>.
	 * @param properties one or properties that makes up the index. The properties must
	 * belong to the same node.
	 * @return a statement using an INDEX SEEK hint.
	 */
	@CheckReturnValue
	StatementBuilder.OngoingReadingWithoutWhere usingIndexSeek(Property... properties);

	/**
	 * Applies a SCAN hint on a node.
	 * <p>
	 * If your query matches large parts of an index, it might be faster to scan the label
	 * and filter out nodes that do not match.
	 * <p>
	 * Read more about SCAN hints <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/query-tuning/using/#query-using-scan-hint">here</a>.
	 * @param node the node that should be scanned
	 * @return a statement using a SCAN hint.
	 */
	@CheckReturnValue
	StatementBuilder.OngoingReadingWithoutWhere usingScan(Node node);

	/**
	 * Applies a JOIN hint on one or more nodes.
	 * <p>
	 * Join hints are the most advanced type of hints, and are not used to find starting
	 * points for the query execution plan, but to enforce that joins are made at
	 * specified points. This implies that there has to be more than one starting point
	 * (leaf) in the plan, in order for the query to be able to join the two branches
	 * ascending from these leaves.
	 * <p>
	 * Read more about JOIN hints <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/query-tuning/using/#query-using-join-hint">here</a>.
	 * @param nodes the nodes on which a join should be started.
	 * @return a statement using a JOIN hint.
	 */
	@CheckReturnValue
	default StatementBuilder.OngoingReadingWithoutWhere usingJoinOn(Node... nodes) {

		Assertions.notEmpty(nodes, "At least one node is required to define a JOIN hint.");
		return this.usingJoinOn(Arrays.stream(nodes).map(Node::getRequiredSymbolicName).toArray(SymbolicName[]::new));
	}

	/**
	 * Applies a JOIN hint on one or more nodes identified by their names.
	 * <p>
	 * Join hints are the most advanced type of hints, and are not used to find starting
	 * points for the query execution plan, but to enforce that joins are made at
	 * specified points. This implies that there has to be more than one starting point
	 * (leaf) in the plan, in order for the query to be able to join the two branches
	 * ascending from these leaves.
	 * <p>
	 * Read more about JOIN hints <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/query-tuning/using/#query-using-join-hint">here</a>.
	 * @param names the symbolic names identifying the nodes.
	 * @return a statement using a JOIN hint.
	 */
	@CheckReturnValue
	StatementBuilder.OngoingReadingWithoutWhere usingJoinOn(SymbolicName... names);

}
