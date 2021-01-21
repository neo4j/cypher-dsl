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

import static org.apiguardian.api.API.Status.EXPERIMENTAL;

import java.util.TimeZone;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.BuiltInFunctions.Aggregates;
import org.neo4j.cypherdsl.core.BuiltInFunctions.Lists;
import org.neo4j.cypherdsl.core.BuiltInFunctions.Predicates;
import org.neo4j.cypherdsl.core.BuiltInFunctions.Scalars;
import org.neo4j.cypherdsl.core.BuiltInFunctions.Spatials;
import org.neo4j.cypherdsl.core.BuiltInFunctions.Strings;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * Factory methods for creating instances of {@link FunctionInvocation functions}.
 *
 * @author Michael J. Simons
 * @author Gerrit Meier
 * @author Romain Rossi
 * @since 1.0
 */
@API(status = EXPERIMENTAL, since = "1.0")
public final class Functions {

	/**
	 * Creates a function invocation for {@code id{}}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-id">id</a>.
	 *
	 * @param node The node for which the internal id should be retrieved
	 * @return A function call for {@code id()} on a node.
	 */
	public static FunctionInvocation id(Node node) {

		Assertions.notNull(node, "The node for id() is required.");

		return FunctionInvocation.create(Scalars.ID, node.getRequiredSymbolicName());
	}

	/**
	 * Creates a function invocation for {@code id{}}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-id">id</a>.
	 *
	 * @param relationship The relationship for which the internal id should be retrieved
	 * @return A function call for {@code id()} on a relationship.
	 */
	public static FunctionInvocation id(Relationship relationship) {

		Assertions.notNull(relationship, "The relationship for id() is required.");

		return FunctionInvocation.create(Scalars.ID, relationship.getRequiredSymbolicName());
	}

	/**
	 * Creates a function invocation for {@code labels{}}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/list/#functions-labels">labels</a>.
	 *
	 * @param node The node for which the labels should be retrieved
	 * @return A function call for {@code labels()} on a node.
	 */
	public static FunctionInvocation labels(Node node) {

		Assertions.notNull(node, "The node parameter is required.");

		return FunctionInvocation.create(Lists.LABELS, node.getRequiredSymbolicName());
	}

	/**
	 * Creates a function invocation for {@code type{}}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-type">type</a>.
	 *
	 * @param relationship The relationship for which the type should be retrieved
	 * @return A function call for {@code type()} on a relationship.
	 */
	public static FunctionInvocation type(Relationship relationship) {

		Assertions.notNull(relationship, "The relationship parameter is required.");

		return FunctionInvocation.create(Scalars.TYPE, relationship.getRequiredSymbolicName());
	}

	/**
	 * @param node The named node to be counted
	 * @return A function call for {@code count()} for one named node
	 * @see #count(Expression)
	 */
	public static FunctionInvocation count(Node node) {

		Assertions.notNull(node, "The node parameter is required.");

		return FunctionInvocation.create(Aggregates.COUNT, node.getRequiredSymbolicName());
	}

	/**
	 * Creates a function invocation for the {@code count()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-count">count</a>.
	 *
	 * @param expression An expression describing the things to count.
	 * @return A function call for {@code count()} for an expression like {@link Cypher#asterisk()} etc.
	 */
	public static FunctionInvocation count(Expression expression) {

		return FunctionInvocation.create(Aggregates.COUNT, expression);
	}

	/**
	 * Creates a function invocation for a {@code count()} function with {@code DISTINCT} added.
	 *
	 * @param node The named node to be counted
	 * @return A function call for {@code count()} for one named node
	 * @see #countDistinct(Expression)
	 */
	public static FunctionInvocation countDistinct(Node node) {

		Assertions.notNull(node, "The node parameter is required.");

		return FunctionInvocation.createDistinct(Aggregates.COUNT, node.getRequiredSymbolicName());
	}

	/**
	 * Creates a function invocation for a {@code count()} function with {@code DISTINCT} added.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-count">count</a>.
	 *
	 * @param expression An expression describing the things to count.
	 * @return A function call for {@code count()} for an expression like {@link Cypher#asterisk()} etc.
	 */
	public static FunctionInvocation countDistinct(Expression expression) {

		return FunctionInvocation.createDistinct(Aggregates.COUNT, expression);
	}

	/**
	 * Creates a function invocation for {@code properties())} on nodes.
	 *
	 * @param node The node who's properties should be returned.
	 * @return A function call for {@code properties())}
	 */
	public static FunctionInvocation properties(Node node) {

		return FunctionInvocation.create(Scalars.PROPERTIES, node.getRequiredSymbolicName());
	}

	/**
	 * Creates a function invocation for {@code properties())} on relationships.
	 *
	 * @param relationship The relationship who's properties should be returned.
	 * @return A function call for {@code properties())}
	 */
	public static FunctionInvocation properties(Relationship relationship) {

		return FunctionInvocation.create(Scalars.PROPERTIES, relationship.getRequiredSymbolicName());
	}

	/**
	 * Creates a function invocation for {@code properties())} on maps.
	 *
	 * @param map The map who's properties should be returned.
	 * @return A function call for {@code properties())}
	 */
	public static FunctionInvocation properties(MapExpression map) {

		return FunctionInvocation.create(Scalars.PROPERTIES, map);
	}

	/**
	 * Creates a function invocation for the {@code coalesce()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-coalesce">coalesce</a>.
	 *
	 * @param expressions One or more expressions to be coalesced
	 * @return A function call for {@code coalesce}.
	 */
	public static FunctionInvocation coalesce(Expression... expressions) {

		return FunctionInvocation.create(Scalars.COALESCE, expressions);
	}

	/**
	 * Creates a function invocation for the {@code toLower()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-toLower">toLower</a>.
	 *
	 * @param expression An expression resolving to a string
	 * @return A function call for {@code toLower()} for one expression
	 */
	public static FunctionInvocation toLower(Expression expression) {

		return FunctionInvocation.create(Strings.TO_LOWER, expression);
	}

	/**
	 * Creates a function invocation for the {@code size()} function. {@code size} can be applied to
	 * <ul>
	 * <li><a href="https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-size">a list</a></li>
	 * <li><a href="https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-size-of-string">to a string</a></li>
	 * </ul>
	 *
	 * @param expression The expression who's size is to be returned
	 * @return A function call for {@code size()} for one expression
	 */
	public static FunctionInvocation size(Expression expression) {

		return FunctionInvocation.create(Scalars.SIZE, expression);
	}

	/**
	 * Creates a function invocation for the {@code size()} function. {@code size} can be applied to
	 * <ul>
	 * <li><a href="https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-size-of-pattern-expression">to a pattern expression</a></li>
	 * </ul>
	 *
	 * @param pattern The pattern for which {@code size()} should be invoked.
	 * @return A function call for {@code size()} for a pattern
	 */
	public static FunctionInvocation size(RelationshipPattern pattern) {

		return FunctionInvocation.create(Scalars.SIZE, pattern);
	}

	/**
	 * Creates a function invocation for the {@code exists()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/predicate/#functions-exists">exists</a>.
	 *
	 * @param expression The expression who's existence is to be evaluated
	 * @return A function call for {@code exists()} for one expression
	 */
	public static FunctionInvocation exists(Expression expression) {

		return FunctionInvocation.create(Predicates.EXISTS, expression);
	}

	/**
	 * Creates a function invocation for the {@code distance()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/spatial/#functions-distance">exists</a>.
	 * Both points need to be in the same coordinate system.
	 *
	 * @param point1 Point 1
	 * @param point2 Point 2
	 * @return A function call for {@code distance()}
	 */
	public static FunctionInvocation distance(Expression point1, Expression point2) {

		Assertions.notNull(point1, "The distance function requires two points.");
		Assertions.notNull(point2, "The distance function requires two points.");

		return FunctionInvocation.create(Spatials.DISTANCE, point1, point2);
	}

	/**
	 * Creates a function invocation for the {@code point()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/spatial/#functions-point">point</a>.
	 *
	 * @param parameterMap The map of parameters for {@code point()}
	 * @return A function call for {@code point()}
	 */
	public static FunctionInvocation point(MapExpression parameterMap) {

		return FunctionInvocation.create(Spatials.POINT, parameterMap);
	}

	/**
	 * Creates a function invocation for the {@code point()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/spatial/#functions-point">point</a>.
	 *
	 * @param parameter A parameter referencing a {@code point()}
	 * @return A function call for {@code point()}
	 */
	public static FunctionInvocation point(Parameter parameter) {

		return FunctionInvocation.create(Spatials.POINT, parameter);
	}

	/**
	 * Creates a function invocation for the {@code avg()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-avg">avg</a>.
	 *
	 * @param expression The things to average
	 * @return A function call for {@code avg()}
	 */
	public static FunctionInvocation avg(Expression expression) {

		return FunctionInvocation.create(Aggregates.AVG, expression);
	}

	/**
	 * Creates a function invocation for the {@code avg()} function with {@code DISTINCT} added.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-avg">avg</a>.
	 *
	 * @param expression The things to average
	 * @return A function call for {@code avg()}
	 */
	public static FunctionInvocation avgDistinct(Expression expression) {

		return FunctionInvocation.createDistinct(Aggregates.AVG, expression);
	}

	/**
	 * Creates a function invocation for the {@code collect()} function.
	 *
	 * @param variable The named thing to collect
	 * @return A function call for {@code collect()}
	 * @see #collect(Expression)
	 */
	public static FunctionInvocation collect(Named variable) {

		Assertions.notNull(variable, "The variable parameter is required.");

		return FunctionInvocation.create(Aggregates.COLLECT, variable.getRequiredSymbolicName());
	}

	/**
	 * Creates a function invocation for the {@code collect()} function with {@code DISTINCT} added.
	 *
	 * @param variable The named thing to collect
	 * @return A function call for {@code collect()}
	 * @see #collect(Expression)
	 */
	public static FunctionInvocation collectDistinct(Named variable) {

		Assertions.notNull(variable, "The variable parameter is required.");

		return FunctionInvocation.createDistinct(Aggregates.COLLECT, variable.getRequiredSymbolicName());
	}

	/**
	 * Creates a function invocation for the {@code collect()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-collect">collect</a>.
	 *
	 * @param expression The things to collect
	 * @return A function call for {@code collect()}
	 */
	public static FunctionInvocation collect(Expression expression) {

		return FunctionInvocation.create(Aggregates.COLLECT, expression);
	}

	/**
	 * Creates a function invocation for the {@code collect()} function with {@code DISTINCT} added.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-collect">collect</a>.
	 *
	 * @param expression The things to collect
	 * @return A function call for {@code collect()}
	 */
	public static FunctionInvocation collectDistinct(Expression expression) {

		return FunctionInvocation.createDistinct(Aggregates.COLLECT, expression);
	}

	/**
	 * Creates a function invocation for the {@code max()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-max">max</a>.
	 *
	 * @param expression A list from which the maximum element value is returned
	 * @return A function call for {@code max()}
	 */
	public static FunctionInvocation max(Expression expression) {

		return FunctionInvocation.create(Aggregates.MAX, expression);
	}

	/**
	 * Creates a function invocation for the {@code max()} function with {@code DISTINCT} added.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-max">max</a>.
	 *
	 * @param expression A list from which the maximum element value is returned
	 * @return A function call for {@code max()}
	 */
	public static FunctionInvocation maxDistinct(Expression expression) {

		return FunctionInvocation.createDistinct(Aggregates.MAX, expression);
	}

	/**
	 * Creates a function invocation for the {@code min()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-min">min</a>.
	 *
	 * @param expression A list from which the minimum element value is returned
	 * @return A function call for {@code min()}
	 */
	public static FunctionInvocation min(Expression expression) {

		return FunctionInvocation.create(Aggregates.MIN, expression);
	}

	/**
	 * Creates a function invocation for the {@code min()} function with {@code DISTINCT} added.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-min">min</a>.
	 *
	 * @param expression A list from which the minimum element value is returned
	 * @return A function call for {@code min()}
	 */
	public static FunctionInvocation minDistinct(Expression expression) {

		return FunctionInvocation.createDistinct(Aggregates.MIN, expression);
	}

	private static void assertPercentileArguments(Aggregates builtIn, Expression expression, Number percentile) {
		Assertions
			.notNull(expression, "The numeric expression for " + builtIn.getImplementationName() + " is required.");
		Assertions.notNull(percentile, "The percentile for " + builtIn.getImplementationName() + " is required.");
		final double p = percentile.doubleValue();
		Assertions.isTrue(p >= 0D && p <= 1D,
			"The percentile for " + builtIn.getImplementationName() + " must be between 0.0 and 1.0.");
	}

	/**
	 * Creates a function invocation for the {@code percentileCont()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-percentilecont">percentileCont</a>.
	 *
	 * @param expression A numeric expression
	 * @param percentile A numeric value between 0.0 and 1.0
	 * @return A function call for {@code percentileCont()}
	 */
	public static FunctionInvocation percentileCont(Expression expression, Number percentile) {

		assertPercentileArguments(Aggregates.PERCENTILE_CONT, expression, percentile);

		return FunctionInvocation.create(Aggregates.PERCENTILE_CONT, expression, new NumberLiteral(percentile));
	}

	/**
	 * Creates a function invocation for the {@code percentileCont()} function with {@code DISTINCT} added.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-percentilecont">percentileCont</a>.
	 *
	 * @param expression A numeric expression
	 * @param percentile A numeric value between 0.0 and 1.0
	 * @return A function call for {@code percentileCont()}
	 */
	public static FunctionInvocation percentileContDistinct(Expression expression, Number percentile) {

		assertPercentileArguments(Aggregates.PERCENTILE_CONT, expression, percentile);

		return FunctionInvocation.createDistinct(Aggregates.PERCENTILE_CONT, expression, new NumberLiteral(percentile));
	}

	/**
	 * Creates a function invocation for the {@code percentileDisc()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-percentiledisc">percentileDisc</a>.
	 *
	 * @param expression A numeric expression
	 * @param percentile A numeric value between 0.0 and 1.0
	 * @return A function call for {@code percentileDisc()}
	 */
	public static FunctionInvocation percentileDisc(Expression expression, Number percentile) {

		assertPercentileArguments(Aggregates.PERCENTILE_DISC, expression, percentile);

		return FunctionInvocation.create(Aggregates.PERCENTILE_DISC, expression, new NumberLiteral(percentile));
	}

	/**
	 * Creates a function invocation for the {@code percentileDisc()} function with {@code DISTINCT} added.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-percentiledisc">percentileDisc</a>.
	 *
	 * @param expression A numeric expression
	 * @param percentile A numeric value between 0.0 and 1.0
	 * @return A function call for {@code percentileDisc()}
	 */
	public static FunctionInvocation percentileDiscDistinct(Expression expression, Number percentile) {

		assertPercentileArguments(Aggregates.PERCENTILE_DISC, expression, percentile);

		return FunctionInvocation.createDistinct(Aggregates.PERCENTILE_DISC, expression, new NumberLiteral(percentile));
	}

	/**
	 * Creates a function invocation for the {@code stDev()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-stdev">stDev</a>.
	 *
	 * @param expression A numeric expression
	 * @return A function call for {@code stDev()}
	 */
	public static FunctionInvocation stDev(Expression expression) {

		return FunctionInvocation.create(Aggregates.ST_DEV, expression);
	}

	/**
	 * Creates a function invocation for the {@code stDev()} function with {@code DISTINCT} added.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-stdev">stDev</a>.
	 *
	 * @param expression A numeric expression
	 * @return A function call for {@code stDev()}
	 */
	public static FunctionInvocation stDevDistinct(Expression expression) {

		return FunctionInvocation.createDistinct(Aggregates.ST_DEV, expression);
	}

	/**
	 * Creates a function invocation for the {@code stDevP()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-stdevp">stDevP</a>.
	 *
	 * @param expression A numeric expression
	 * @return A function call for {@code stDevP()}
	 */
	public static FunctionInvocation stDevP(Expression expression) {

		return FunctionInvocation.create(Aggregates.ST_DEV_P, expression);
	}

	/**
	 * Creates a function invocation for the {@code stDevP()} function with {@code DISTINCT} added.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-stdevp">stDevP</a>.
	 *
	 * @param expression A numeric expression
	 * @return A function call for {@code stDevP()}
	 */
	public static FunctionInvocation stDevPDistinct(Expression expression) {

		return FunctionInvocation.createDistinct(Aggregates.ST_DEV_P, expression);
	}

	/**
	 * Creates a function invocation for the {@code sum()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-sum">sum</a>.
	 *
	 * @param expression An expression returning a set of numeric values
	 * @return A function call for {@code sum()}
	 */
	public static FunctionInvocation sum(Expression expression) {

		return FunctionInvocation.create(Aggregates.SUM, expression);
	}

	/**
	 * Creates a function invocation for the {@code sum()} function  with {@code DISTINCT} added.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-sum">sum</a>.
	 *
	 * @param expression An expression returning a set of numeric values
	 * @return A function call for {@code sum()}
	 */
	public static FunctionInvocation sumDistinct(Expression expression) {

		return FunctionInvocation.createDistinct(Aggregates.SUM, expression);
	}

	/**
	 * @param start the range's start
	 * @param end   the range's end
	 * @return A function call for {@code range()}
	 * @see #range(Expression, Expression)
	 */
	public static FunctionInvocation range(Integer start, Integer end) {

		return range(Cypher.literalOf(start), Cypher.literalOf(end));
	}

	/**
	 * @param start the range's start
	 * @param end   the range's end
	 * @return A function call for {@code range()}
	 * @see #range(Expression, Expression, Expression)
	 */
	public static FunctionInvocation range(Expression start, Expression end) {
		return range(start, end, null);
	}

	/**
	 * Creates a function invocation for the {@code range()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/list/#functions-range">range</a>.
	 *
	 * @param start the range's start
	 * @param end   the range's end
	 * @param step  the range's step
	 * @return A function call for {@code range()}
	 * @see #range(Expression, Expression, Expression)
	 */
	public static FunctionInvocation range(Integer start, Integer end, Integer step) {

		return range(Cypher.literalOf(start), Cypher.literalOf(end), Cypher.literalOf(step));
	}

	/**
	 * Creates a function invocation for the {@code range()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/list/#functions-range">range</a>.
	 *
	 * @param start the range's start
	 * @param end   the range's end
	 * @param step  the range's step
	 * @return A function call for {@code range()}
	 */
	public static FunctionInvocation range(Expression start, Expression end, Expression step) {

		Assertions.notNull(start, "The expression for range is required.");
		Assertions.notNull(end, "The expression for range is required.");

		if (step == null) {
			return FunctionInvocation.create(Lists.RANGE, start, end);
		} else {
			return FunctionInvocation.create(Lists.RANGE, start, end, step);
		}
	}

	/**
	 * Creates a function invocation for the {@code head()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-head">head</a>.
	 *
	 * @param expression A list from which the head element is returned
	 * @return A function call for {@code head()}
	 */
	public static FunctionInvocation head(Expression expression) {

		return FunctionInvocation.create(Scalars.HEAD, expression);
	}

	/**
	 * Creates a function invocation for the {@code last()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-last">last</a>.
	 *
	 * @param expression A list from which the last element is returned
	 * @return A function call for {@code last()}
	 */
	public static FunctionInvocation last(Expression expression) {

		return FunctionInvocation.create(Scalars.LAST, expression);
	}

	/**
	 * Creates a function invocation for {@code nodes{}}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/list/#functions-nodes">nodes</a>.
	 *
	 * @param path The path for which the number of nodes should be retrieved
	 * @return A function call for {@code nodes()} on a path.
	 * @since 1.1
	 */
	public static FunctionInvocation nodes(NamedPath path) {

		Assertions.notNull(path, "The path for nodes is required.");
		return FunctionInvocation.create(Lists.NODES,
			path.getSymbolicName().orElseThrow(() -> new IllegalArgumentException("The path needs to be named!")));
	}

	/**
	 * Creates a function invocation for {@code nodes{}}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/list/#functions-nodes">nodes</a>.
	 *
	 * @param symbolicName The symbolic name of a path for which the number of nodes should be retrieved
	 * @return A function call for {@code nodes{}} on a path represented by a symbolic name.
	 * @since 2020.1.5
	 */
	public static FunctionInvocation nodes(SymbolicName symbolicName) {

		Assertions.notNull(symbolicName, "The symbolic name of the path for nodes is required.");
		return FunctionInvocation.create(Lists.NODES, symbolicName);
	}

	/**
	 * Creates a function invocation for {@code relationships{}}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/list/#functions-relationships">relationships</a>.
	 *
	 * @param path The path for which the relationships should be retrieved
	 * @return A function call for {@code relationships()} on a path.
	 * @since 2020.0.2
	 */
	public static FunctionInvocation relationships(NamedPath path) {

		Assertions.notNull(path, "The path for relationships is required.");
		return FunctionInvocation.create(Lists.RELATIONSHIPS,
			path.getSymbolicName().orElseThrow(() -> new IllegalArgumentException("The path needs to be named!")));
	}

	/**
	 * Creates a function invocation for {@code relationships{}}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/list/#functions-relationships">relationships</a>.
	 *
	 * @param symbolicName The symbolic name of a path for which the relationships should be retrieved
	 * @return A function call for {@code relationships()} on a path represented by a symbolic name.
	 * @since 2020.1.5
	 */
	public static FunctionInvocation relationships(SymbolicName symbolicName) {

		Assertions.notNull(symbolicName, "The symbolic name of the path for relationships is required.");
		return FunctionInvocation.create(Lists.RELATIONSHIPS, symbolicName);
	}

	/**
	 * Creates a function invocation for {@code startNode{}}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-startnode">startNode</a>.
	 *
	 * @param relationship The relationship for which the start node be retrieved
	 * @return A function call for {@code startNode()} on a path.
	 * @since 2020.0.2
	 */
	public static FunctionInvocation startNode(Relationship relationship) {

		Assertions.notNull(relationship, "The relationship for endNode is required.");
		return FunctionInvocation.create(Scalars.START_NODE,
			relationship.getSymbolicName()
				.orElseThrow(() -> new IllegalArgumentException("The relationship needs to be named!")));
	}

	/**
	 * Creates a function invocation for {@code endNode{}}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-endnode">endNode</a>.
	 *
	 * @param relationship The relationship for which the end node be retrieved
	 * @return A function call for {@code endNode()} on a path.
	 * @since 2020.0.2
	 */
	public static FunctionInvocation endNode(Relationship relationship) {

		Assertions.notNull(relationship, "The relationship for endNode is required.");
		return FunctionInvocation.create(Scalars.END_NODE,
			relationship.getSymbolicName()
				.orElseThrow(() -> new IllegalArgumentException("The relationship needs to be named!")));
	}

	/**
	 * Creates a function invocation for {@code date()}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/date/">date</a>.
	 * This is the most simple form.
	 *
	 * @return A function call for {@code date()}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation date() {

		return FunctionInvocation.create(BuiltInFunctions.Temporals.DATE);
	}

	/**
	 * Creates a function invocation for {@code date({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/date/">date</a>.
	 *
	 * @param year  The year
	 * @param month The month
	 * @param day   The day
	 * @return A function call for {@code date({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation calendarDate(Integer year, Integer month, Integer day) {

		Assertions.notNull(year, "The year is required.");
		Assertions.notNull(month, "The month is required.");
		Assertions.notNull(day, "The year is required.");
		return FunctionInvocation.create(BuiltInFunctions.Temporals.DATE, Cypher
			.mapOf("year", Cypher.literalOf(year), "month", Cypher.literalOf(month), "day", Cypher.literalOf(day)));
	}

	/**
	 * Creates a function invocation for {@code date({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/date/">date</a>.
	 *
	 * @param year      The year
	 * @param week      The optional week
	 * @param dayOfWeek The optional day of the week
	 * @return A function call for {@code date({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation weekDate(Integer year, Integer week, Integer dayOfWeek) {

		Assertions.notNull(year, "The year is required.");
		Object[] parameters = new Object[2 + (week == null ? 0 : 2) + (dayOfWeek == null ? 0 : 2)];
		int i = 0;
		parameters[i++] = "year";
		parameters[i++] = Cypher.literalOf(year);
		if (week != null) {
			parameters[i++] = "week";
			parameters[i++] = Cypher.literalOf(week);
		}
		if (dayOfWeek != null) {
			if (week == null) {
				throw new IllegalArgumentException("week is required when using dayOfWeek.");
			}
			parameters[i++] = "dayOfWeek";
			parameters[i++] = Cypher.literalOf(dayOfWeek);
		}
		return FunctionInvocation.create(BuiltInFunctions.Temporals.DATE, Cypher.mapOf(parameters));
	}

	/**
	 * Creates a function invocation for {@code date({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/date/">date</a>.
	 *
	 * @param year         The year
	 * @param quarter      The optional week
	 * @param dayOfQuarter The optional day of the week
	 * @return A function call for {@code date({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation quarterDate(Integer year, Integer quarter, Integer dayOfQuarter) {

		Assertions.notNull(year, "The year is required.");
		Object[] parameters = new Object[2 + (quarter == null ? 0 : 2) + (dayOfQuarter == null ? 0 : 2)];
		int i = 0;
		parameters[i++] = "year";
		parameters[i++] = Cypher.literalOf(year);
		if (quarter != null) {
			parameters[i++] = "quarter";
			parameters[i++] = Cypher.literalOf(quarter);
		}
		if (dayOfQuarter != null) {
			if (dayOfQuarter == null) {
				throw new IllegalArgumentException("quarter is required when using dayOfQuarter.");
			}
			parameters[i++] = "dayOfQuarter";
			parameters[i++] = Cypher.literalOf(dayOfQuarter);
		}
		return FunctionInvocation.create(BuiltInFunctions.Temporals.DATE, Cypher.mapOf(parameters));
	}

	/**
	 * Creates a function invocation for {@code date({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/date/">date</a>.
	 *
	 * @param year       The year
	 * @param ordinalDay The ordinal day of the year.
	 * @return A function call for {@code date({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation ordinalDate(Integer year, Integer ordinalDay) {

		Assertions.notNull(year, "The year is required.");
		Object[] parameters = new Object[2 + (ordinalDay == null ? 0 : 2)];
		int i = 0;
		parameters[i++] = "year";
		parameters[i++] = Cypher.literalOf(year);
		if (ordinalDay != null) {
			parameters[i++] = "ordinalDay";
			parameters[i++] = Cypher.literalOf(ordinalDay);
		}
		return FunctionInvocation.create(BuiltInFunctions.Temporals.DATE, Cypher.mapOf(parameters));
	}

	/**
	 * Creates a function invocation for {@code date({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/date/">date</a>.
	 * This is the most generic form.
	 *
	 * @param components The map to pass to {@code date({})}
	 * @return A function call for {@code date({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation date(MapExpression components) {

		Assertions.notNull(components, "The components is required.");
		return FunctionInvocation.create(BuiltInFunctions.Temporals.DATE, components);
	}

	/**
	 * Creates a function invocation for {@code date({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/date/">date</a>.
	 * This creates a date from a string.
	 *
	 * @param temporalValue A string representing a temporal value.
	 * @return A function call for {@code date({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation date(String temporalValue) {

		Assertions.hasText(temporalValue, "The temporalValue is required.");
		return FunctionInvocation.create(BuiltInFunctions.Temporals.DATE, Cypher.literalOf(temporalValue));
	}

	/**
	 * Creates a function invocation for {@code date({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/date/">date</a>.
	 * This creates a date from a string.
	 *
	 * @param temporalValue An expression representing a temporal value.
	 * @return A function call for {@code date({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation date(Expression temporalValue) {

		Assertions.notNull(temporalValue, "The temporalValue is required.");
		return FunctionInvocation.create(BuiltInFunctions.Temporals.DATE, temporalValue);
	}

	/**
	 * Creates a function invocation for {@code datetime({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/datetime/">datetime</a>.
	 *
	 * @return A function call for {@code datetime({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation datetime() {

		return FunctionInvocation.create(BuiltInFunctions.Temporals.DATETIME);
	}

	/**
	 * Creates a function invocation for {@code datetime({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/datetime/">datetime</a>.
	 *
	 * @param timeZone The timezone to use when creating the temporal instance
	 * @return A function call for {@code datetime({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation datetime(TimeZone timeZone) {

		Assertions.notNull(timeZone, "The timezone is required.");
		return FunctionInvocation
			.create(BuiltInFunctions.Temporals.DATETIME, timezoneMapLiteralOf(timeZone));
	}

	/**
	 * Creates a function invocation for {@code datetime({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/datetime/">datetime</a>.
	 * This is the most generic form.
	 *
	 * @param components The map to pass to {@code datetime({})}
	 * @return A function call for {@code datetime({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation datetime(MapExpression components) {

		Assertions.notNull(components, "The components is required.");
		return FunctionInvocation.create(BuiltInFunctions.Temporals.DATETIME, components);
	}

	/**
	 * Creates a function invocation for {@code datetime({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/date/">datetime</a>.
	 * This creates a datetime from a string.
	 *
	 * @param temporalValue A string representing a temporal value.
	 * @return A function call for {@code datetime({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation datetime(String temporalValue) {

		Assertions.hasText(temporalValue, "The temporalValue is required.");
		return FunctionInvocation.create(BuiltInFunctions.Temporals.DATETIME, Cypher.literalOf(temporalValue));
	}

	/**
	 * Creates a function invocation for {@code datetime({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/date/">datetime</a>.
	 * This creates a datetime from a string.
	 *
	 * @param temporalValue An expression representing a temporal value.
	 * @return A function call for {@code date({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation datetime(Expression temporalValue) {

		Assertions.notNull(temporalValue, "The temporalValue is required.");
		return FunctionInvocation.create(BuiltInFunctions.Temporals.DATETIME, temporalValue);
	}

	/**
	 * Creates a function invocation for {@code localdatetime({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/localdatetime/">localdatetime</a>.
	 *
	 * @return A function call for {@code localdatetime({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation localdatetime() {

		return FunctionInvocation.create(BuiltInFunctions.Temporals.LOCALDATETIME);
	}

	/**
	 * Creates a function invocation for {@code localdatetime({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/localdatetime/">localdatetime</a>.
	 *
	 * @param timeZone The timezone to use when creating the temporal instance
	 * @return A function call for {@code localdatetime({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation localdatetime(TimeZone timeZone) {

		Assertions.notNull(timeZone, "The timezone is required.");
		return FunctionInvocation
			.create(BuiltInFunctions.Temporals.LOCALDATETIME, timezoneMapLiteralOf(timeZone));
	}

	/**
	 * Creates a function invocation for {@code localdatetime({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/localdatetime/">localdatetime</a>.
	 * This is the most generic form.
	 *
	 * @param components The map to pass to {@code localdatetime({})}
	 * @return A function call for {@code localdatetime({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation localdatetime(MapExpression components) {

		Assertions.notNull(components, "The components is required.");
		return FunctionInvocation.create(BuiltInFunctions.Temporals.LOCALDATETIME, components);
	}

	/**
	 * Creates a function invocation for {@code localdatetime({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/localdatetime/">localdatetime</a>.
	 * This creates a localdatetime from a string.
	 *
	 * @param temporalValue A string representing a temporal value.
	 * @return A function call for {@code localdatetime({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation localdatetime(String temporalValue) {

		Assertions.hasText(temporalValue, "The temporalValue is required.");
		return FunctionInvocation.create(BuiltInFunctions.Temporals.LOCALDATETIME, Cypher.literalOf(temporalValue));
	}

	/**
	 * Creates a function invocation for {@code localdatetime({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/localdatetime/">localdatetime</a>.
	 * This creates a localdatetime from a string.
	 *
	 * @param temporalValue An expression representing a temporal value.
	 * @return A function call for {@code localdatetime({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation localdatetime(Expression temporalValue) {

		Assertions.notNull(temporalValue, "The temporalValue is required.");
		return FunctionInvocation.create(BuiltInFunctions.Temporals.LOCALDATETIME, temporalValue);
	}

	/**
	 * Creates a function invocation for {@code localtime({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/localdatetime/">localtime</a>.
	 *
	 * @return A function call for {@code localtime({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation localtime() {

		return FunctionInvocation.create(BuiltInFunctions.Temporals.LOCALTIME);
	}

	/**
	 * Creates a function invocation for {@code localtime({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/localtime/">localtime</a>.
	 *
	 * @param timeZone The timezone to use when creating the temporal instance
	 * @return A function call for {@code localtime({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation localtime(TimeZone timeZone) {

		Assertions.notNull(timeZone, "The timezone is required.");
		return FunctionInvocation
			.create(BuiltInFunctions.Temporals.LOCALTIME, timezoneMapLiteralOf(timeZone));
	}

	/**
	 * Creates a function invocation for {@code localtime({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/localdatetime/">localtime</a>.
	 * This is the most generic form.
	 *
	 * @param components The map to pass to {@code localtime({})}
	 * @return A function call for {@code localtime({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation localtime(MapExpression components) {

		Assertions.notNull(components, "The components is required.");
		return FunctionInvocation.create(BuiltInFunctions.Temporals.LOCALTIME, components);
	}

	/**
	 * Creates a function invocation for {@code localtime({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/localtime/">localtime</a>.
	 * This creates a localtime from a string.
	 *
	 * @param temporalValue A string representing a temporal value.
	 * @return A function call for {@code localtime({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation localtime(String temporalValue) {

		Assertions.hasText(temporalValue, "The temporalValue is required.");
		return FunctionInvocation.create(BuiltInFunctions.Temporals.LOCALTIME, Cypher.literalOf(temporalValue));
	}

	/**
	 * Creates a function invocation for {@code localtime({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/localtime/">localtime</a>.
	 * This creates a localtime from a string.
	 *
	 * @param temporalValue An expression representing a temporal value.
	 * @return A function call for {@code localtime({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation localtime(Expression temporalValue) {

		Assertions.notNull(temporalValue, "The temporalValue is required.");
		return FunctionInvocation.create(BuiltInFunctions.Temporals.LOCALTIME, temporalValue);
	}

	/**
	 * Creates a function invocation for {@code time({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/time/">time</a>.
	 *
	 * @return A function call for {@code time({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation time() {

		return FunctionInvocation.create(BuiltInFunctions.Temporals.TIME);
	}

	/**
	 * Creates a function invocation for {@code time({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/time/">time</a>.
	 *
	 * @param timeZone The timezone to use when creating the temporal instance
	 * @return A function call for {@code time({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation time(TimeZone timeZone) {

		Assertions.notNull(timeZone, "The timezone is required.");
		return FunctionInvocation
			.create(BuiltInFunctions.Temporals.TIME, timezoneMapLiteralOf(timeZone));
	}

	private static MapExpression timezoneMapLiteralOf(TimeZone timeZone) {
		return Cypher.mapOf("timezone", Cypher.literalOf(timeZone.getID()));
	}

	/**
	 * Creates a function invocation for {@code time({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/time/">time</a>.
	 * This is the most generic form.
	 *
	 * @param components The map to pass to {@code time({})}
	 * @return A function call for {@code time({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation time(MapExpression components) {

		Assertions.notNull(components, "The components is required.");
		return FunctionInvocation.create(BuiltInFunctions.Temporals.TIME, components);
	}

	/**
	 * Creates a function invocation for {@code time({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/time/">time</a>.
	 * This creates a time from a string.
	 *
	 * @param temporalValue A string representing a temporal value.
	 * @return A function call for {@code time({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation time(String temporalValue) {

		Assertions.hasText(temporalValue, "The temporalValue is required.");
		return FunctionInvocation.create(BuiltInFunctions.Temporals.TIME, Cypher.literalOf(temporalValue));
	}

	/**
	 * Creates a function invocation for {@code time({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/time/">time</a>.
	 * This creates a time from a string.
	 *
	 * @param temporalValue An expression representing a temporal value.
	 * @return A function call for {@code time({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation time(Expression temporalValue) {

		Assertions.notNull(temporalValue, "The temporalValue is required.");
		return FunctionInvocation.create(BuiltInFunctions.Temporals.TIME, temporalValue);
	}

	/**
	 * Creates a function invocation for {@code duration({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/duration/">duration</a>.
	 * This is the most generic form.
	 *
	 * @param components The map to pass to {@code duration({})}
	 * @return A function call for {@code duration({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation duration(MapExpression components) {

		Assertions.notNull(components, "The components is required.");
		return FunctionInvocation.create(BuiltInFunctions.Temporals.DURATION, components);
	}

	/**
	 * Creates a function invocation for {@code duration({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/duration/">duration</a>.
	 * This creates a duration from a string.
	 *
	 * @param temporalAmount A string representing a temporal amount.
	 * @return A function call for {@code duration({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation duration(String temporalAmount) {

		Assertions.hasText(temporalAmount, "The temporalAmount is required.");
		return FunctionInvocation.create(BuiltInFunctions.Temporals.DURATION, Cypher.literalOf(temporalAmount));
	}

	/**
	 * Creates a function invocation for {@code duration({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/duration/">duration</a>.
	 * This creates a duration from a string.
	 *
	 * @param temporalAmount An expression representing a temporal amount.
	 * @return A function call for {@code duration({})}.
	 * @since 2020.1.0
	 */
	public static FunctionInvocation duration(Expression temporalAmount) {

		Assertions.notNull(temporalAmount, "The temporalAmount is required.");
		return FunctionInvocation.create(BuiltInFunctions.Temporals.DURATION, temporalAmount);
	}

	/**
	 * Creates a function invocation for {@code shortestPath({})}.
	 *
	 * @param relationship The relationship to be passed to {@code shortestPath}.
	 * @return A function call for {@code shortestPath({})}.
	 * @since 2020.0.0
	 */
	public static FunctionInvocation shortestPath(Relationship relationship) {

		return FunctionInvocation.create(Scalars.SHORTEST_PATH, relationship);
	}

	/**
	 * Starts building a function invocation for {@code reduce({})}.
	 *
	 * @param variable The closure will have a variable introduced in its context. We decide here which variable to use.
	 * @return An ongoing definition for a function call to {@code reduce({})}.
	 * @since 2020.1.5
	 */
	public static Reduction.OngoingDefinitionWithVariable reduce(SymbolicName variable) {

		return Reduction.of(variable);
	}
}
