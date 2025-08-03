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

import java.util.TimeZone;

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
final class Functions {

	/**
	 * Creates a function invocation for {@code id{}}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-id">id</a>.
	 *
	 * @param node The node for which the internal id should be retrieved
	 * @return A function call for {@code id()} on a node.
	 * @deprecated see {@link #elementId(Node)} for a replacement. Neo4j the database will remove support for {@code id(n)}
	 * at some point.
	 */
	@Deprecated(since = "2023.3.0")
	@SuppressWarnings({ "squid:S1133" }) // Yes, I promise, this will be removed at some point, but not yet.
	static FunctionInvocation id(Node node) {

		Assertions.notNull(node, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_NODE_REQUIRED));

		return FunctionInvocation.create(Scalars.ID, node.getRequiredSymbolicName());
	}

	/**
	 * Creates a function invocation for {@code id{}}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-id">id</a>.
	 *
	 * @param relationship The relationship for which the internal id should be retrieved
	 * @return A function call for {@code id()} on a relationship.
	 * @deprecated see {@link #elementId(Relationship)} for a replacement. Neo4j the database will remove support for
	 * {@code id(n)} at some point.
	 */
	@Deprecated(since = "2023.3.0")
	@SuppressWarnings({ "squid:S1133" }) // Yes, I promise, this will be removed at some point, but not yet.
	static FunctionInvocation id(Relationship relationship) {

		Assertions.notNull(relationship, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_RELATIONSHIP_REQUIRED));

		return FunctionInvocation.create(Scalars.ID, relationship.getRequiredSymbolicName());
	}

	/**
	 * Creates a function invocation for {@code elementId{}}.
	 *
	 * @param node The node for which the element id should be retrieved
	 * @return A function call for {@code elementId()} on a node.
	 */
	@Neo4jVersion(minimum = "5.0.0")
	static FunctionInvocation elementId(Node node) {

		Assertions.notNull(node, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_NODE_REQUIRED));

		return FunctionInvocation.create(Scalars.ELEMENT_ID, node.getRequiredSymbolicName());
	}

	/**
	 * Creates a function invocation for {@code elementId{}}.
	 *
	 * @param relationship The relationship for which the element id should be retrieved
	 * @return A function call for {@code elementId()} on a relationship.
	 */
	@Neo4jVersion(minimum = "5.0.0")
	static FunctionInvocation elementId(Relationship relationship) {

		Assertions.notNull(relationship, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_RELATIONSHIP_REQUIRED));

		return FunctionInvocation.create(Scalars.ELEMENT_ID, relationship.getRequiredSymbolicName());
	}

	/**
	 * Creates a function invocation for {@code keys{}}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/list/#functions-keys">keys</a>.
	 *
	 * @param node The node which keys should be returned.
	 * @return A function call for {@code keys()} on an expression.
	 * @since 2021.0.2
	 */
	static FunctionInvocation keys(Node node) {

		Assertions.notNull(node, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_NODE_REQUIRED));
		return keys(node.getRequiredSymbolicName());
	}

	/**
	 * Creates a function invocation for {@code keys{}}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/list/#functions-keys">keys</a>.
	 *
	 * @param relationship The relationship which keys should be returned.
	 * @return A function call for {@code keys()} on an expression.
	 * @since 2021.0.2
	 */
	static FunctionInvocation keys(Relationship relationship) {

		Assertions.notNull(relationship, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_RELATIONSHIP_REQUIRED));
		return keys(relationship.getRequiredSymbolicName());
	}

	/**
	 * Creates a function invocation for {@code keys{}}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/list/#functions-keys">keys</a>.
	 *
	 * @param expression The expressions which keys should be returned. Must resolve to a node, relationship or map.
	 * @return A function call for {@code keys()} on an expression.
	 * @since 2021.0.2
	 */
	static FunctionInvocation keys(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));

		Expression param = expression instanceof Named named ? named.getRequiredSymbolicName() : expression;
		return FunctionInvocation.create(Lists.KEYS, param);
	}

	/**
	 * Creates a function invocation for {@code labels{}}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/list/#functions-labels">labels</a>.
	 *
	 * @param node The node for which the labels should be retrieved
	 * @return A function call for {@code labels()} on a node.
	 */
	static FunctionInvocation labels(Node node) {

		Assertions.notNull(node, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_NODE_REQUIRED));

		return labels(node.getRequiredSymbolicName());
	}

	/**
	 * Creates a  function invocation for  {@code labels{}}.  The {@link SymbolicName  symbolic name} {@code  node} must
	 * point to a node. This can't be checked during compile time, so please make sure of that.
	 * <p>
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/list/#functions-labels">labels</a>.
	 *
	 * @param node The node for which the labels should be retrieved
	 * @return A function call for {@code labels()} on a node.
	 * @since 2023.2.0
	 */
	static FunctionInvocation labels(SymbolicName node) {

		Assertions.notNull(node, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_NODE_REQUIRED));

		return FunctionInvocation.create(Lists.LABELS, node);
	}

	/**
	 * Creates a function invocation for {@code type{}}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-type">type</a>.
	 *
	 * @param relationship The relationship for which the type should be retrieved
	 * @return A function call for {@code type()} on a relationship.
	 */
	static FunctionInvocation type(Relationship relationship) {

		Assertions.notNull(relationship, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_RELATIONSHIP_REQUIRED));

		return type(relationship.getRequiredSymbolicName());
	}

	/**
	 * Creates a  function invocation for  {@code type{}}. The {@link  SymbolicName symbolic name}  {@code relationship}
	 * must point to a relationship. This can't be checked during compile time, so please make sure of that.
	 * <p>
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-type">type</a>.
	 *
	 * @param relationship The relationship for which the type should be retrieved
	 * @return A function call for {@code type()} on a relationship.
	 * @since 2023.2.0
	 */
	static FunctionInvocation type(SymbolicName relationship) {

		Assertions.notNull(relationship, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_RELATIONSHIP_REQUIRED));

		return FunctionInvocation.create(Scalars.TYPE, relationship);
	}

	/**
	 * @param node The named node to be counted
	 * @return A function call for {@code count()} for one named node
	 * @see #count(Expression)
	 */
	static FunctionInvocation count(Node node) {

		Assertions.notNull(node, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_NODE_REQUIRED));

		return FunctionInvocation.create(Aggregates.COUNT, node.getRequiredSymbolicName());
	}

	/**
	 * Creates a function invocation for the {@code count()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-count">count</a>.
	 *
	 * @param expression An expression describing the things to count.
	 * @return A function call for {@code count()} for an expression like {@link Cypher#asterisk()} etc.
	 */
	static FunctionInvocation count(Expression expression) {

		return FunctionInvocation.create(Aggregates.COUNT, expression);
	}

	/**
	 * Creates a function invocation for a {@code count()} function with {@code DISTINCT} added.
	 *
	 * @param node The named node to be counted
	 * @return A function call for {@code count()} for one named node
	 * @see #countDistinct(Expression)
	 */
	static FunctionInvocation countDistinct(Node node) {

		Assertions.notNull(node, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_NODE_REQUIRED));

		return FunctionInvocation.createDistinct(Aggregates.COUNT, node.getRequiredSymbolicName());
	}

	/**
	 * Creates a function invocation for a {@code count()} function with {@code DISTINCT} added.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-count">count</a>.
	 *
	 * @param expression An expression describing the things to count.
	 * @return A function call for {@code count()} for an expression like {@link Cypher#asterisk()} etc.
	 */
	static FunctionInvocation countDistinct(Expression expression) {

		return FunctionInvocation.createDistinct(Aggregates.COUNT, expression);
	}

	/**
	 * Creates a function invocation for {@code properties())} on nodes.
	 *
	 * @param node The node who's properties should be returned.
	 * @return A function call for {@code properties())}
	 */
	static FunctionInvocation properties(Node node) {

		Assertions.notNull(node, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_NODE_REQUIRED));

		return FunctionInvocation.create(Scalars.PROPERTIES, node.getRequiredSymbolicName());
	}

	/**
	 * Creates a function invocation for {@code properties())} on relationships.
	 *
	 * @param relationship The relationship who's properties should be returned.
	 * @return A function call for {@code properties())}
	 */
	static FunctionInvocation properties(Relationship relationship) {

		Assertions.notNull(relationship, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_RELATIONSHIP_REQUIRED));

		return FunctionInvocation.create(Scalars.PROPERTIES, relationship.getRequiredSymbolicName());
	}

	/**
	 * Creates a function invocation for {@code properties())} on maps.
	 *
	 * @param map The map who's properties should be returned.
	 * @return A function call for {@code properties())}
	 */
	static FunctionInvocation properties(MapExpression map) {

		return FunctionInvocation.create(Scalars.PROPERTIES, map);
	}

	/**
	 * Creates a function invocation for the {@code coalesce()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-coalesce">coalesce</a>.
	 *
	 * @param expressions One or more expressions to be coalesced
	 * @return A function call for {@code coalesce}.
	 */
	static FunctionInvocation coalesce(Expression... expressions) {

		return FunctionInvocation.create(Scalars.COALESCE, expressions);
	}

	/**
	 * Creates a function invocation for the {@code left()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-left">left</a>.
	 *
	 * @param expression An expression resolving to a string
	 * @param length     desired length
	 * @return A function call for {@code left()}
	 * @since 2023.0.2
	 */
	static FunctionInvocation left(Expression expression, Expression length) {

		if (expression != null && length == null) {
			throw new IllegalArgumentException("length might not be null when the expression is not null");
		}
		return FunctionInvocation.create(Strings.LEFT,
			expressionOrNullLit(expression),
			expressionOrNullLit(length)
		);
	}

	/**
	 * Creates a function invocation for the {@code ltrim()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-ltrim">ltrim</a>.
	 *
	 * @param expression An expression resolving to a string
	 * @return A function call for {@code ltrim()}
	 * @since 2023.0.2
	 */
	static FunctionInvocation ltrim(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(Strings.LTRIM, expressionOrNullLit(expression));
	}

	/**
	 * Creates a function invocation for the {@code replace()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-ltrim">replace</a>.
	 *
	 * @param original An expression that returns a string
	 * @param search   An expression that specifies the string to be replaced in {@code original}.
	 * @param replace  An expression that specifies the replacement string.
	 * @return A function call for {@code replace()}
	 * @since 2023.0.2
	 */
	static FunctionInvocation replace(Expression original, Expression search, Expression replace) {

		return FunctionInvocation.create(Strings.REPLACE, expressionOrNullLit(original), expressionOrNullLit(search),
			expressionOrNullLit(replace));
	}

	/**
	 * Creates a function invocation for the {@code reverse()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-reverse">reverse</a>.
	 *
	 * @param original An expression that returns a string
	 * @return A function call for {@code reverse()}
	 * @since 2023.0.2
	 */
	static FunctionInvocation reverse(Expression original) {

		Assertions.notNull(original, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(Strings.REVERSE, expressionOrNullLit(original));
	}

	/**
	 * Creates a function invocation for the {@code right()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-left">right</a>.
	 *
	 * @param expression An expression resolving to a string
	 * @param length     desired length
	 * @return A function call for {@code right()}
	 * @since 2023.0.2
	 */
	static FunctionInvocation right(Expression expression, Expression length) {

		if (expression != null && length == null) {
			throw new IllegalArgumentException("length might not be null when the expression is not null");
		}
		return FunctionInvocation.create(Strings.RIGHT,
			expressionOrNullLit(expression),
			expressionOrNullLit(length)
		);
	}

	/**
	 * Creates a function invocation for the {@code rtrim()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-rtrim">rtrim</a>.
	 *
	 * @param expression An expression resolving to a string
	 * @return A function call for {@code rtrim()}
	 * @since 2023.0.2
	 */
	static FunctionInvocation rtrim(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(Strings.RTRIM, expressionOrNullLit(expression));
	}

	/**
	 * Creates a function invocation for the {@code substring()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-substring">rtrim</a>.
	 *
	 * @param original An expression resolving to a string
	 * @param start    An expression that returns a positive integer, denoting the position at which the substring will begin.
	 * @param length   An expression that returns a positive integer, denoting how many characters of original will be returned.
	 * @return A function call for {@code substring()}
	 * @since 2023.0.2
	 */
	static FunctionInvocation substring(Expression original, Expression start, Expression length) {

		Assertions.notNull(start, "start is required");
		if (length != null) {
			return FunctionInvocation.create(Strings.SUBSTRING, expressionOrNullLit(original), start, length);
		}
		return FunctionInvocation.create(Strings.SUBSTRING, expressionOrNullLit(original), start);
	}

	private static Expression expressionOrNullLit(Expression expression) {
		return expression == null ? Cypher.literalNull() : expression;
	}

	/**
	 * Creates a function invocation for the {@code toLower()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-toLower">toLower</a>.
	 *
	 * @param expression An expression resolving to a string
	 * @return A function call for {@code toLower()} for one expression
	 */
	static FunctionInvocation toLower(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(Strings.TO_LOWER, expressionOrNullLit(expression));
	}

	/**
	 * Creates a function invocation for the {@code toUpper()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-toUpper">toUpper</a>.
	 *
	 * @param expression An expression resolving to a string
	 * @return A function call for {@code toLower()} for one expression
	 * @since 2023.0.2
	 */
	static FunctionInvocation toUpper(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(Strings.TO_UPPER, expressionOrNullLit(expression));
	}

	/**
	 * Creates a function invocation for the {@code trim()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-trim">trim</a>.
	 *
	 * @param expression An expression resolving to a string
	 * @return A function call for {@code trim()} for one expression
	 * @since 2021.2.1
	 */
	static FunctionInvocation trim(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(Strings.TRIM, expression);
	}

	/**
	 * Creates a function invocation for the {@code split()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-split">split</a>.
	 *
	 * @param expression An expression resolving to a string that should be split
	 * @param delimiter  The delimiter on which to split
	 * @return A function call for {@code split()}
	 * @since 2021.2.1
	 */
	static FunctionInvocation split(Expression expression, Expression delimiter) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		Assertions.notNull(delimiter, "The delimiter is required.");
		return FunctionInvocation.create(Strings.SPLIT, expression, delimiter);
	}

	/**
	 * Creates a function invocation for the {@code split()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-split">split</a>.
	 *
	 * @param expression An expression resolving to a string that should be split
	 * @param delimiter  The delimiter on which to split
	 * @return A function call for {@code split()}
	 * @since 2021.2.1
	 */
	static FunctionInvocation split(Expression expression, String delimiter) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		Assertions.notNull(delimiter, "The delimiter is required.");
		return split(expression, Cypher.literalOf(delimiter));
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
	static FunctionInvocation size(Expression expression) {

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
	static FunctionInvocation size(RelationshipPattern pattern) {

		return FunctionInvocation.create(Scalars.SIZE, pattern);
	}

	/**
	 * Creates a function invocation for the {@code exists()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/predicate/#functions-exists">exists</a>.
	 *
	 * @param expression The expression who's existence is to be evaluated
	 * @return A function call for {@code exists()} for one expression
	 */
	static FunctionInvocation exists(Expression expression) {

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
	static FunctionInvocation distance(Expression point1, Expression point2) {

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
	static FunctionInvocation point(MapExpression parameterMap) {

		return point((Expression) parameterMap);
	}

	/**
	 * Creates a function invocation for the {@code point()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/spatial/#functions-point">point</a>.
	 * <p>
	 * This generic expression variant is useful for referencing a point inside a parameter or another map.
	 *
	 * @param expression An expression resolving to a valid map of parameters for {@code point()}
	 * @return A function call for {@code point()}
	 * @since 2021.0.0
	 */
	static FunctionInvocation point(Expression expression) {

		return FunctionInvocation.create(Spatials.POINT, expression);
	}

	/**
	 * Creates a function invocation for the {@code point()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/spatial/#functions-point">point</a>.
	 *
	 * @param parameter A parameter referencing a {@code point()}
	 * @return A function call for {@code point()}
	 * @since 2022.7.3
	 */
	static FunctionInvocation point(Parameter<?> parameter) {

		return FunctionInvocation.create(Spatials.POINT, parameter);
	}

	/**
	 * Convenience method for creating a 2d cartesian point
	 *
	 * @param x The x coordinate
	 * @param y The y coordinate
	 * @return A function call for {@code point()}
	 * @since 2022.7.3
	 */
	static FunctionInvocation cartesian(double x, double y) {

		return point(Cypher.mapOf("x", Cypher.literalOf(x), "y", Cypher.literalOf(y)));
	}

	/**
	 * Convenience method for creating a 2d coordinate in the WGS 84 coordinate system
	 *
	 * @param longitude The longitude
	 * @param latitude  The latitude
	 * @return A function call for {@code point()}
	 * @since 2022.7.3
	 */
	static FunctionInvocation coordinate(double longitude, double latitude) {

		return point(Cypher.mapOf("longitude", Cypher.literalOf(longitude), "latitude", Cypher.literalOf(latitude)));
	}

	/**
	 * Creates a function invocation for the {@code point.withinBBox} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/spatial/#functions-withinBBox">point.withinBBox</a>.
	 *
	 * @param point      The point to check
	 * @param lowerLeft  The lower left point of the bounding box (south-west coordinate)
	 * @param upperRight The upper right point of the bounding box (north-east coordinate)
	 * @return A function call for {@code point.withinBBox}
	 * @since 2022.7.3
	 */
	static FunctionInvocation withinBBox(Expression point, Expression lowerLeft, Expression upperRight) {

		return FunctionInvocation.create(() -> "point.withinBBox", point, lowerLeft, upperRight);
	}

	/**
	 * Creates a function invocation for the {@code avg()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-avg">avg</a>.
	 *
	 * @param expression The things to average
	 * @return A function call for {@code avg()}
	 */
	static FunctionInvocation avg(Expression expression) {

		return FunctionInvocation.create(Aggregates.AVG, expression);
	}

	/**
	 * Creates a function invocation for the {@code avg()} function with {@code DISTINCT} added.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-avg">avg</a>.
	 *
	 * @param expression The things to average
	 * @return A function call for {@code avg()}
	 */
	static FunctionInvocation avgDistinct(Expression expression) {

		return FunctionInvocation.createDistinct(Aggregates.AVG, expression);
	}

	/**
	 * Creates a function invocation for the {@code collect()} function.
	 *
	 * @param variable The named thing to collect
	 * @return A function call for {@code collect()}
	 * @see #collect(Expression)
	 */
	static FunctionInvocation collect(Named variable) {

		Assertions.notNull(variable, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_VARIABLE_REQUIRED));

		return FunctionInvocation.create(Aggregates.COLLECT, variable.getRequiredSymbolicName());
	}

	/**
	 * Creates a function invocation for the {@code collect()} function with {@code DISTINCT} added.
	 *
	 * @param variable The named thing to collect
	 * @return A function call for {@code collect()}
	 * @see #collect(Expression)
	 */
	static FunctionInvocation collectDistinct(Named variable) {

		Assertions.notNull(variable, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_VARIABLE_REQUIRED));

		return FunctionInvocation.createDistinct(Aggregates.COLLECT, variable.getRequiredSymbolicName());
	}

	/**
	 * Creates a function invocation for the {@code collect()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-collect">collect</a>.
	 *
	 * @param expression The things to collect
	 * @return A function call for {@code collect()}
	 */
	static FunctionInvocation collect(Expression expression) {

		return FunctionInvocation.create(Aggregates.COLLECT, expression);
	}

	/**
	 * Creates a function invocation for the {@code collect()} function with {@code DISTINCT} added.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-collect">collect</a>.
	 *
	 * @param expression The things to collect
	 * @return A function call for {@code collect()}
	 */
	static FunctionInvocation collectDistinct(Expression expression) {

		return FunctionInvocation.createDistinct(Aggregates.COLLECT, expression);
	}

	/**
	 * Creates a function invocation for the {@code max()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-max">max</a>.
	 *
	 * @param expression A list from which the maximum element value is returned
	 * @return A function call for {@code max()}
	 */
	static FunctionInvocation max(Expression expression) {

		return FunctionInvocation.create(Aggregates.MAX, expression);
	}

	/**
	 * Creates a function invocation for the {@code max()} function with {@code DISTINCT} added.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-max">max</a>.
	 *
	 * @param expression A list from which the maximum element value is returned
	 * @return A function call for {@code max()}
	 */
	static FunctionInvocation maxDistinct(Expression expression) {

		return FunctionInvocation.createDistinct(Aggregates.MAX, expression);
	}

	/**
	 * Creates a function invocation for the {@code min()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-min">min</a>.
	 *
	 * @param expression A list from which the minimum element value is returned
	 * @return A function call for {@code min()}
	 */
	static FunctionInvocation min(Expression expression) {

		return FunctionInvocation.create(Aggregates.MIN, expression);
	}

	/**
	 * Creates a function invocation for the {@code min()} function with {@code DISTINCT} added.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-min">min</a>.
	 *
	 * @param expression A list from which the minimum element value is returned
	 * @return A function call for {@code min()}
	 */
	static FunctionInvocation minDistinct(Expression expression) {

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
	static FunctionInvocation percentileCont(Expression expression, Number percentile) {

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
	static FunctionInvocation percentileContDistinct(Expression expression, Number percentile) {

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
	static FunctionInvocation percentileDisc(Expression expression, Number percentile) {

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
	static FunctionInvocation percentileDiscDistinct(Expression expression, Number percentile) {

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
	static FunctionInvocation stDev(Expression expression) {

		return FunctionInvocation.create(Aggregates.ST_DEV, expression);
	}

	/**
	 * Creates a function invocation for the {@code stDev()} function with {@code DISTINCT} added.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-stdev">stDev</a>.
	 *
	 * @param expression A numeric expression
	 * @return A function call for {@code stDev()}
	 */
	static FunctionInvocation stDevDistinct(Expression expression) {

		return FunctionInvocation.createDistinct(Aggregates.ST_DEV, expression);
	}

	/**
	 * Creates a function invocation for the {@code stDevP()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-stdevp">stDevP</a>.
	 *
	 * @param expression A numeric expression
	 * @return A function call for {@code stDevP()}
	 */
	static FunctionInvocation stDevP(Expression expression) {

		return FunctionInvocation.create(Aggregates.ST_DEV_P, expression);
	}

	/**
	 * Creates a function invocation for the {@code stDevP()} function with {@code DISTINCT} added.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-stdevp">stDevP</a>.
	 *
	 * @param expression A numeric expression
	 * @return A function call for {@code stDevP()}
	 */
	static FunctionInvocation stDevPDistinct(Expression expression) {

		return FunctionInvocation.createDistinct(Aggregates.ST_DEV_P, expression);
	}

	/**
	 * Creates a function invocation for the {@code sum()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-sum">sum</a>.
	 *
	 * @param expression An expression returning a set of numeric values
	 * @return A function call for {@code sum()}
	 */
	static FunctionInvocation sum(Expression expression) {

		return FunctionInvocation.create(Aggregates.SUM, expression);
	}

	/**
	 * Creates a function invocation for the {@code sum()} function  with {@code DISTINCT} added.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-sum">sum</a>.
	 *
	 * @param expression An expression returning a set of numeric values
	 * @return A function call for {@code sum()}
	 */
	static FunctionInvocation sumDistinct(Expression expression) {

		return FunctionInvocation.createDistinct(Aggregates.SUM, expression);
	}

	/**
	 * @param start the range's start
	 * @param end   the range's end
	 * @return A function call for {@code range()}
	 * @see #range(Expression, Expression)
	 */
	static FunctionInvocation range(Integer start, Integer end) {

		return range(Cypher.literalOf(start), Cypher.literalOf(end));
	}

	/**
	 * @param start the range's start
	 * @param end   the range's end
	 * @return A function call for {@code range()}
	 * @see #range(Expression, Expression, Expression)
	 */
	static FunctionInvocation range(Expression start, Expression end) {
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
	static FunctionInvocation range(Integer start, Integer end, Integer step) {

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
	static FunctionInvocation range(Expression start, Expression end, Expression step) {

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
	static FunctionInvocation head(Expression expression) {

		return FunctionInvocation.create(Scalars.HEAD, expression);
	}

	/**
	 * Creates a function invocation for the {@code last()} function.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-last">last</a>.
	 *
	 * @param expression A list from which the last element is returned
	 * @return A function call for {@code last()}
	 */
	static FunctionInvocation last(Expression expression) {

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
	static FunctionInvocation nodes(NamedPath path) {

		Assertions.notNull(path, "The path for nodes is required.");
		return FunctionInvocation.create(Lists.NODES,
			path.getSymbolicName().orElseThrow(() -> new IllegalArgumentException(
				Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_NAMED_PATH_REQUIRED))));
	}

	/**
	 * Creates a function invocation for {@code nodes{}}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/list/#functions-nodes">nodes</a>.
	 *
	 * @param symbolicName The symbolic name of a path for which the number of nodes should be retrieved
	 * @return A function call for {@code nodes{}} on a path represented by a symbolic name.
	 * @since 2020.1.5
	 */
	static FunctionInvocation nodes(SymbolicName symbolicName) {

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
	static FunctionInvocation relationships(NamedPath path) {

		Assertions.notNull(path, "The path for relationships is required.");
		return FunctionInvocation.create(Lists.RELATIONSHIPS,
			path.getSymbolicName().orElseThrow(() -> new IllegalArgumentException(
				Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_NAMED_PATH_REQUIRED))));
	}

	/**
	 * Creates a function invocation for {@code relationships{}}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/list/#functions-relationships">relationships</a>.
	 *
	 * @param symbolicName The symbolic name of a path for which the relationships should be retrieved
	 * @return A function call for {@code relationships()} on a path represented by a symbolic name.
	 * @since 2020.1.5
	 */
	static FunctionInvocation relationships(SymbolicName symbolicName) {

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
	static FunctionInvocation startNode(Relationship relationship) {

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
	static FunctionInvocation endNode(Relationship relationship) {

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
	static FunctionInvocation date() {

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
	static FunctionInvocation calendarDate(Integer year, Integer month, Integer day) {

		Assertions.notNull(year, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_YEAR_REQUIRED));
		Assertions.notNull(month, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_MONTH_REQUIRED));
		Assertions.notNull(day, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_DAY_REQUIRED));
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
	static FunctionInvocation weekDate(Integer year, Integer week, Integer dayOfWeek) {

		Assertions.notNull(year, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_YEAR_REQUIRED));
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
			parameters[i] = Cypher.literalOf(dayOfWeek);
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
	static FunctionInvocation quarterDate(Integer year, Integer quarter, Integer dayOfQuarter) {

		Assertions.notNull(year, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_YEAR_REQUIRED));
		Object[] parameters = new Object[2 + (quarter == null ? 0 : 2) + (dayOfQuarter == null ? 0 : 2)];
		int i = 0;
		parameters[i++] = "year";
		parameters[i++] = Cypher.literalOf(year);
		if (quarter != null) {
			parameters[i++] = "quarter";
			parameters[i++] = Cypher.literalOf(quarter);
		}
		if (dayOfQuarter != null) {
			parameters[i++] = "dayOfQuarter";
			parameters[i] = Cypher.literalOf(dayOfQuarter);
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
	static FunctionInvocation ordinalDate(Integer year, Integer ordinalDay) {

		Assertions.notNull(year, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_YEAR_REQUIRED));
		Object[] parameters = new Object[2 + (ordinalDay == null ? 0 : 2)];
		int i = 0;
		parameters[i++] = "year";
		parameters[i++] = Cypher.literalOf(year);
		if (ordinalDay != null) {
			parameters[i++] = "ordinalDay";
			parameters[i] = Cypher.literalOf(ordinalDay);
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
	static FunctionInvocation date(MapExpression components) {

		Assertions.notNull(components, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_COMPONENTS_REQUIRED));
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
	static FunctionInvocation date(String temporalValue) {

		Assertions.hasText(temporalValue, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_TEMPORAL_VALUE_REQUIRED));
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
	static FunctionInvocation date(Expression temporalValue) {

		Assertions.notNull(temporalValue, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_TEMPORAL_VALUE_REQUIRED));
		return FunctionInvocation.create(BuiltInFunctions.Temporals.DATE, temporalValue);
	}

	/**
	 * Creates a function invocation for {@code datetime({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/datetime/">datetime</a>.
	 *
	 * @return A function call for {@code datetime({})}.
	 * @since 2020.1.0
	 */
	static FunctionInvocation datetime() {

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
	static FunctionInvocation datetime(TimeZone timeZone) {

		Assertions.notNull(timeZone, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_TZ_REQUIRED));
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
	static FunctionInvocation datetime(MapExpression components) {

		Assertions.notNull(components, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_COMPONENTS_REQUIRED));
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
	static FunctionInvocation datetime(String temporalValue) {

		Assertions.hasText(temporalValue, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_TEMPORAL_VALUE_REQUIRED));
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
	static FunctionInvocation datetime(Expression temporalValue) {

		Assertions.notNull(temporalValue, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_TEMPORAL_VALUE_REQUIRED));
		return FunctionInvocation.create(BuiltInFunctions.Temporals.DATETIME, temporalValue);
	}

	/**
	 * Creates a function invocation for {@code localdatetime({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/localdatetime/">localdatetime</a>.
	 *
	 * @return A function call for {@code localdatetime({})}.
	 * @since 2020.1.0
	 */
	static FunctionInvocation localdatetime() {

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
	static FunctionInvocation localdatetime(TimeZone timeZone) {

		Assertions.notNull(timeZone, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_TZ_REQUIRED));
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
	static FunctionInvocation localdatetime(MapExpression components) {

		Assertions.notNull(components, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_COMPONENTS_REQUIRED));
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
	static FunctionInvocation localdatetime(String temporalValue) {

		Assertions.hasText(temporalValue, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_TEMPORAL_VALUE_REQUIRED));
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
	static FunctionInvocation localdatetime(Expression temporalValue) {

		Assertions.notNull(temporalValue, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_TEMPORAL_VALUE_REQUIRED));
		return FunctionInvocation.create(BuiltInFunctions.Temporals.LOCALDATETIME, temporalValue);
	}

	/**
	 * Creates a function invocation for {@code localtime({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/localdatetime/">localtime</a>.
	 *
	 * @return A function call for {@code localtime({})}.
	 * @since 2020.1.0
	 */
	static FunctionInvocation localtime() {

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
	static FunctionInvocation localtime(TimeZone timeZone) {

		Assertions.notNull(timeZone, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_TZ_REQUIRED));
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
	static FunctionInvocation localtime(MapExpression components) {

		Assertions.notNull(components, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_COMPONENTS_REQUIRED));
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
	static FunctionInvocation localtime(String temporalValue) {

		Assertions.hasText(temporalValue, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_TEMPORAL_VALUE_REQUIRED));
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
	static FunctionInvocation localtime(Expression temporalValue) {

		Assertions.notNull(temporalValue, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_TEMPORAL_VALUE_REQUIRED));
		return FunctionInvocation.create(BuiltInFunctions.Temporals.LOCALTIME, temporalValue);
	}

	/**
	 * Creates a function invocation for {@code time({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/temporal/time/">time</a>.
	 *
	 * @return A function call for {@code time({})}.
	 * @since 2020.1.0
	 */
	static FunctionInvocation time() {

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
	static FunctionInvocation time(TimeZone timeZone) {

		Assertions.notNull(timeZone, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_TZ_REQUIRED));
		return FunctionInvocation
			.create(BuiltInFunctions.Temporals.TIME, timezoneMapLiteralOf(timeZone));
	}

	private static Expression timezoneMapLiteralOf(TimeZone timeZone) {
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
	static FunctionInvocation time(MapExpression components) {

		Assertions.notNull(components, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_COMPONENTS_REQUIRED));
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
	static FunctionInvocation time(String temporalValue) {

		Assertions.hasText(temporalValue, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_TEMPORAL_VALUE_REQUIRED));
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
	static FunctionInvocation time(Expression temporalValue) {

		Assertions.notNull(temporalValue, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_TEMPORAL_VALUE_REQUIRED));
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
	static FunctionInvocation duration(MapExpression components) {

		Assertions.notNull(components, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_COMPONENTS_REQUIRED));
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
	static FunctionInvocation duration(String temporalAmount) {

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
	static FunctionInvocation duration(Expression temporalAmount) {

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
	static FunctionInvocation shortestPath(Relationship relationship) {

		return FunctionInvocation.create(Scalars.SHORTEST_PATH, relationship);
	}

	/**
	 * Starts building a function invocation for {@code reduce({})}.
	 *
	 * @param variable The closure will have a variable introduced in its context. We decide here which variable to use.
	 * @return An ongoing definition for a function call to {@code reduce({})}.
	 * @since 2020.1.5
	 */
	static Reduction.OngoingDefinitionWithVariable reduce(SymbolicName variable) {

		return Reduction.of(variable);
	}

	/**
	 * Creates a function invocation for {@code abs({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-abs">abs</a>.
	 *
	 * @param expression The value to pass to the function.
	 * @return A function call for {@code abs({})}.
	 * @since 2021.0.0
	 */
	static FunctionInvocation abs(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(BuiltInFunctions.MathematicalFunctions.ABS, expression);
	}

	/**
	 * Creates a function invocation for {@code ceil({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-ceil">ceil</a>.
	 *
	 * @param expression The value to pass to the function.
	 * @return A function call for {@code ceil({})}.
	 * @since 2021.0.0
	 */
	static FunctionInvocation ceil(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(BuiltInFunctions.MathematicalFunctions.CEIL, expression);
	}

	/**
	 * Creates a function invocation for {@code floor({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-floor">floor</a>.
	 *
	 * @param expression The value to pass to the function.
	 * @return A function call for {@code floor({})}.
	 * @since 2021.0.0
	 */
	static FunctionInvocation floor(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(BuiltInFunctions.MathematicalFunctions.FLOOR, expression);
	}

	/**
	 * Creates a function invocation for {@code rand({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-rand">rand</a>.
	 *
	 * @return A function call for {@code rand({})}.
	 * @since 2021.0.0
	 */
	static FunctionInvocation rand() {

		return FunctionInvocation.create(BuiltInFunctions.MathematicalFunctions.RAND);
	}

	/**
	 * Creates a function invocation for {@code round({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-round">round</a>.
	 *
	 * @param value      The value to round
	 * @param expression Additional parameters, length must be 0, 1 or 2:
	 *                   First entry is the precision, second is the rounding mode
	 * @return A function call for {@code round({})}.
	 * @since 2021.0.0
	 */
	static FunctionInvocation round(Expression value, Expression... expression) {

		if (expression == null || expression.length == 0) {
			return FunctionInvocation.create(BuiltInFunctions.MathematicalFunctions.ROUND, value);
		} else if (expression.length == 1) {
			return FunctionInvocation.create(BuiltInFunctions.MathematicalFunctions.ROUND, value, expression[0]);
		} else if (expression.length == 2) {
			return FunctionInvocation
				.create(BuiltInFunctions.MathematicalFunctions.ROUND, value, expression[0], expression[1]);
		} else {
			throw new IllegalArgumentException(
				"round() must be called with 1, 2 or 3 arguments (value, value + precision or value + precision + rounding mode.");
		}
	}

	/**
	 * Creates a function invocation for {@code sign({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-sign">sign</a>.
	 *
	 * @param expression The value to pass to the function.
	 * @return A function call for {@code sign({})}.
	 * @since 2021.0.0
	 */
	static FunctionInvocation sign(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(BuiltInFunctions.MathematicalFunctions.SIGN, expression);
	}

	/**
	 * Creates a function invocation for {@code e({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-e">e</a>.
	 *
	 * @return A function call for {@code e({})}.
	 * @since 2021.0.0
	 */
	static FunctionInvocation e() {

		return FunctionInvocation.create(BuiltInFunctions.MathematicalFunctions.E);
	}

	/**
	 * Creates a function invocation for {@code exp({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-exp">exp</a>.
	 *
	 * @param expression The value to pass to the function.
	 * @return A function call for {@code exp({})}.
	 * @since 2021.0.0
	 */
	static FunctionInvocation exp(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(BuiltInFunctions.MathematicalFunctions.EXP, expression);
	}

	/**
	 * Creates a function invocation for {@code log({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-log">log</a>.
	 *
	 * @param expression The value to pass to the function.
	 * @return A function call for {@code log({})}.
	 * @since 2021.0.0
	 */
	static FunctionInvocation log(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(BuiltInFunctions.MathematicalFunctions.LOG, expression);
	}

	/**
	 * Creates a function invocation for {@code log10({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-log10">log10</a>.
	 *
	 * @param expression The value to pass to the function.
	 * @return A function call for {@code log10({})}.
	 * @since 2021.0.0
	 */
	static FunctionInvocation log10(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(BuiltInFunctions.MathematicalFunctions.LOG10, expression);
	}

	/**
	 * Creates a function invocation for {@code sqrt({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-sqrt">sqrt</a>.
	 *
	 * @param expression The value to pass to the function.
	 * @return A function call for {@code sqrt({})}.
	 * @since 2021.0.0
	 */
	static FunctionInvocation sqrt(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(BuiltInFunctions.MathematicalFunctions.SQRT, expression);
	}

	/**
	 * Creates a function invocation for {@code acos({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-acos">acos</a>.
	 *
	 * @param expression The value to pass to the function.
	 * @return A function call for {@code acos({})}.
	 * @since 2021.0.0
	 */
	static FunctionInvocation acos(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(BuiltInFunctions.MathematicalFunctions.ACOS, expression);
	}

	/**
	 * Creates a function invocation for {@code asin({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-asin">asin</a>.
	 *
	 * @param expression The value to pass to the function.
	 * @return A function call for {@code asin({})}.
	 * @since 2021.0.0
	 */
	static FunctionInvocation asin(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(BuiltInFunctions.MathematicalFunctions.ASIN, expression);
	}

	/**
	 * Creates a function invocation for {@code atan({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-atan">atan</a>.
	 *
	 * @param expression The value to pass to the function.
	 * @return A function call for {@code atan({})}.
	 * @since 2021.0.0
	 */
	static FunctionInvocation atan(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(BuiltInFunctions.MathematicalFunctions.ATAN, expression);
	}

	/**
	 * Creates a function invocation for {@code atan2({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-atan2">atan2</a>.
	 *
	 * @param y the y value of a point
	 * @param x the x value of a point
	 * @return A function call for {@code atan2({})}.
	 * @since 2021.0.0
	 */
	static FunctionInvocation atan2(Expression y, Expression x) {

		Assertions.notNull(y, "y is required.");
		Assertions.notNull(x, "x is required.");
		return FunctionInvocation.create(BuiltInFunctions.MathematicalFunctions.ATAN2, y, x);
	}

	/**
	 * Creates a function invocation for {@code cos({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-cos">cos</a>.
	 *
	 * @param expression The value to pass to the function.
	 * @return A function call for {@code cos({})}.
	 * @since 2021.0.0
	 */
	static FunctionInvocation cos(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(BuiltInFunctions.MathematicalFunctions.COS, expression);
	}

	/**
	 * Creates a function invocation for {@code cot({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-cot">cot</a>.
	 *
	 * @param expression The value to pass to the function.
	 * @return A function call for {@code cot({})}.
	 * @since 2021.0.0
	 */
	static FunctionInvocation cot(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(BuiltInFunctions.MathematicalFunctions.COT, expression);
	}

	/**
	 * Creates a function invocation for {@code degrees({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-degrees">degrees</a>.
	 *
	 * @param expression The value to pass to the function.
	 * @return A function call for {@code degrees({})}.
	 * @since 2021.0.0
	 */
	static FunctionInvocation degrees(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(BuiltInFunctions.MathematicalFunctions.DEGREES, expression);
	}

	/**
	 * Creates a function invocation for {@code haversin({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-haversin">haversin</a>.
	 *
	 * @param expression The value to pass to the function.
	 * @return A function call for {@code haversin({})}.
	 * @since 2021.0.0
	 */
	static FunctionInvocation haversin(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(BuiltInFunctions.MathematicalFunctions.HAVERSIN, expression);
	}

	/**
	 * Creates a function invocation for {@code pi({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-pi">pi</a>.
	 *
	 * @return A function call for {@code pi({})}.
	 * @since 2021.0.0
	 */
	static FunctionInvocation pi() {

		return FunctionInvocation.create(BuiltInFunctions.MathematicalFunctions.PI);
	}

	/**
	 * Creates a function invocation for {@code radians({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-radians">radians</a>.
	 *
	 * @param expression The value to pass to the function.
	 * @return A function call for {@code radians({})}.
	 * @since 2021.0.0
	 */
	static FunctionInvocation radians(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(BuiltInFunctions.MathematicalFunctions.RADIANS, expression);
	}

	/**
	 * Creates a function invocation for {@code sin({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-sin">sin</a>.
	 *
	 * @param expression The value to pass to the function.
	 * @return A function call for {@code sin({})}.
	 * @since 2021.0.0
	 */
	static FunctionInvocation sin(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(BuiltInFunctions.MathematicalFunctions.SIN, expression);
	}

	/**
	 * Creates a function invocation for {@code tan({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-tan">tan</a>.
	 *
	 * @param expression The value to pass to the function.
	 * @return A function call for {@code tan({})}.
	 * @since 2021.0.0
	 */
	static FunctionInvocation tan(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(BuiltInFunctions.MathematicalFunctions.TAN, expression);
	}

	/**
	 * Creates a function invocation for {@code toInteger({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-tointeger">toInteger</a>.
	 *
	 * @param expression The value to pass to the function.
	 * @return A function call for {@code toInteger({})}.
	 * @since 2021.2.1
	 */
	static FunctionInvocation toInteger(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(Scalars.TO_INTEGER, expression);
	}

	/**
	 * Creates a function invocation for {@code toString({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-tostring">toString</a>.
	 *
	 * @param expression The value to pass to the function.
	 * @return A function call for {@code toString({})}.
	 * @since 2022.3.0
	 */
	static FunctionInvocation toString(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(Scalars.TO_STRING, expression);
	}

	/**
	 * Creates a function invocation for {@code toStringOrNull({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-toStringOrNull">toStringOrNull</a>.
	 *
	 * @param expression The value to pass to the function.
	 * @return A function call for {@code toStringOrNull({})}.
	 * @since 2023.0.2
	 */
	static FunctionInvocation toStringOrNull(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(Strings.TO_STRING_OR_NULL, expressionOrNullLit(expression));
	}

	/**
	 * Creates a function invocation for {@code toFloat({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-tofloat">toFloat</a>.
	 *
	 * @param expression The value to pass to the function.
	 * @return A function call for {@code toFloat({})}.
	 * @since 2021.2.1
	 */
	static FunctionInvocation toFloat(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(Scalars.TO_FLOAT, expression);
	}

	/**
	 * Creates a function invocation for {@code toBoolean({})}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-toboolean">toBoolean</a>.
	 *
	 * @param expression The value to pass to the function.
	 * @return A function call for {@code toBoolean({})}.
	 * @since 2021.2.1
	 */
	static FunctionInvocation toBoolean(Expression expression) {

		Assertions.notNull(expression, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_REQUIRED));
		return FunctionInvocation.create(Scalars.TO_BOOLEAN, expression);
	}

	/**
	 * Creates a function invocation for {@code linenumber({})}. Only applicable inside an {@code LOAD CSV} clause.
	 *
	 * @return A function call for {@code linenumber({})}.
	 * @since 2021.2.1
	 */
	static FunctionInvocation linenumber() {

		return FunctionInvocation.create(() -> "linenumber");
	}

	/**
	 * Creates a function invocation for {@code file({})}. Only applicable inside an {@code LOAD CSV} clause.
	 *
	 * @return A function call for {@code file({})}.
	 * @since 2021.2.1
	 */
	static FunctionInvocation file() {

		return FunctionInvocation.create(() -> "file");
	}

	/**
	 * Creates a function invocation for {@code randomUUID({})}. Only applicable inside an {@code LOAD CSV} clause.
	 *
	 * @return A function call for {@code randomUUID({})}.
	 * @since 2022.2.1
	 */
	static FunctionInvocation randomUUID() {
		return FunctionInvocation.create(() -> "randomUUID");
	}

	/**
	 * Creates a function invocation for {@code length()}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-length">length</a>.
	 *
	 * @param path The path for which the length should be retrieved
	 * @return A function call for {@code length()} on a path.
	 * @since 2023.0.1
	 */
	static FunctionInvocation length(NamedPath path) {

		Assertions.notNull(path, "The path for length is required.");
		return FunctionInvocation.create(Scalars.LENGTH,
			path.getSymbolicName().orElseThrow(() -> new IllegalArgumentException(
				Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_NAMED_PATH_REQUIRED))));
	}

	/**
	 * Creates a function invocation for {@code graph.names()}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/graph/#functions-graph-names">graph.names</a>.
	 *
	 * @return A function call for {@code graph.names()}.
	 * @since 2023.4.0
	 */
	@Neo4jVersion(minimum = "5.0.0")
	static FunctionInvocation graphNames() {

		return FunctionInvocation.create(BuiltInFunctions.Graph.NAMES);
	}

	/**
	 * Creates a function invocation for {@code graph.propertiesByName()}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/graph/#functions-graph-propertiesByName">graph.propertiesByName</a>.
	 *
	 * @param name The name of the graph
	 * @return A function call for {@code graph.propertiesByName()}.
	 * @since 2023.4.0
	 */
	@Neo4jVersion(minimum = "5.0.0")
	static FunctionInvocation graphPropertiesByName(Expression name) {

		return FunctionInvocation.create(BuiltInFunctions.Graph.PROPERTIES_BY_NAME, name);
	}

	/**
	 * Creates a function invocation for {@code graph.byName()}.
	 * See <a href="https://neo4j.com/docs/cypher-manual/current/functions/graph/#functions-graph-byname">graph.byName</a>.
	 *
	 * @param name The name of the graph
	 * @return A function call for {@code graph.byName()}.
	 * @since 2023.4.0
	 */
	@Neo4jVersion(minimum = "5.0.0")
	static FunctionInvocation graphByName(Expression name) {

		return FunctionInvocation.create(BuiltInFunctions.Graph.BY_NAME, name);
	}

	private Functions() {
	}
}
