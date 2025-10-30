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

import java.lang.reflect.Array;
import java.net.URI;
import java.time.Duration;
import java.time.Period;
import java.time.temporal.TemporalAccessor;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.ResourceBundle;
import java.util.TimeZone;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ListComprehension.OngoingDefinitionWithVariable;
import org.neo4j.cypherdsl.core.Literal.UnsupportedLiteralException;
import org.neo4j.cypherdsl.core.PatternComprehension.OngoingDefinitionWithPattern;
import org.neo4j.cypherdsl.core.Statement.SingleQuery;
import org.neo4j.cypherdsl.core.Statement.UnionQuery;
import org.neo4j.cypherdsl.core.Statement.UseStatement;
import org.neo4j.cypherdsl.core.StatementBuilder.OngoingStandaloneCallWithoutArguments;
import org.neo4j.cypherdsl.core.utils.Assertions;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * The main entry point into the Cypher DSL. The Cypher Builder API is intended for
 * framework usage to produce Cypher statements required for database operations.
 *
 * @author Michael J. Simons
 * @author Gerrit Meier
 * @author Andreas Berger
 * @author Ali Ince
 * @since 1.0
 */
@SuppressWarnings("unused")
@API(status = STABLE, since = "1.0")
public final class Cypher {

	static final ResourceBundle MESSAGES = ResourceBundle.getBundle("org.neo4j.cypherdsl.core.messages");

	/**
	 * The foreign adapter factory. Can only be used when `com.querydsl:querydsl-core` is
	 * on the class path. The object won't be modified after initialisation.
	 */
	@SuppressWarnings("squid:S3077")
	private static volatile ForeignAdapterFactory foreignAdapterFactory;

	/**
	 * Not to be instantiated.
	 */
	private Cypher() {
	}

	/**
	 * Create a new Node representation with at least one label, the "primary" label. This
	 * is required. All other labels are optional.
	 * @param primaryLabel the primary label this node is identified by.
	 * @param additionalLabels additional labels
	 * @return a new node representation
	 */
	public static Node node(String primaryLabel, String... additionalLabels) {

		return new InternalNodeImpl(primaryLabel, additionalLabels);
	}

	/**
	 * Create a new Node representation with at least one label, the "primary" label. This
	 * is required. All other labels are optional.
	 * @param primaryLabel the primary label this node is identified by.
	 * @param additionalLabels additional labels
	 * @return a new node representation
	 */
	public static Node node(String primaryLabel, List<String> additionalLabels) {

		return new InternalNodeImpl(primaryLabel, additionalLabels.toArray(new String[] {}));
	}

	/**
	 * Create a new Node representation with at least one label, the "primary" label. This
	 * is required. All other labels are optional. This method also takes a map of
	 * properties. This allows the returned node object to be used in a {@code MATCH} or
	 * {@code MERGE} statement.
	 * @param primaryLabel the primary label this node is identified by.
	 * @param properties the properties expected to exist on the node.
	 * @param additionalLabels additional labels
	 * @return a new node representation
	 */
	public static Node node(String primaryLabel, MapExpression properties, String... additionalLabels) {

		return new InternalNodeImpl(null, primaryLabel, properties, additionalLabels);
	}

	/**
	 * Create a new Node representation with at least one label, the "primary" label. This
	 * is required. All other labels are optional. This method also takes a map of
	 * properties. This allows the returned node object to be used in a {@code MATCH} or
	 * {@code MERGE} statement.
	 * @param primaryLabel the primary label this node is identified by.
	 * @param properties the properties expected to exist on the node.
	 * @param additionalLabels additional labels
	 * @return a new node representation
	 * @since 2021.2.2
	 */
	public static Node node(String primaryLabel, MapExpression properties, Collection<String> additionalLabels) {

		return node(primaryLabel, properties, additionalLabels.toArray(new String[] {}));
	}

	/**
	 * {@return a node matching any node}
	 */
	public static Node anyNode() {
		return new InternalNodeImpl();
	}

	/**
	 * Creates an expression that matches the given {@code label} exactly. Can be used as
	 * starting point to add conditions via {@link Labels#and(Labels)} or
	 * {@link Labels#or(Labels)}.
	 * @param label the label to match
	 * @return a label expression
	 */
	public static Labels exactlyLabel(String label) {
		return Labels.exactly(label);
	}

	/**
	 * Creates a dynamic label expression matching all labels to which {@code expression}
	 * resolves.
	 * @param expression the expression that must resolve to a string or a list of strings
	 * @return a dynamic label expression
	 * @since 2025.1.0
	 */
	public static Labels allLabels(Expression expression) {
		return Labels.all(expression);
	}

	/**
	 * Creates a dynamic label expression matching any label to which {@code expression}
	 * resolves.
	 * @param expression the expression that must resolve to a string or a list of strings
	 * @return a dynamic label expression
	 * @since 2025.1.0
	 */
	public static Labels anyLabel(Expression expression) {
		return Labels.any(expression);
	}

	/**
	 * Creates a new {@literal Node} object.
	 * @param labelExpression required expression
	 * @return a node matching a label expression
	 * @since 2023.0.2
	 * @deprecated use {@link #node(Labels)}
	 */
	@SuppressWarnings("removal")
	@Deprecated(forRemoval = true)
	public static Node node(LabelExpression labelExpression) {
		return node(Labels.of(labelExpression));
	}

	/**
	 * Creates a new {@literal Node} object.
	 * @param labels required expression
	 * @return a node matching the given labels
	 * @since 2025.1.0
	 */
	public static Node node(Labels labels) {
		return new InternalNodeImpl(Objects.requireNonNull(labels), null);
	}

	/**
	 * {@return the '*' wildcard literal}
	 */
	public static Asterisk asterisk() {
		return Asterisk.INSTANCE;
	}

	/**
	 * Creates a new unlabeled {@literal Node} object.
	 * @param symbolicName the new symbolic name
	 * @return a node matching any node with the symbolic the given {@code symbolicName}.
	 */
	public static Node anyNode(String symbolicName) {
		return new InternalNodeImpl().named(symbolicName);
	}

	/**
	 * Creates a new unlabeled {@literal Node} object.
	 * @param symbolicName the new symbolic name
	 * @return a node matching any node with the symbolic the given {@code symbolicName}.
	 */
	public static Node anyNode(SymbolicName symbolicName) {
		return new InternalNodeImpl().named(symbolicName);
	}

	/**
	 * Dereferences a property for a symbolic name, most likely pointing to a property
	 * container like a node or a relationship.
	 * @param containerName the symbolic name of a property container
	 * @param names the names of the properties to dereference. More than one name does
	 * create a nested property like {@code containerName.name1.name2}.
	 * @return a new property
	 */
	public static Property property(String containerName, String... names) {
		return property(name(containerName), names);
	}

	/**
	 * Dereferences a property for a symbolic name, most likely pointing to a property
	 * container like a node or a relationship.
	 * @param containerName the symbolic name of a property container
	 * @param names the names of the properties to dereference. More than one name does
	 * create a nested property like {@code containerName.name1.name2}.
	 * @return a new property
	 * @since 2021.2.2
	 */
	public static Property property(String containerName, Collection<String> names) {
		return property(name(containerName), names.toArray(new String[] {}));
	}

	/**
	 * Dereferences a property on an arbitrary expression.
	 * @param expression the expression that describes some sort of accessible map
	 * @param names the names of the properties to dereference. More than one name does
	 * create a nested property like {@code expression.name1.name2}.
	 * @return a new property.
	 */
	public static Property property(Expression expression, String... names) {
		return InternalPropertyImpl.create(expression, names);
	}

	/**
	 * Dereferences a property on a arbitrary expression.
	 * @param expression the expression that describes some sort of accessible map
	 * @param names the names of the properties to dereference. More than one name does
	 * create a nested property like {@code expression.name1.name2}.
	 * @return a new property.
	 * @since 2021.2.2
	 */
	public static Property property(Expression expression, Collection<String> names) {
		return property(expression, names.toArray(new String[] {}));
	}

	/**
	 * Creates a dynamic lookup of a property for a symbolic name, most likely pointing to
	 * a property container like a node or a relationship. A dynamic property will be
	 * rendered as {@code p[expression]}.
	 * @param containerName the symbolic name of a property container
	 * @param lookup an expression to use as a dynamic lookup for properties of the
	 * container with the given name
	 * @return a new property
	 * @since 2021.0.0
	 */
	public static Property property(String containerName, Expression lookup) {
		return property(name(containerName), lookup);
	}

	/**
	 * Creates a dynamic lookup of a property on an arbitrary expression. A dynamic
	 * property will be rendered as {@code p[expression]}.
	 * @param expression the expression that describes some sort of accessible map
	 * @param lookup an expression to use as a dynamic lookup for properties of the
	 * container the expression resolved to
	 * @return a new property.
	 * @since 2021.0.0
	 */
	public static Property property(Expression expression, Expression lookup) {
		return InternalPropertyImpl.create(expression, lookup);
	}

	/**
	 * Starts defining a named path by indicating a name.
	 * @param name the name of the new path
	 * @return an ongoing definition of a named path
	 * @since 1.1
	 */
	public static NamedPath.OngoingDefinitionWithName path(String name) {
		return NamedPath.named(name);
	}

	/**
	 * Starts defining a named path by indicating a name.
	 * @param name the name of the new path
	 * @return an ongoing definition of a named path
	 * @since 1.1
	 */
	public static NamedPath.OngoingDefinitionWithName path(SymbolicName name) {
		return NamedPath.named(name);
	}

	/**
	 * Returns an ongoing definition of a named path, returning the k shortest paths.
	 * @param k the number of shortest groups to return
	 * @return an ongoing definition of a named path, returning the k shortest paths.
	 * @since 2024.7.0
	 */
	public static NamedPath.OngoingShortestDefinition shortestK(int k) {
		return NamedPath.shortest(k);
	}

	/**
	 * Returns an ongoing definition of a named path, returning the k shortest groups of
	 * points.
	 * @param k the number of shortest groups to return
	 * @return an ongoing definition of a named path, returning the k shortest groups of
	 * paths.
	 * @since 2024.7.0
	 */
	public static NamedPath.OngoingShortestDefinition shortestKGroups(int k) {
		return NamedPath.shortestKGroups(k);
	}

	/**
	 * Returns an ongoing definition of a named path, returning any shortest path.
	 * @return an ongoing definition of a named path, returning any shortest path
	 * @since 2024.7.0
	 */
	public static NamedPath.OngoingShortestDefinition anyShortest() {
		return NamedPath.any();
	}

	/**
	 * Returns an ongoing definition of a named path, returning all shortest paths.
	 * @return an ongoing definition of a named path, returning all shortest paths
	 * @since 2024.7.0
	 */
	public static NamedPath.OngoingShortestDefinition allShortest() {
		return NamedPath.allShortest();
	}

	/**
	 * Creates a new symbolic name.
	 * @param value the value of the symbolic name
	 * @return a new symbolic name
	 */
	public static SymbolicName name(String value) {

		return SymbolicName.of(value);
	}

	/**
	 * Creates a new parameter placeholder. Existing $-signs will be removed.
	 * @param name the name of the parameter, must not be null
	 * @return the new parameter
	 */
	public static Parameter<Object> parameter(String name) {
		return Parameter.create(name);
	}

	/**
	 * Creates a new parameter with the given {@code name} and a value bound to it. The
	 * value can be retrieved from the final statement build.
	 * @param name the name of the parameter, must not be null
	 * @param value the value of the parameter.
	 * @param <T> type of the new parameter
	 * @return the new parameter
	 * @since 2021.0.0
	 */
	public static <T> Parameter<T> parameter(String name, T value) {
		return Parameter.create(name, value);
	}

	/**
	 * Creates a new anonymous parameter with a value bound to it. The value can be
	 * retrieved from the final statement build. The name will be available as soon as the
	 * statement has been rendered.
	 * @param value the value of the parameter.
	 * @param <T> type of the new parameter
	 * @return the new parameter
	 * @since 2021.1.0
	 */
	public static <T> Parameter<T> anonParameter(T value) {
		return Parameter.anon(value);
	}

	/**
	 * Prepares an optional match statement.
	 * @param pattern the patterns to match
	 * @return an ongoing match that is used to specify an optional where and a required
	 * return clause
	 */
	public static StatementBuilder.OngoingReadingWithoutWhere optionalMatch(PatternElement... pattern) {

		return Statement.builder().optionalMatch(pattern);
	}

	/**
	 * Prepares an optional match statement.
	 * @param pattern the patterns to match
	 * @return an ongoing match that is used to specify an optional where and a required
	 * return clause
	 * @since 2021.2.2
	 */
	public static StatementBuilder.OngoingReadingWithoutWhere optionalMatch(
			Collection<? extends PatternElement> pattern) {

		return optionalMatch(pattern.toArray(new PatternElement[] {}));
	}

	/**
	 * Starts building a statement based on a match clause. Use
	 * {@link Cypher#node(String, String...)} and related to retrieve a node or a
	 * relationship, which both are pattern elements.
	 * @param pattern the patterns to match
	 * @return an ongoing match that is used to specify an optional where and a required
	 * return clause
	 */
	public static StatementBuilder.OngoingReadingWithoutWhere match(PatternElement... pattern) {

		return Statement.builder().match(pattern);
	}

	/**
	 * Starts building a statement based on a match clause. Use
	 * {@link Cypher#node(String, String...)} and related to retrieve a node or a
	 * relationship, which both are pattern elements.
	 * @param pattern the patterns to match
	 * @return an ongoing match that is used to specify an optional where and a required
	 * return clause
	 * @since 2021.2.2
	 */
	public static StatementBuilder.OngoingReadingWithoutWhere match(Collection<? extends PatternElement> pattern) {

		return match(pattern.toArray(new PatternElement[] {}));
	}

	/**
	 * Starts building a statement based on a match clause. Use
	 * {@link Cypher#node(String, String...)} and related to retrieve a node or a
	 * relationship, which both are pattern elements.
	 * @param optional a flag whether the {@code MATCH} clause includes the
	 * {@code OPTIONAL} keyword.
	 * @param pattern the patterns to match
	 * @return an ongoing match that is used to specify an optional where and a required
	 * return clause
	 * @since 2020.1.3
	 */
	public static StatementBuilder.OngoingReadingWithoutWhere match(boolean optional, PatternElement... pattern) {

		return Statement.builder().match(optional, pattern);
	}

	/**
	 * Starts building a statement based on a match clause. Use
	 * {@link Cypher#node(String, String...)} and related to retrieve a node or a
	 * relationship, which both are pattern elements.
	 * @param optional a flag whether the {@code MATCH} clause includes the
	 * {@code OPTIONAL} keyword.
	 * @param pattern the patterns to match
	 * @return an ongoing match that is used to specify an optional where and a required
	 * return clause
	 * @since 2021.2.2
	 */
	public static StatementBuilder.OngoingReadingWithoutWhere match(boolean optional,
			Collection<? extends PatternElement> pattern) {

		return match(optional, pattern.toArray(new PatternElement[] {}));
	}

	/**
	 * Starts building a statement based on a {@code CREATE} clause.
	 * @param pattern the patterns to create
	 * @return an ongoing {@code CREATE} that can be used to specify {@code WITH} and
	 * {@code RETURNING} etc.
	 */
	public static StatementBuilder.OngoingUpdate create(PatternElement... pattern) {

		return Statement.builder().create(pattern);
	}

	/**
	 * Starts building a statement based on a {@code CREATE} clause.
	 * @param pattern the patterns to create
	 * @return an ongoing {@code CREATE} that can be used to specify {@code WITH} and
	 * {@code RETURNING} etc.
	 * @since 2021.2.2
	 */
	public static StatementBuilder.OngoingUpdate create(Collection<? extends PatternElement> pattern) {

		return create(pattern.toArray(new PatternElement[] {}));
	}

	/**
	 * Starts a statement with a leading {@code WITH}. Those are useful for passing on
	 * lists of various type that can be unwound later on etc. A leading {@code WITH}
	 * obviously cannot be used with patterns and needs its arguments to have an alias.
	 * @param variables one ore more variables.
	 * @return an ongoing with clause.
	 * @since 2020.1.2
	 */
	public static StatementBuilder.OrderableOngoingReadingAndWithWithoutWhere with(String... variables) {

		return Statement.builder().with(variables);
	}

	/**
	 * Starts a statement with a leading {@code WITH}. Those are useful for passing on
	 * lists of various type that can be unwound later on etc. A leading {@code WITH}
	 * cannot be used with patterns obviously and needs its arguments to have an alias.
	 * @param elements one ore more variables.
	 * @return an ongoing with clause.
	 * @since 2020.1.2
	 */
	public static StatementBuilder.OrderableOngoingReadingAndWithWithoutWhere with(IdentifiableElement... elements) {

		return Statement.builder().with(elements);
	}

	/**
	 * Start building a new sub-query expression by importing variables into the scope
	 * with a {@literal WITH} clause.
	 * @param identifiableElements the identifiable elements to import
	 * @return a builder for creating the concrete sub-query
	 * @since 2023.9.0
	 */
	public static SubqueryExpressionBuilder subqueryWith(String... identifiableElements) {
		return subqueryWith(Arrays.stream(identifiableElements).map(SymbolicName::of).toArray(SymbolicName[]::new));
	}

	/**
	 * Start building a new sub-query expression by importing variables into the scope
	 * with a {@literal WITH} clause.
	 * @param identifiableElements the identifiable elements to import
	 * @return a builder for creating the concrete sub-query
	 * @since 2023.9.0
	 */
	public static SubqueryExpressionBuilder subqueryWith(IdentifiableElement... identifiableElements) {
		return Expressions.with(identifiableElements);
	}

	/**
	 * Starts a statement with a leading {@code WITH}. Those are useful for passing on
	 * lists of various type that can be unwound later on etc. A leading {@code WITH}
	 * cannot be used with patterns obviously and needs its arguments to have an alias.
	 * <p>
	 * This method takes both aliased and non-aliased expression. The later will produce
	 * only valid Cypher when used in combination with a correlated subquery via
	 * {@link Cypher#call(Statement)}.
	 * @param elements one ore more expressions.
	 * @return an ongoing with clause.
	 * @since 2021.2.2
	 */
	public static StatementBuilder.OrderableOngoingReadingAndWithWithoutWhere with(
			Collection<IdentifiableElement> elements) {

		return Statement.builder().with(elements);
	}

	/**
	 * Starts building a statement based on a {@code MERGE} clause.
	 * @param pattern the patterns to merge
	 * @return an ongoing {@code MERGE} that can be used to specify {@code WITH} and
	 * {@code RETURNING} etc.
	 */
	public static StatementBuilder.OngoingMerge merge(PatternElement... pattern) {

		return Statement.builder().merge(pattern);
	}

	/**
	 * Starts building a statement based on a {@code MERGE} clause.
	 * @param pattern the patterns to merge
	 * @return an ongoing {@code MERGE} that can be used to specify {@code WITH} and
	 * {@code RETURNING} etc.
	 * @since 2021.2.2
	 */
	public static StatementBuilder.OngoingMerge merge(Collection<? extends PatternElement> pattern) {

		return merge(pattern.toArray(new PatternElement[] {}));
	}

	/**
	 * Starts building a statement starting with an {@code UNWIND} clause. The expression
	 * needs to be an expression evaluating to a list, otherwise the query will fail.
	 * @param expression the expression to unwind
	 * @return an ongoing {@code UNWIND}.
	 */
	public static StatementBuilder.OngoingUnwind unwind(Expression expression) {

		return Statement.builder().unwind(expression);
	}

	/**
	 * Starts building a statement starting with an {@code UNWIND} clause. The expressions
	 * passed will be turned into a list expression
	 * @param expressions expressions to unwind
	 * @return a new instance of {@link StatementBuilder.OngoingUnwind}
	 */
	public static StatementBuilder.OngoingUnwind unwind(Expression... expressions) {

		return Statement.builder().unwind(Cypher.listOf(expressions));
	}

	/**
	 * Starts building a statement starting with an {@code UNWIND} clause. The expressions
	 * passed will be turned into a list expression
	 * @param expressions expressions to unwind
	 * @return a new instance of {@link StatementBuilder.OngoingUnwind}
	 * @since 2021.2.2
	 */
	public static StatementBuilder.OngoingUnwind unwind(Collection<? extends Expression> expressions) {

		return unwind(expressions.toArray(new Expression[] {}));
	}

	/**
	 * Creates a new {@link SortItem} to be used as part of an {@link Order}.
	 * @param expression the expression by which things should be sorted
	 * @return a sort item, providing means to specify ascending or descending order
	 */
	public static SortItem sort(Expression expression) {

		return SortItem.create(expression, null);
	}

	/**
	 * Creates a new {@link SortItem} to be used as part of an {@link Order}.
	 * @param expression the expression by which things should be sorted
	 * @param direction the direction to sort by. Defaults to
	 * {@link SortItem.Direction#UNDEFINED}.
	 * @return a sort item
	 * @since 2021.1.0
	 */
	public static SortItem sort(Expression expression, SortItem.Direction direction) {

		return SortItem.create(expression, direction);
	}

	/**
	 * Creates a map of expression from a list of key/value pairs.
	 * @param keysAndValues a list of key and values. Must be an even number, with
	 * alternating {@link String} and {@link Expression}
	 * @return a new map expression.
	 */
	public static MapExpression mapOf(Object... keysAndValues) {

		return MapExpression.create(false, keysAndValues);
	}

	/**
	 * Creates an alphabetically sorted map of expression from a list of key/value pairs.
	 * @param keysAndValues a list of key and values. Must be an even number, with
	 * alternating {@link String} and {@link Expression}
	 * @return a new map expression.
	 */
	public static MapExpression sortedMapOf(Object... keysAndValues) {

		return MapExpression.create(true, keysAndValues);
	}

	/**
	 * Creates a map of expression from a Java Map.
	 * @param map a map to be turned into a MapExpression
	 * @return a new map expression.
	 * @since 2021.1.0
	 */
	public static MapExpression asExpression(Map<String, Object> map) {

		return MapExpression.create(map);
	}

	/**
	 * Creates a {@link ListExpression list-expression} from several expressions.
	 * @param expressions expressions to get combined into a list
	 * @return a new instance of {@link ListExpression}
	 */
	public static ListExpression listOf(Expression... expressions) {

		return ListExpression.create(expressions);
	}

	/**
	 * Creates a {@link ListExpression list-expression} from several expressions.
	 * @param expressions expressions to get combined into a list
	 * @return a new instance of {@link ListExpression}
	 * @since 2021.2.2
	 */
	public static ListExpression listOf(Collection<? extends Expression> expressions) {

		return Cypher.listOf(expressions.toArray(new Expression[0]));
	}

	/**
	 * Creates a new {@link Literal Literal&lt;?&gt;} from the given {@code object}.
	 * @param object the object to represent.
	 * @param <T> the type of the literal returned
	 * @return a new {@link Literal Literal&lt;?&gt;}.
	 * @throws UnsupportedLiteralException when the object cannot be represented as a
	 * literal
	 */
	@SuppressWarnings("unchecked")
	public static <T> Literal<T> literalOf(Object object) {

		if (object == null) {
			return (Literal<T>) NullLiteral.INSTANCE;
		}
		if (object instanceof Literal<?>) {
			return (Literal<T>) object;
		}
		if (object instanceof CharSequence charSequence) {
			return (Literal<T>) new StringLiteral(charSequence);
		}
		if (object instanceof Character) {
			return (Literal<T>) new StringLiteral(String.valueOf(object));
		}
		if (object instanceof Number number) {
			return (Literal<T>) new NumberLiteral(number);
		}
		if (object instanceof TemporalAccessor temporalAccessor) {
			return (Literal<T>) new TemporalLiteral(temporalAccessor);
		}
		if (object instanceof Duration duration) {
			return (Literal<T>) DurationLiteral.of(duration);
		}
		if (object instanceof Period period) {
			return (Literal<T>) PeriodLiteral.of(period);
		}
		if (object instanceof Parameter<?> parameter) {
			return (Literal<T>) ParameterLiteral.of(parameter);
		}
		if (object instanceof Iterable || object.getClass().isArray()) {
			List<Literal<?>> elements = new ArrayList<>();
			Consumer<Object> handleElement = element -> {
				if (element instanceof Literal) {
					elements.add((Literal<?>) element);
				}
				else {
					try {
						elements.add(Cypher.literalOf(element));
					}
					catch (UnsupportedLiteralException ex) {
						throw new UnsupportedLiteralException("Unsupported literal type in iterable.", element);
					}
				}
			};
			if (object.getClass().isArray()) {
				for (int i = 0; i < Array.getLength(object); i++) {
					handleElement.accept(Array.get(object, i));
				}
			}
			else {
				((Iterable<?>) object).forEach(handleElement);
			}

			ListLiteral listLiteral = new ListLiteral(elements);
			return (Literal<T>) listLiteral;
		}
		if (object instanceof Map) {
			Map<String, Literal<?>> map = new LinkedHashMap<>();
			BiConsumer<Object, Object> handleEntry = (key, value) -> {
				if (!(key instanceof CharSequence || key instanceof Character)) {
					throw new UnsupportedLiteralException("Unsupported literal map key (not a string/char type).", key);
				}
				if (value instanceof Literal) {
					map.put(key.toString(), (Literal<?>) value);
				}
				else {
					try {
						map.put(key.toString(), Cypher.literalOf(value));
					}
					catch (UnsupportedLiteralException ex) {
						throw new UnsupportedLiteralException("Unsupported literal type in map.", value);
					}
				}
			};
			((Map<?, ?>) object).forEach(handleEntry);
			MapLiteral mapLiteral = new MapLiteral(map);
			return (Literal<T>) mapLiteral;
		}
		if (object instanceof Boolean b) {
			return (Literal<T>) BooleanLiteral.of(b);
		}
		throw new UnsupportedLiteralException(object);
	}

	/**
	 * {@return the `TRUE` literal}
	 */
	public static Literal<Boolean> literalTrue() {
		return BooleanLiteral.TRUE;
	}

	/**
	 * {@return the `FALSE` literal}
	 */
	public static Literal<Boolean> literalFalse() {
		return BooleanLiteral.FALSE;
	}

	/**
	 * {@return the `NULL` literal}
	 */
	public static Literal<Void> literalNull() {
		return NullLiteral.INSTANCE;
	}

	/**
	 * Creates a {@code UNION} statement from several other statements. No checks are
	 * applied for matching return types.
	 * @param statements the statements to union.
	 * @return a union statement.
	 */
	public static UnionQuery union(Statement... statements) {
		return unionImpl(false, statements);
	}

	/**
	 * Creates a {@code UNION} statement from several other statements. No checks are
	 * applied for matching return types.
	 * @param statements the statements to union.
	 * @return a union statement.
	 * @since 2021.2.2
	 */
	public static UnionQuery union(Collection<Statement> statements) {
		return union(statements.toArray(new Statement[] {}));
	}

	/**
	 * Creates a {@code UNION ALL} statement from several other statements. No checks are
	 * applied for matching return types.
	 * @param statements the statements to union.
	 * @return a union statement.
	 */
	public static Statement unionAll(Statement... statements) {
		return unionImpl(true, statements);
	}

	/**
	 * Creates a {@code UNION ALL} statement from several other statements. No checks are
	 * applied for matching return types.
	 * @param statements the statements to union.
	 * @return a union statement.
	 * @since 2021.2.2
	 */
	public static Statement unionAll(Collection<Statement> statements) {
		return unionAll(statements.toArray(new Statement[] {}));
	}

	/**
	 * A {@literal RETURN} statement without a previous match.
	 * @param expressions the elements to return
	 * @return a buildable statement
	 * @since 1.0.1
	 */
	public static StatementBuilder.OngoingReadingAndReturn returning(Expression... expressions) {
		return Statement.builder().returning(expressions);
	}

	/**
	 * A {@literal RETURN} statement without a previous match.
	 * @param expressions the expressions to return
	 * @return a buildable statement
	 * @since 2021.2.2
	 */
	public static StatementBuilder.OngoingReadingAndReturn returning(Collection<? extends Expression> expressions) {
		return Statement.builder().returning(expressions);
	}

	/**
	 * Creates a list comprehension starting with a {@link Relationship} or a
	 * {@link RelationshipChain chain of relationships}.
	 * @param relationshipPattern the relationship pattern on which the new list
	 * comprehension is based on.
	 * @return an ongoing definition.
	 * @since 2020.0.0
	 */
	public static OngoingDefinitionWithPattern listBasedOn(RelationshipPattern relationshipPattern) {
		return PatternComprehension.basedOn(relationshipPattern);
	}

	/**
	 * Creates a list comprehension starting with a {@link NamedPath named path}.
	 * @param namedPath the named path on which the new list comprehension is based on.
	 * @return an ongoing definition.
	 * @since 2020.1.1
	 */
	public static OngoingDefinitionWithPattern listBasedOn(NamedPath namedPath) {
		return PatternComprehension.basedOn(namedPath);
	}

	/**
	 * Starts defining a {@link ListComprehension list comprehension}.
	 * @param variable the variable to which each element of the list is assigned.
	 * @return an ongoing definition of a list comprehension
	 * @since 1.0.1
	 */
	public static OngoingDefinitionWithVariable listWith(SymbolicName variable) {
		return ListComprehension.with(variable);
	}

	/**
	 * Escapes and quotes the {@code unquotedString} for safe usage in Neo4j-Browser and
	 * Shell.
	 * @param unquotedString an unquoted string
	 * @return a quoted string with special chars escaped.
	 */
	public static String quote(String unquotedString) {
		return literalOf(unquotedString).asString();
	}

	/**
	 * {@return generic case expression start}
	 */
	public static Case caseExpression() {
		return Case.create(null);
	}

	/**
	 * Starts building a {@literal CASE} expression.
	 * @param expression initial expression for the simple case statement
	 * @return simple case expression start
	 */
	public static Case caseExpression(Expression expression) {
		return Case.create(expression);
	}

	/**
	 * Starts defining a procedure call of the procedure with the given
	 * {@literal procedureName}. That procedure name might be fully qualified - that is,
	 * including a namespace - or just a simple name.
	 * @param procedureName the procedure name of the procedure to call. Might be fully
	 * qualified.
	 * @return an ongoing definition of a call
	 */
	public static OngoingStandaloneCallWithoutArguments call(String procedureName) {

		Assertions.hasText(procedureName, "The procedure name must not be null or empty.");
		return call(procedureName.split("\\."));
	}

	/**
	 * Starts defining a procedure call of the procedure with the given qualified name.
	 * @param namespaceAndProcedure the procedure name of the procedure to call.
	 * @return an ongoing definition of a call
	 */
	public static OngoingStandaloneCallWithoutArguments call(String... namespaceAndProcedure) {
		return Statement.call(namespaceAndProcedure);
	}

	/**
	 * Starts defining a procedure call of the procedure with the given qualified name.
	 * @param namespaceAndProcedure the procedure name of the procedure to call.
	 * @return an ongoing definition of a call
	 * @since 2021.2.2
	 */
	public static OngoingStandaloneCallWithoutArguments call(Collection<String> namespaceAndProcedure) {
		return call(namespaceAndProcedure.toArray(new String[] {}));
	}

	/**
	 * Starts building a statement based on one subquery.
	 * @param subquery the statement representing the subquery
	 * @return a new ongoing read without any further conditions or returns.
	 * @neo4j.version 4.0.0
	 * @since 2020.1.2
	 * @see ExposesSubqueryCall#call(Statement)
	 */
	@Neo4jVersion(minimum = "4.0.0")
	public static StatementBuilder.OngoingReadingWithoutWhere call(Statement subquery) {
		return Statement.builder().call(subquery);
	}

	/**
	 * Creates a closed range with given boundaries.
	 * @param targetExpression the target expression for the range
	 * @param start the inclusive start
	 * @param end the exclusive end
	 * @return a range literal.
	 * @since 2020.1.0
	 */
	public static Expression subList(Expression targetExpression, Integer start, Integer end) {

		return ListOperator.subList(targetExpression, Cypher.literalOf(start), Cypher.literalOf(end));
	}

	/**
	 * Creates a closed range with given boundaries.
	 * @param targetExpression the target expression for the range
	 * @param start the inclusive start
	 * @param end the exclusive end
	 * @return a range literal.
	 * @since 2020.1.0
	 */
	public static Expression subList(Expression targetExpression, Expression start, Expression end) {

		return ListOperator.subList(targetExpression, start, end);
	}

	/**
	 * Creates an open range starting at {@code start}.
	 * @param targetExpression the target expression for the range
	 * @param start the inclusive start
	 * @return a range literal.
	 * @since 2020.1.0
	 */
	public static Expression subListFrom(Expression targetExpression, Integer start) {

		return ListOperator.subListFrom(targetExpression, Cypher.literalOf(start));
	}

	/**
	 * Creates an open range starting at {@code start}.
	 * @param targetExpression the target expression for the range
	 * @param start the inclusive start
	 * @return a range literal.
	 * @since 2020.1.0
	 */
	public static Expression subListFrom(Expression targetExpression, Expression start) {

		return ListOperator.subListFrom(targetExpression, start);
	}

	/**
	 * Creates an open range starting at {@code start}.
	 * @param targetExpression the target expression for the range
	 * @param end the exclusive end
	 * @return a range literal.
	 * @since 2020.1.0
	 */
	public static Expression subListUntil(Expression targetExpression, Integer end) {

		return ListOperator.subListUntil(targetExpression, Cypher.literalOf(end));
	}

	/**
	 * Creates an open range starting at {@code start}.
	 * @param targetExpression the target expression for the range
	 * @param end the exclusive end
	 * @return a range literal.
	 * @since 2020.1.0
	 */
	public static Expression subListUntil(Expression targetExpression, Expression end) {

		return ListOperator.subListUntil(targetExpression, end);
	}

	/**
	 * Creates a single valued range at {@code index}.
	 * @param targetExpression the target expression for the range
	 * @param index the index of the range
	 * @return a range literal.
	 * @since 2020.1.0
	 */
	public static ListOperator valueAt(Expression targetExpression, Integer index) {

		return valueAt(targetExpression, Cypher.literalOf(index));
	}

	/**
	 * Creates a single valued range at {@code index}.
	 * @param targetExpression the target expression for the range
	 * @param index the index of the range
	 * @return a range literal.
	 * @since 2020.1.0
	 */
	public static ListOperator valueAt(Expression targetExpression, Expression index) {

		return ListOperator.valueAt(targetExpression, index);
	}

	/**
	 * Creates an expression from a raw string fragment. No validation is performed on it.
	 * If it is used as expression, you must make sure to define something that works as
	 * expression.
	 * <p>
	 * This method expects exactly one placeholder in the form of {@literal $E} for any
	 * argument passed with {@code mixedArgs}.
	 * <p>
	 * To use exactly the term {@literal $E} escape it like this: {@literal \$E}
	 * @param format a raw Cypher string
	 * @param mixedArgs args to the Cypher string
	 * @return an expression to reuse with the builder.
	 * @since 2021.0.2
	 */
	public static Expression raw(String format, Object... mixedArgs) {

		return RawLiteral.create(format, mixedArgs);
	}

	/**
	 * Starts building a statement from a raw Cypher string that might also have arguments
	 * as supported through {@link Cypher#raw(String, Object...)}. Use this method as your
	 * own risk and be aware that no checks are done on the Cypher.
	 * @param rawCypher the raw Cypher statement to call
	 * @param args optional args that replace placeholders in the {@code rawCypher}
	 * @return ongoing sub-query definition based on the raw Cypher statement.
	 * @since 2024.2.0
	 */
	public static ExposesSubqueryCall.BuildableSubquery callRawCypher(String rawCypher, Object... args) {
		return Statement.builder().callRawCypher(rawCypher, args);
	}

	/**
	 * Creates a {@code RETURN} clause from a raw Cypher expression created via
	 * {@link Cypher#raw(String, Object...)}. The expression maybe aliased but it must
	 * resolve to a raw element
	 * @param rawExpression must be a plain raw or an aliased raw expression. To
	 * eventually render as valid Cypher, it must contain the {@code RETURN} keyword.
	 * @return a match that can be build now
	 * @since 2021.2.1
	 */
	public static StatementBuilder.OngoingReadingAndReturn returningRaw(Expression rawExpression) {
		return Statement.builder().returningRaw(rawExpression);
	}

	/**
	 * Provides access to the foreign DSL adapter. Please make sure you have the necessary
	 * runtime dependencies on the class path, otherwise you will see some kind of
	 * {@link ClassNotFoundException} along various classes related to the foreign DSL.
	 * @param expression the expression that should be adapted
	 * @param <FE> the type of the expression
	 * @return a foreign adapter
	 * @throws IllegalArgumentException in case the object cannot be adapter
	 * @since 2021.1.0
	 */
	public static <FE> ForeignAdapter<FE> adapt(FE expression) {

		ForeignAdapterFactory initializedForeignAdapterFactory = foreignAdapterFactory;
		if (initializedForeignAdapterFactory == null) {
			synchronized (Cypher.class) {
				initializedForeignAdapterFactory = foreignAdapterFactory;
				if (initializedForeignAdapterFactory == null) {
					foreignAdapterFactory = new ForeignAdapterFactory();
					initializedForeignAdapterFactory = foreignAdapterFactory;
				}
			}
		}
		return initializedForeignAdapterFactory.getAdapterFor(expression);
	}

	/**
	 * Starts building a {@code LOAD CSV} clause by using a periodic commit. The default
	 * rate of the database will be used.
	 * @return an ongoing definition of a {@code LOAD CSV} clause
	 * @since 2021.2.1
	 */
	public static ExposesLoadCSV usingPeriodicCommit() {

		return usingPeriodicCommit(null);
	}

	/**
	 * Starts building a {@code LOAD CSV} clause by using a periodic commit.
	 * @param rate the rate to be used. No checks are done on the rate, the database will
	 * verify valid values.
	 * @return an ongoing definition of a {@code LOAD CSV} clause
	 * @since 2021.2.1
	 */
	public static ExposesLoadCSV usingPeriodicCommit(Integer rate) {

		return LoadCSVStatementBuilder.usingPeriodicCommit(rate);
	}

	/**
	 * Starts building a {@code LOAD CSV}. No headers are assumed.
	 * @param from the URI to load data from. Any uri that is resolvable by the database
	 * itself is valid.
	 * @return an ongoing definition of a {@code LOAD CSV} clause
	 * @since 2021.2.1
	 */
	public static LoadCSVStatementBuilder.OngoingLoadCSV loadCSV(URI from) {

		return loadCSV(from, false);
	}

	/**
	 * Starts building a {@code LOAD CSV}.
	 * @param from the URI to load data from. Any uri that is resolvable by the database
	 * itself is valid.
	 * @param withHeaders set to {@literal true} if the csv file contains header
	 * @return an ongoing definition of a {@code LOAD CSV} clause
	 */
	public static LoadCSVStatementBuilder.OngoingLoadCSV loadCSV(URI from, boolean withHeaders) {

		return LoadCSVStatementBuilder.loadCSV(from, withHeaders);
	}

	private static UnionQuery unionImpl(boolean unionAll, Statement... statements) {

		Assertions.isTrue(statements != null && statements.length >= 2, "At least two statements are required!");

		int i = 0;
		UnionQueryImpl existingUnionQuery = null;
		@SuppressWarnings("squid:S2259") // Really, we asserted it 4 lines above this one.
											// Thank you, sonar.
		boolean isUnionQuery = statements[0] instanceof UnionQueryImpl;
		if (isUnionQuery) {
			existingUnionQuery = (UnionQueryImpl) statements[0];
			Assertions.isTrue(existingUnionQuery.isAll() == unionAll, "Cannot mix union and union all!");
			i = 1;
		}

		List<Statement> listOfQueries = new ArrayList<>();
		do {
			Assertions.isTrue(statements[i] instanceof SingleQuery || statements[i] instanceof ClausesBasedStatement,
					"Can only union single queries!");
			listOfQueries.add(statements[i]);
		}
		while (++i < statements.length);

		if (existingUnionQuery == null) {
			return UnionQueryImpl.create(unionAll, listOfQueries);
		}
		else {
			return existingUnionQuery.addAdditionalQueries(listOfQueries);
		}
	}

	/**
	 * Tries to format this expression into something human-readable. Not all expressions
	 * are supported
	 * @param expression an expression to format
	 * @return a human-readable string
	 * @throws IllegalArgumentException when the expression cannot be formatted
	 * @since 2021.3.2
	 */
	public static String format(Expression expression) {
		return Expressions.format(expression);
	}

	/**
	 * Decorates the given statement by prepending a static {@literal USE} clause.
	 * @param target the target. This might be a single database or a constituent of a
	 * composite database. This value will be escaped if necessary. If it contains a
	 * {@literal .}, both the first and second part will be escaped individually.
	 * @param statement the statement to decorate
	 * @return the new buildable statement
	 * @since 2023.0.0
	 */
	public static UseStatement use(String target, Statement statement) {
		return DecoratedQuery.decorate(statement, UseClauseImpl.of(target));
	}

	/**
	 * Decorates the given statement by prepending a dynamic {@literal USE} clause. A
	 * dynamic {@literal USE} clause will utilize {@code graph.byName} to resolve the
	 * target database.
	 * @param target a parameter that must resolve to a Cypher string.
	 * @param statement the statement to decorate
	 * @return the new buildable statement
	 * @since 2023.0.0
	 */
	public static UseStatement use(Parameter<?> target, Statement statement) {
		return DecoratedQuery.decorate(statement, UseClauseImpl.of(target));
	}

	/**
	 * Decorates the given statement by prepending a dynamic {@literal USE} clause. A
	 * dynamic {@literal USE} clause will utilize {@code graph.byName} to resolve the
	 * target database.
	 * @param target a string expression
	 * @param statement the statement to decorate
	 * @return the new buildable statement
	 * @since 2023.0.0
	 */
	public static UseStatement use(StringLiteral target, Statement statement) {
		return DecoratedQuery.decorate(statement, UseClauseImpl.of(target));
	}

	/**
	 * Decorates the given statement by prepending a dynamic {@literal USE} clause. A
	 * dynamic {@literal USE} clause will utilize {@code graph.byName} to resolve the
	 * target database unless {@link Cypher#graphByName(Expression)} has already been
	 * used.
	 * @param target the name of a variable pointing to the graph or constituent
	 * @param statement the statement to decorate
	 * @return the new buildable statement
	 * @since 2023.4.0
	 */
	public static UseStatement use(Expression target, Statement statement) {
		return DecoratedQuery.decorate(statement, UseClauseImpl.of(target));
	}

	/**
	 * Creates a condition that checks whether the {@code lhs} includes all elements
	 * present in {@code rhs}.
	 * @param lhs argument that is tested whether it contains all values in {@code rhs} or
	 * not
	 * @param rhs the reference collection
	 * @return an "includesAll" comparison
	 * @since 2023.9.0
	 */
	public static Condition includesAll(Expression lhs, Expression rhs) {
		return Conditions.includesAll(lhs, rhs);
	}

	/**
	 * Creates a condition that checks whether the {@code lhs} includes any element
	 * present in {@code rhs}.
	 * @param lhs argument that is tested whether it contains any values in {@code rhs} or
	 * not
	 * @param rhs the reference collection
	 * @return a "not_includes" comparison
	 * @since 2023.9.0
	 */
	public static Condition includesAny(Expression lhs, Expression rhs) {
		return Conditions.includesAny(lhs, rhs);
	}

	/**
	 * A condition testing if the given relationship pattern matches.
	 * @param relationshipPattern the pattern being evaluated in a condition
	 * @return a new condition matching the given pattern
	 * @since 2023.9.0
	 */
	public static Condition matching(RelationshipPattern relationshipPattern) {
		return Conditions.matching(relationshipPattern);
	}

	/**
	 * Creates a condition that matches if the right hand side is a regular expression
	 * that matches the left hand side via {@code =~}.
	 * @param lhs the left hand side of the comparison
	 * @param rhs the right hand side of the comparison
	 * @return a "matches" comparison
	 * @since 2023.9.0
	 */
	public static Condition matches(Expression lhs, Expression rhs) {
		return Conditions.matches(lhs, rhs);
	}

	/**
	 * Creates a condition that matches if both expressions are equals according to
	 * {@code =}.
	 * @param lhs the left hand side of the comparison
	 * @param rhs the right hand side of the comparison
	 * @return an "equals" comparison
	 * @since 2023.9.0
	 */
	public static Condition isEqualTo(Expression lhs, Expression rhs) {
		return Conditions.isEqualTo(lhs, rhs);
	}

	/**
	 * Creates a condition that matches if both expressions are equals according to
	 * {@code <>}.
	 * @param lhs the left hand side of the comparison
	 * @param rhs the right hand side of the comparison
	 * @return a "not equals" comparison
	 * @since 2023.9.0
	 */
	public static Condition isNotEqualTo(Expression lhs, Expression rhs) {
		return Conditions.isNotEqualTo(lhs, rhs);
	}

	/**
	 * Creates a condition that matches if the left hand side is less than the right hand
	 * side.
	 * @param lhs the left hand side of the comparison
	 * @param rhs the right hand side of the comparison
	 * @return a "less than" comparison
	 * @since 2023.9.0
	 */
	public static Condition lt(Expression lhs, Expression rhs) {
		return Conditions.lt(lhs, rhs);
	}

	/**
	 * Creates a condition that matches if the left hand side is less than or equal the
	 * right hand side.
	 * @param lhs the left hand side of the comparison
	 * @param rhs the right hand side of the comparison
	 * @return a "less than or equal" comparison
	 * @since 2023.9.0
	 */
	public static Condition lte(Expression lhs, Expression rhs) {
		return Conditions.lte(lhs, rhs);
	}

	/**
	 * Creates a condition that matches if the left hand side is greater than or equal the
	 * right hand side.
	 * @param lhs the left hand side of the comparison
	 * @param rhs the right hand side of the comparison
	 * @return a "greater than or equal" comparison
	 * @since 2023.9.0
	 */
	public static Condition gte(Expression lhs, Expression rhs) {
		return Conditions.gte(lhs, rhs);
	}

	/**
	 * Creates a condition that matches if the left hand side is greater than the right
	 * hand side.
	 * @param lhs the left hand side of the comparison
	 * @param rhs the right hand side of the comparison
	 * @return a "greater than" comparison
	 * @since 2023.9.0
	 */
	public static Condition gt(Expression lhs, Expression rhs) {
		return Conditions.gt(lhs, rhs);
	}

	/**
	 * Negates the given condition.
	 * @param condition the condition to negate. Must not be null.
	 * @return the negated condition.
	 * @since 2023.9.0
	 */
	public static Condition not(Condition condition) {
		return Conditions.not(condition);
	}

	/**
	 * Negates the given pattern element: The pattern must not matche to be included in
	 * the result.
	 * @param pattern the pattern to negate. Must not be null.
	 * @return a condition that evaluates to true when the pattern does not match.
	 * @since 2023.9.0
	 */
	public static Condition not(RelationshipPattern pattern) {
		return Conditions.not(pattern);
	}

	/**
	 * Creates a condition that checks whether the {@code lhs} starts with the
	 * {@code rhs}.
	 * @param lhs the left hand side of the comparison
	 * @param rhs the right hand side of the comparison
	 * @return a new condition.
	 * @since 2023.9.0
	 */
	public static Condition startsWith(Expression lhs, Expression rhs) {
		return Conditions.startsWith(lhs, rhs);
	}

	/**
	 * Creates a condition that checks whether the {@code lhs} contains with the
	 * {@code rhs}.
	 * @param lhs the left hand side of the comparison
	 * @param rhs the right hand side of the comparison
	 * @return a new condition.
	 * @since 2023.9.0
	 */
	public static Condition contains(Expression lhs, Expression rhs) {
		return Conditions.contains(lhs, rhs);
	}

	/**
	 * Creates a condition that checks whether the {@code lhs} ends with the {@code rhs}.
	 * @param lhs the left hand side of the comparison
	 * @param rhs the right hand side of the comparison
	 * @return a new condition.
	 * @since 2023.9.0
	 */
	public static Condition endsWith(Expression lhs, Expression rhs) {
		return Conditions.endsWith(lhs, rhs);
	}

	/**
	 * Creates a placeholder condition which is not rendered in the final statement but is
	 * useful while chaining conditions together.
	 * @return a placeholder condition.
	 * @since 2023.9.0
	 */
	public static Condition noCondition() {
		return Conditions.noCondition();
	}

	/**
	 * Creates a condition that checks whether the {@code expression} is {@literal null}.
	 * @param expression the expression to check for {@literal null}
	 * @return a new condition.
	 * @since 2023.9.0
	 */
	public static Condition isNull(Expression expression) {
		return Conditions.isNull(expression);
	}

	/**
	 * Creates a condition that checks whether the {@code expression} is not
	 * {@literal null}.
	 * @param expression the expression to check for {@literal null}
	 * @return a new condition.
	 * @since 2023.9.0
	 */
	public static Condition isNotNull(Expression expression) {
		return Conditions.isNotNull(expression);
	}

	/**
	 * Creates a new condition based on a function invocation for the {@code isEmpty()}
	 * function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/predicate/#functions-isempty">isEmpty</a>.
	 * <p>
	 * The argument {@code e} must refer to an expression that evaluates to a list for
	 * {@code isEmpty()} to work
	 * @param expression an expression referring to a list
	 * @return a function call for {@code isEmpty()} for a list
	 * @since 2023.9.0
	 */
	public static Condition isEmpty(Expression expression) {
		return Predicates.isEmpty(expression);
	}

	/**
	 * Creates a |{@literal true} condition.
	 * @return a condition that is always true.
	 * @since 2023.9.0
	 */
	public static Condition isTrue() {
		return Conditions.isTrue();
	}

	/**
	 * Creates a |{@literal false} condition.
	 * @return a condition that is always false.
	 * @since 2023.9.0
	 */
	public static Condition isFalse() {
		return Conditions.isFalse();
	}

	/**
	 * Checks if a given object has the given labels or types.
	 * @param symbolicName reference to the entity that should be checked for labels or
	 * types
	 * @param labelsOrTypes the list of labels or types to check for
	 * @return a condition that checks whether a node has a set of given labels or a
	 * relationship a set of given types.
	 * @since 2023.9.0
	 */
	public static Condition hasLabelsOrType(SymbolicName symbolicName, String... labelsOrTypes) {
		return HasLabelCondition.create(symbolicName, labelsOrTypes);
	}

	/**
	 * Checks if a given object has the given labels or types.
	 * @param symbolicName reference to the entity that should be checked for labels or
	 * types
	 * @param labels the expression of labels or types to check for
	 * @return a condition that checks whether a node has a set of given labels or a
	 * relationship a set of given types.
	 * @since 2025.1.0
	 */
	public static Condition hasLabelsOrType(SymbolicName symbolicName, Labels labels) {
		return HasLabelCondition.create(symbolicName, labels);
	}

	/**
	 * Creates a {@literal COUNT} sub-query expressions from at least one pattern.
	 * @param requiredPattern one pattern is required
	 * @param patternElement optional pattern
	 * @return the immutable {@link CountExpression}
	 * @since 2023.9.0
	 */
	public static CountExpression count(PatternElement requiredPattern, PatternElement... patternElement) {
		return Expressions.count(requiredPattern, patternElement);
	}

	/**
	 * Creates a {@literal COUNT} with an inner {@literal UNION} sub-query.
	 * @param union the union that will be the source of the {@literal COUNT} sub-query
	 * @return the immutable {@link CountExpression}
	 * @since 2023.9.0
	 */
	public static CountExpression count(UnionQuery union) {
		return Expressions.count(union);
	}

	/**
	 * Creates a {@literal COUNT} from a full statement, including its filters and
	 * conditions. The statement may or may not have a {@literal RETURN} clause. It must
	 * however not contain any updates. While it would render syntactically correct
	 * Cypher, Neo4j does not support updates inside counting sub-queries.
	 * @param statement the statement to be passed to {@code count{}}
	 * @param imports optional imports to be used in the statement (will be imported with
	 * {@literal WITH})
	 * @return a counting sub-query.
	 * @since 2023.9.0
	 */
	public static CountExpression count(Statement statement, IdentifiableElement... imports) {
		return Expressions.count(statement, imports);
	}

	/**
	 * Creates a {@literal COUNT} expression based on a list of pattern.
	 * @param pattern the list of patterns that shall be counted
	 * @param where an optional where-clause
	 * @return a count expression.
	 * @since 2023.9.0
	 */
	public static CountExpression count(List<PatternElement> pattern, Where where) {
		return Expressions.count(pattern, where);
	}

	/**
	 * Creates a {@literal COLLECT} subquery from a statement, including its filters and
	 * conditions. The statement must return exactly one column. It must however not
	 * contain any updates. While it would render syntactically correct Cypher, Neo4j does
	 * not support updates inside counting sub-queries.
	 * @param statement the statement to be passed to {@code COLLECT{}}
	 * @return a collecting sub-query.
	 * @since 2023.9.0
	 */
	public static Expression collect(Statement statement) {
		return Expressions.collect(statement);
	}

	/**
	 * Returns the name of the given expression or the expression itself if it isn't
	 * named.
	 * @param <T> the tyoe of the expression
	 * @param expression possibly named with a non-empty symbolic name.
	 * @return the name of the expression if the expression is named or the expression
	 * itself.
	 * @since 2023.9.0
	 */
	public static <T extends Expression> Expression nameOrExpression(T expression) {
		return Expressions.nameOrExpression(expression);
	}

	public static SymbolicName[] createSymbolicNames(String[] variables) {
		return Expressions.createSymbolicNames(variables);
	}

	public static SymbolicName[] createSymbolicNames(Named[] variables) {
		return Expressions.createSymbolicNames(variables);
	}

	/**
	 * Creates a function invocation for {@code elementId{}}.
	 * @param node the node for which the element id should be retrieved
	 * @return a function call for {@code elementId()} on a node.
	 * @since 2023.9.0
	 */
	@Neo4jVersion(minimum = "5.0.0")
	public static FunctionInvocation elementId(Node node) {
		return Functions.elementId(node);
	}

	/**
	 * Creates a function invocation for {@code elementId{}}.
	 * @param relationship the relationship for which the element id should be retrieved
	 * @return a function call for {@code elementId()} on a relationship.
	 * @since 2023.9.0
	 */
	@Neo4jVersion(minimum = "5.0.0")
	public static FunctionInvocation elementId(Relationship relationship) {
		return Functions.elementId(relationship);
	}

	/**
	 * Creates a function invocation for {@code keys{}}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/list/#functions-keys">keys</a>.
	 * @param node the node which keys should be returned.
	 * @return a function call for {@code keys()} on an expression.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation keys(Node node) {
		return Functions.keys(node);
	}

	/**
	 * Creates a function invocation for {@code keys{}}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/list/#functions-keys">keys</a>.
	 * @param relationship the relationship which keys should be returned.
	 * @return a function call for {@code keys()} on an expression.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation keys(Relationship relationship) {
		return Functions.keys(relationship);
	}

	/**
	 * Creates a function invocation for {@code keys{}}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/list/#functions-keys">keys</a>.
	 * @param expression the expressions which keys should be returned. Must resolve to a
	 * node, relationship or map.
	 * @return a function call for {@code keys()} on an expression.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation keys(Expression expression) {
		return Functions.keys(expression);
	}

	/**
	 * Creates a function invocation for {@code labels{}}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/list/#functions-labels">labels</a>.
	 * @param node the node for which the labels should be retrieved
	 * @return a function call for {@code labels()} on a node.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation labels(Node node) {
		return Functions.labels(node);
	}

	/**
	 * Creates a function invocation for {@code labels{}}. The {@link SymbolicName
	 * symbolic name} {@code  node} must point to a node. This can't be checked during
	 * compile time, so please make sure of that.
	 * <p>
	 * See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/list/#functions-labels">labels</a>.
	 * @param node the node for which the labels should be retrieved
	 * @return a function call for {@code labels()} on a node.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation labels(SymbolicName node) {
		return Functions.labels(node);
	}

	/**
	 * Creates a function invocation for {@code type{}}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-type">type</a>.
	 * @param relationship the relationship for which the type should be retrieved
	 * @return a function call for {@code type()} on a relationship.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation type(Relationship relationship) {
		return Functions.type(relationship);
	}

	/**
	 * Creates a function invocation for {@code type{}}. The {@link SymbolicName symbolic
	 * name} {@code relationship} must point to a relationship. This can't be checked
	 * during compile time, so please make sure of that.
	 * <p>
	 * See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-type">type</a>.
	 * @param relationship the relationship for which the type should be retrieved
	 * @return a function call for {@code type()} on a relationship.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation type(SymbolicName relationship) {
		return Functions.type(relationship);
	}

	/**
	 * Creates an instance of the {@code count} function.
	 * @param node the named node to be counted
	 * @return a function call for {@code count()} for one named node
	 * @since 2023.9.0
	 * @see #count(Expression)
	 */
	public static FunctionInvocation count(Node node) {
		return Functions.count(node);
	}

	/**
	 * Creates a function invocation for the {@code count()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-count">count</a>.
	 * @param expression an expression describing the things to count.
	 * @return a function call for {@code count()} for an expression like
	 * {@link Cypher#asterisk()} etc.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation count(Expression expression) {
		return Functions.count(expression);
	}

	/**
	 * Creates a function invocation for a {@code count()} function with {@code DISTINCT}
	 * added.
	 * @param node the named node to be counted
	 * @return a function call for {@code count()} for one named node
	 * @since 2023.9.0
	 * @see #countDistinct(Expression)
	 */
	public static FunctionInvocation countDistinct(Node node) {
		return Functions.countDistinct(node);
	}

	/**
	 * Creates a function invocation for a {@code count()} function with {@code DISTINCT}
	 * added. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-count">count</a>.
	 * @param expression an expression describing the things to count.
	 * @return a function call for {@code count()} for an expression like
	 * {@link Cypher#asterisk()} etc.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation countDistinct(Expression expression) {
		return Functions.countDistinct(expression);
	}

	/**
	 * Creates a function invocation for {@code properties())} on nodes.
	 * @param node the node who's properties should be returned.
	 * @return a function call for {@code properties())}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation properties(Node node) {
		return Functions.properties(node);
	}

	/**
	 * Creates a function invocation for {@code properties())} on relationships.
	 * @param relationship the relationship who's properties should be returned.
	 * @return a function call for {@code properties())}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation properties(Relationship relationship) {
		return Functions.properties(relationship);
	}

	/**
	 * Creates a function invocation for {@code properties())} on maps.
	 * @param map the map whose properties should be returned.
	 * @return a function call for {@code properties())}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation properties(MapExpression map) {
		return Functions.properties(map);
	}

	/**
	 * Creates a function invocation for the {@code coalesce()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-coalesce">coalesce</a>.
	 * @param expressions one or more expressions to be coalesced
	 * @return a function call for {@code coalesce}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation coalesce(Expression... expressions) {
		return Functions.coalesce(expressions);
	}

	/**
	 * Creates a function invocation for the {@code left()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-left">left</a>.
	 * @param expression an expression resolving to a string
	 * @param length desired length
	 * @return a function call for {@code left()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation left(Expression expression, Expression length) {
		return Functions.left(expression, length);
	}

	/**
	 * Creates a function invocation for the {@code ltrim()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-ltrim">ltrim</a>.
	 * @param expression an expression resolving to a string
	 * @return a function call for {@code ltrim()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation ltrim(Expression expression) {
		return Functions.ltrim(expression);
	}

	/**
	 * Creates a function invocation for the {@code replace()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-ltrim">replace</a>.
	 * @param original an expression that returns a string
	 * @param search an expression that specifies the string to be replaced in
	 * {@code original}.
	 * @param replace an expression that specifies the replacement string.
	 * @return a function call for {@code replace()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation replace(Expression original, Expression search, Expression replace) {
		return Functions.replace(original, search, replace);
	}

	/**
	 * Creates a function invocation for the {@code reverse()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-reverse">reverse</a>.
	 * @param original an expression that returns a string
	 * @return a function call for {@code reverse()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation reverse(Expression original) {
		return Functions.reverse(original);
	}

	/**
	 * Creates a function invocation for the {@code right()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-left">right</a>.
	 * @param expression an expression resolving to a string
	 * @param length desired length
	 * @return a function call for {@code right()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation right(Expression expression, Expression length) {
		return Functions.right(expression, length);
	}

	/**
	 * Creates a function invocation for the {@code rtrim()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-rtrim">rtrim</a>.
	 * @param expression an expression resolving to a string
	 * @return a function call for {@code rtrim()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation rtrim(Expression expression) {
		return Functions.rtrim(expression);
	}

	/**
	 * Creates a function invocation for the {@code substring()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-substring">rtrim</a>.
	 * @param original an expression resolving to a string
	 * @param start an expression that returns a positive integer, denoting the position
	 * at which the substring will begin.
	 * @param length an expression that returns a positive integer, denoting how many
	 * characters of original will be returned.
	 * @return a function call for {@code substring()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation substring(Expression original, Expression start, Expression length) {
		return Functions.substring(original, start, length);
	}

	/**
	 * Creates a function invocation for the {@code toLower()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-toLower">toLower</a>.
	 * @param expression an expression resolving to a string
	 * @return a function call for {@code toLower()} for one expression
	 * @since 2023.9.0
	 */
	public static FunctionInvocation toLower(Expression expression) {
		return Functions.toLower(expression);
	}

	/**
	 * Creates a function invocation for the {@code toUpper()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-toUpper">toUpper</a>.
	 * @param expression an expression resolving to a string
	 * @return a function call for {@code toLower()} for one expression
	 * @since 2023.9.0
	 */
	public static FunctionInvocation toUpper(Expression expression) {
		return Functions.toUpper(expression);
	}

	/**
	 * Creates a function invocation for the {@code trim()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-trim">trim</a>.
	 * @param expression an expression resolving to a string
	 * @return a function call for {@code trim()} for one expression
	 * @since 2023.9.0
	 */
	public static FunctionInvocation trim(Expression expression) {
		return Functions.trim(expression);
	}

	/**
	 * Creates a function invocation for the {@code split()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-split">split</a>.
	 * @param expression an expression resolving to a string that should be split
	 * @param delimiter the delimiter on which to split
	 * @return a function call for {@code split()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation split(Expression expression, Expression delimiter) {
		return Functions.split(expression, delimiter);
	}

	/**
	 * Creates a function invocation for the {@code split()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-split">split</a>.
	 * @param expression an expression resolving to a string that should be split
	 * @param delimiter the delimiter on which to split
	 * @return a function call for {@code split()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation split(Expression expression, String delimiter) {
		return Functions.split(expression, delimiter);
	}

	/**
	 * Creates a function invocation for the {@code size()} function. {@code size} can be
	 * applied to
	 * <ul>
	 * <li><a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-size">a
	 * list</a></li>
	 * <li><a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-size-of-string">to
	 * a string</a></li>
	 * </ul>
	 * @param expression the expression who's size is to be returned
	 * @return a function call for {@code size()} for one expression
	 * @since 2023.9.0
	 */
	public static FunctionInvocation size(Expression expression) {
		return Functions.size(expression);
	}

	/**
	 * Creates a function invocation for the {@code size()} function. {@code size} can be
	 * applied to
	 * <ul>
	 * <li><a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-size-of-pattern-expression">to
	 * a pattern expression</a></li>
	 * </ul>
	 * @param pattern the pattern for which {@code size()} should be invoked.
	 * @return a function call for {@code size()} for a pattern
	 * @since 2023.9.0
	 */
	public static FunctionInvocation size(RelationshipPattern pattern) {
		return Functions.size(pattern);
	}

	/**
	 * Creates a function invocation for the {@code exists()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/predicate/#functions-exists">exists</a>.
	 * @param expression the expression whose existence is to be evaluated
	 * @return a function call for {@code exists()} for one expression
	 * @since 2023.9.0
	 */
	public static FunctionInvocation exists(Expression expression) {
		return Functions.exists(expression);
	}

	/**
	 * Creates a function invocation for the {@code distance()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/spatial/#functions-distance">exists</a>.
	 * Both points need to be in the same coordinate system.
	 * @param point1 point 1
	 * @param point2 point 2
	 * @return a function call for {@code distance()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation distance(Expression point1, Expression point2) {
		return Functions.distance(point1, point2);
	}

	/**
	 * Creates a function invocation for the {@code point()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/spatial/#functions-point">point</a>.
	 * @param parameterMap the map of parameters for {@code point()}
	 * @return a function call for {@code point()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation point(MapExpression parameterMap) {
		return Functions.point(parameterMap);
	}

	/**
	 * Creates a function invocation for the {@code point()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/spatial/#functions-point">point</a>.
	 * <p>
	 * This generic expression variant is useful for referencing a point inside a
	 * parameter or another map.
	 * @param expression an expression resolving to a valid map of parameters for
	 * {@code point()}
	 * @return a function call for {@code point()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation point(Expression expression) {
		return Functions.point(expression);
	}

	/**
	 * Creates a function invocation for the {@code point()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/spatial/#functions-point">point</a>.
	 * @param parameter a parameter referencing a {@code point()}
	 * @return a function call for {@code point()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation point(Parameter<?> parameter) {
		return Functions.point(parameter);
	}

	/**
	 * Convenience method for creating a 2d cartesian point.
	 * @param x the x coordinate
	 * @param y the y coordinate
	 * @return a function call for {@code point()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation cartesian(double x, double y) {
		return Functions.cartesian(x, y);
	}

	/**
	 * Convenience method for creating a 2d coordinate in the WGS 84 coordinate system.
	 * @param longitude the longitude
	 * @param latitude the latitude
	 * @return a function call for {@code point()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation coordinate(double longitude, double latitude) {
		return Functions.coordinate(longitude, latitude);
	}

	/**
	 * Creates a function invocation for the {@code point.withinBBox} function. See
	 * <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/spatial/#functions-withinBBox">point.withinBBox</a>.
	 * @param point the point to check
	 * @param lowerLeft the lower left point of the bounding box (south-west coordinate)
	 * @param upperRight the upper right point of the bounding box (north-east coordinate)
	 * @return a function call for {@code point.withinBBox}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation withinBBox(Expression point, Expression lowerLeft, Expression upperRight) {
		return Functions.withinBBox(point, lowerLeft, upperRight);
	}

	/**
	 * Creates a function invocation for the {@code avg()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-avg">avg</a>.
	 * @param expression the things to average
	 * @return a function call for {@code avg()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation avg(Expression expression) {
		return Functions.avg(expression);
	}

	/**
	 * Creates a function invocation for the {@code avg()} function with {@code DISTINCT}
	 * added. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-avg">avg</a>.
	 * @param expression the things to average
	 * @return a function call for {@code avg()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation avgDistinct(Expression expression) {
		return Functions.avgDistinct(expression);
	}

	/**
	 * Creates a function invocation for the {@code collect()} function.
	 * @param variable the named thing to collect
	 * @return a function call for {@code collect()}
	 * @since 2023.9.0
	 * @see #collect(Expression)
	 */
	public static FunctionInvocation collect(Named variable) {
		return Functions.collect(variable);
	}

	/**
	 * Creates a function invocation for the {@code collect()} function with
	 * {@code DISTINCT} added.
	 * @param variable the named thing to collect
	 * @return a function call for {@code collect()}
	 * @since 2023.9.0
	 * @see #collect(Expression)
	 */
	public static FunctionInvocation collectDistinct(Named variable) {
		return Functions.collectDistinct(variable);
	}

	/**
	 * Creates a function invocation for the {@code collect()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-collect">collect</a>.
	 * @param expression the things to collect
	 * @return a function call for {@code collect()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation collect(Expression expression) {
		return Functions.collect(expression);
	}

	/**
	 * Creates a function invocation for the {@code collect()} function with
	 * {@code DISTINCT} added. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-collect">collect</a>.
	 * @param expression the things to collect
	 * @return a function call for {@code collect()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation collectDistinct(Expression expression) {
		return Functions.collectDistinct(expression);
	}

	/**
	 * Creates a function invocation for the {@code max()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-max">max</a>.
	 * @param expression a list from which the maximum element value is returned
	 * @return a function call for {@code max()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation max(Expression expression) {
		return Functions.max(expression);
	}

	/**
	 * Creates a function invocation for the {@code max()} function with {@code DISTINCT}
	 * added. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-max">max</a>.
	 * @param expression a list from which the maximum element value is returned
	 * @return a function call for {@code max()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation maxDistinct(Expression expression) {
		return Functions.maxDistinct(expression);
	}

	/**
	 * Creates a function invocation for the {@code min()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-min">min</a>.
	 * @param expression a list from which the minimum element value is returned
	 * @return a function call for {@code min()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation min(Expression expression) {
		return Functions.min(expression);
	}

	/**
	 * Creates a function invocation for the {@code min()} function with {@code DISTINCT}
	 * added. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-min">min</a>.
	 * @param expression a list from which the minimum element value is returned
	 * @return a function call for {@code min()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation minDistinct(Expression expression) {
		return Functions.minDistinct(expression);
	}

	/**
	 * Creates a function invocation for the {@code percentileCont()} function. See
	 * <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-percentilecont">percentileCont</a>.
	 * @param expression a numeric expression
	 * @param percentile a numeric value between 0.0 and 1.0
	 * @return a function call for {@code percentileCont()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation percentileCont(Expression expression, Number percentile) {
		return Functions.percentileCont(expression, percentile);
	}

	/**
	 * Creates a function invocation for the {@code percentileCont()} function with
	 * {@code DISTINCT} added. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-percentilecont">percentileCont</a>.
	 * @param expression a numeric expression
	 * @param percentile a numeric value between 0.0 and 1.0
	 * @return a function call for {@code percentileCont()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation percentileContDistinct(Expression expression, Number percentile) {
		return Functions.percentileContDistinct(expression, percentile);
	}

	/**
	 * Creates a function invocation for the {@code percentileDisc()} function. See
	 * <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-percentiledisc">percentileDisc</a>.
	 * @param expression a numeric expression
	 * @param percentile a numeric value between 0.0 and 1.0
	 * @return a function call for {@code percentileDisc()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation percentileDisc(Expression expression, Number percentile) {
		return Functions.percentileDisc(expression, percentile);
	}

	/**
	 * Creates a function invocation for the {@code percentileDisc()} function with
	 * {@code DISTINCT} added. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-percentiledisc">percentileDisc</a>.
	 * @param expression a numeric expression
	 * @param percentile a numeric value between 0.0 and 1.0
	 * @return a function call for {@code percentileDisc()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation percentileDiscDistinct(Expression expression, Number percentile) {
		return Functions.percentileDiscDistinct(expression, percentile);
	}

	/**
	 * Creates a function invocation for the {@code stDev()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-stdev">stDev</a>.
	 * @param expression a numeric expression
	 * @return a function call for {@code stDev()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation stDev(Expression expression) {
		return Functions.stDev(expression);
	}

	/**
	 * Creates a function invocation for the {@code stDev()} function with
	 * {@code DISTINCT} added. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-stdev">stDev</a>.
	 * @param expression a numeric expression
	 * @return a function call for {@code stDev()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation stDevDistinct(Expression expression) {
		return Functions.stDevDistinct(expression);
	}

	/**
	 * Creates a function invocation for the {@code stDevP()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-stdevp">stDevP</a>.
	 * @param expression a numeric expression
	 * @return a function call for {@code stDevP()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation stDevP(Expression expression) {
		return Functions.stDevP(expression);
	}

	/**
	 * Creates a function invocation for the {@code stDevP()} function with
	 * {@code DISTINCT} added. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-stdevp">stDevP</a>.
	 * @param expression a numeric expression
	 * @return a function call for {@code stDevP()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation stDevPDistinct(Expression expression) {
		return Functions.stDevPDistinct(expression);
	}

	/**
	 * Creates a function invocation for the {@code sum()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-sum">sum</a>.
	 * @param expression an expression returning a set of numeric values
	 * @return a function call for {@code sum()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation sum(Expression expression) {
		return Functions.sum(expression);
	}

	/**
	 * Creates a function invocation for the {@code sum()} function with {@code DISTINCT}
	 * added. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/aggregating/#functions-sum">sum</a>.
	 * @param expression an expression returning a set of numeric values
	 * @return a function call for {@code sum()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation sumDistinct(Expression expression) {
		return Functions.sumDistinct(expression);
	}

	/**
	 * Creates a function invocation for the {@code range()} function.
	 * @param start the range's start
	 * @param end the range's end
	 * @return a function call for {@code range()}
	 * @since 2023.9.0
	 * @see #range(Expression, Expression)
	 */
	public static FunctionInvocation range(Integer start, Integer end) {
		return Functions.range(start, end);
	}

	/**
	 * Creates a function invocation for the {@code range()} function.
	 * @param start the range's start
	 * @param end the range's end
	 * @return a function call for {@code range()}
	 * @since 2023.9.0
	 * @see #range(Expression, Expression, Expression)
	 */
	public static FunctionInvocation range(Expression start, Expression end) {
		return Functions.range(start, end);
	}

	/**
	 * Creates a function invocation for the {@code range()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/list/#functions-range">range</a>.
	 * @param start the range's start
	 * @param end the range's end
	 * @param step the range's step
	 * @return a function call for {@code range()}
	 * @since 2023.9.0
	 * @see #range(Expression, Expression, Expression)
	 */
	public static FunctionInvocation range(Integer start, Integer end, Integer step) {
		return Functions.range(start, end, step);
	}

	/**
	 * Creates a function invocation for the {@code range()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/list/#functions-range">range</a>.
	 * @param start the range's start
	 * @param end the range's end
	 * @param step the range's step
	 * @return a function call for {@code range()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation range(Expression start, Expression end, Expression step) {
		return Functions.range(start, end, step);
	}

	/**
	 * Creates a function invocation for the {@code head()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-head">head</a>.
	 * @param expression a list from which the head element is returned
	 * @return a function call for {@code head()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation head(Expression expression) {
		return Functions.head(expression);
	}

	/**
	 * Creates a function invocation for the {@code last()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-last">last</a>.
	 * @param expression a list from which the last element is returned
	 * @return a function call for {@code last()}
	 * @since 2023.9.0
	 */
	public static FunctionInvocation last(Expression expression) {
		return Functions.last(expression);
	}

	/**
	 * Creates a function invocation for {@code nodes{}}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/list/#functions-nodes">nodes</a>.
	 * @param path the path for which the number of nodes should be retrieved
	 * @return a function call for {@code nodes()} on a path.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation nodes(NamedPath path) {
		return Functions.nodes(path);
	}

	/**
	 * Creates a function invocation for {@code nodes{}}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/list/#functions-nodes">nodes</a>.
	 * @param symbolicName the symbolic name of a path for which the number of nodes
	 * should be retrieved
	 * @return a function call for {@code nodes{}} on a path represented by a symbolic
	 * name.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation nodes(SymbolicName symbolicName) {
		return Functions.nodes(symbolicName);
	}

	/**
	 * Creates a function invocation for {@code relationships{}}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/list/#functions-relationships">relationships</a>.
	 * @param path the path for which the relationships should be retrieved
	 * @return a function call for {@code relationships()} on a path.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation relationships(NamedPath path) {
		return Functions.relationships(path);
	}

	/**
	 * Creates a function invocation for {@code relationships{}}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/list/#functions-relationships">relationships</a>.
	 * @param symbolicName the symbolic name of a path for which the relationships should
	 * be retrieved
	 * @return a function call for {@code relationships()} on a path represented by a
	 * symbolic name.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation relationships(SymbolicName symbolicName) {
		return Functions.relationships(symbolicName);
	}

	/**
	 * Creates a function invocation for {@code startNode{}}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-startnode">startNode</a>.
	 * @param relationship the relationship for which the start node be retrieved
	 * @return a function call for {@code startNode()} on a path.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation startNode(Relationship relationship) {
		return Functions.startNode(relationship);
	}

	/**
	 * Creates a function invocation for {@code endNode{}}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-endnode">endNode</a>.
	 * @param relationship the relationship for which the end node be retrieved
	 * @return a function call for {@code endNode()} on a path.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation endNode(Relationship relationship) {
		return Functions.endNode(relationship);
	}

	/**
	 * Creates a function invocation for {@code date()}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/date/">date</a>.
	 * This is the most simple form.
	 * @return a function call for {@code date()}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation date() {
		return Functions.date();
	}

	/**
	 * Creates a function invocation for {@code date({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/date/">date</a>.
	 * @param year the year
	 * @param month the month
	 * @param day the day
	 * @return a function call for {@code date({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation calendarDate(Integer year, Integer month, Integer day) {
		return Functions.calendarDate(year, month, day);
	}

	/**
	 * Creates a function invocation for {@code date({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/date/">date</a>.
	 * @param year the year
	 * @param week the optional week
	 * @param dayOfWeek the optional day of the week
	 * @return a function call for {@code date({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation weekDate(Integer year, Integer week, Integer dayOfWeek) {
		return Functions.weekDate(year, week, dayOfWeek);
	}

	/**
	 * Creates a function invocation for {@code date({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/date/">date</a>.
	 * @param year the year
	 * @param quarter the optional week
	 * @param dayOfQuarter the optional day of the week
	 * @return a function call for {@code date({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation quarterDate(Integer year, Integer quarter, Integer dayOfQuarter) {
		return Functions.quarterDate(year, quarter, dayOfQuarter);
	}

	/**
	 * Creates a function invocation for {@code date({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/date/">date</a>.
	 * @param year the year
	 * @param ordinalDay the ordinal day of the year.
	 * @return a function call for {@code date({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation ordinalDate(Integer year, Integer ordinalDay) {
		return Functions.ordinalDate(year, ordinalDay);
	}

	/**
	 * Creates a function invocation for {@code date({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/date/">date</a>.
	 * This is the most generic form.
	 * @param components the map to pass to {@code date({})}
	 * @return a function call for {@code date({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation date(MapExpression components) {
		return Functions.date(components);
	}

	/**
	 * Creates a function invocation for {@code date({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/date/">date</a>.
	 * This creates a date from a string.
	 * @param temporalValue a string representing a temporal value.
	 * @return a function call for {@code date({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation date(String temporalValue) {
		return Functions.date(temporalValue);
	}

	/**
	 * Creates a function invocation for {@code date({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/date/">date</a>.
	 * This creates a date from a string.
	 * @param temporalValue an expression representing a temporal value.
	 * @return a function call for {@code date({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation date(Expression temporalValue) {
		return Functions.date(temporalValue);
	}

	/**
	 * Creates a function invocation for {@code datetime({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/datetime/">datetime</a>.
	 * @return a function call for {@code datetime({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation datetime() {
		return Functions.datetime();
	}

	/**
	 * Creates a function invocation for {@code datetime({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/datetime/">datetime</a>.
	 * @param timeZone the timezone to use when creating the temporal instance
	 * @return a function call for {@code datetime({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation datetime(TimeZone timeZone) {
		return Functions.datetime(timeZone);
	}

	/**
	 * Creates a function invocation for {@code datetime({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/datetime/">datetime</a>.
	 * This is the most generic form.
	 * @param components the map to pass to {@code datetime({})}
	 * @return a function call for {@code datetime({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation datetime(MapExpression components) {
		return Functions.datetime(components);
	}

	/**
	 * Creates a function invocation for {@code datetime({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/date/">datetime</a>.
	 * This creates a datetime from a string.
	 * @param temporalValue a string representing a temporal value.
	 * @return a function call for {@code datetime({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation datetime(String temporalValue) {
		return Functions.datetime(temporalValue);
	}

	/**
	 * Creates a function invocation for {@code datetime({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/date/">datetime</a>.
	 * This creates a datetime from a string.
	 * @param temporalValue an expression representing a temporal value.
	 * @return a function call for {@code date({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation datetime(Expression temporalValue) {
		return Functions.datetime(temporalValue);
	}

	/**
	 * Creates a function invocation for {@code localdatetime({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/localdatetime/">localdatetime</a>.
	 * @return a function call for {@code localdatetime({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation localdatetime() {
		return Functions.localdatetime();
	}

	/**
	 * Creates a function invocation for {@code localdatetime({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/localdatetime/">localdatetime</a>.
	 * @param timeZone the timezone to use when creating the temporal instance
	 * @return a function call for {@code localdatetime({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation localdatetime(TimeZone timeZone) {
		return Functions.localdatetime(timeZone);
	}

	/**
	 * Creates a function invocation for {@code localdatetime({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/localdatetime/">localdatetime</a>.
	 * This is the most generic form.
	 * @param components the map to pass to {@code localdatetime({})}
	 * @return a function call for {@code localdatetime({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation localdatetime(MapExpression components) {
		return Functions.localdatetime(components);
	}

	/**
	 * Creates a function invocation for {@code localdatetime({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/localdatetime/">localdatetime</a>.
	 * This creates a localdatetime from a string.
	 * @param temporalValue a string representing a temporal value.
	 * @return a function call for {@code localdatetime({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation localdatetime(String temporalValue) {
		return Functions.localdatetime(temporalValue);
	}

	/**
	 * Creates a function invocation for {@code localdatetime({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/localdatetime/">localdatetime</a>.
	 * This creates a localdatetime from a string.
	 * @param temporalValue an expression representing a temporal value.
	 * @return a function call for {@code localdatetime({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation localdatetime(Expression temporalValue) {
		return Functions.localdatetime(temporalValue);
	}

	/**
	 * Creates a function invocation for {@code localtime({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/localdatetime/">localtime</a>.
	 * @return a function call for {@code localtime({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation localtime() {
		return Functions.localtime();
	}

	/**
	 * Creates a function invocation for {@code localtime({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/localtime/">localtime</a>.
	 * @param timeZone the timezone to use when creating the temporal instance
	 * @return a function call for {@code localtime({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation localtime(TimeZone timeZone) {
		return Functions.localtime(timeZone);
	}

	/**
	 * Creates a function invocation for {@code localtime({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/localdatetime/">localtime</a>.
	 * This is the most generic form.
	 * @param components the map to pass to {@code localtime({})}
	 * @return a function call for {@code localtime({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation localtime(MapExpression components) {
		return Functions.localtime(components);
	}

	/**
	 * Creates a function invocation for {@code localtime({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/localtime/">localtime</a>.
	 * This creates a localtime from a string.
	 * @param temporalValue a string representing a temporal value.
	 * @return a function call for {@code localtime({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation localtime(String temporalValue) {
		return Functions.localtime(temporalValue);
	}

	/**
	 * Creates a function invocation for {@code localtime({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/localtime/">localtime</a>.
	 * This creates a localtime from a string.
	 * @param temporalValue an expression representing a temporal value.
	 * @return a function call for {@code localtime({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation localtime(Expression temporalValue) {
		return Functions.localtime(temporalValue);
	}

	/**
	 * Creates a function invocation for {@code time({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/time/">time</a>.
	 * @return a function call for {@code time({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation time() {
		return Functions.time();
	}

	/**
	 * Creates a function invocation for {@code time({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/time/">time</a>.
	 * @param timeZone the timezone to use when creating the temporal instance
	 * @return a function call for {@code time({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation time(TimeZone timeZone) {
		return Functions.time(timeZone);
	}

	/**
	 * Creates a function invocation for {@code time({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/time/">time</a>.
	 * This is the most generic form.
	 * @param components the map to pass to {@code time({})}
	 * @return a function call for {@code time({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation time(MapExpression components) {
		return Functions.time(components);
	}

	/**
	 * Creates a function invocation for {@code time({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/time/">time</a>.
	 * This creates a time from a string.
	 * @param temporalValue a string representing a temporal value.
	 * @return a function call for {@code time({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation time(String temporalValue) {
		return Functions.time(temporalValue);
	}

	/**
	 * Creates a function invocation for {@code time({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/time/">time</a>.
	 * This creates a time from a string.
	 * @param temporalValue an expression representing a temporal value.
	 * @return a function call for {@code time({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation time(Expression temporalValue) {
		return Functions.time(temporalValue);
	}

	/**
	 * Creates a function invocation for {@code duration({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/duration/">duration</a>.
	 * This is the most generic form.
	 * @param components the map to pass to {@code duration({})}
	 * @return a function call for {@code duration({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation duration(MapExpression components) {
		return Functions.duration(components);
	}

	/**
	 * Creates a function invocation for {@code duration({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/duration/">duration</a>.
	 * This creates a duration from a string.
	 * @param temporalAmount a string representing a temporal amount.
	 * @return a function call for {@code duration({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation duration(String temporalAmount) {
		return Functions.duration(temporalAmount);
	}

	/**
	 * Creates a function invocation for {@code duration({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/temporal/duration/">duration</a>.
	 * This creates a duration from a string.
	 * @param temporalAmount an expression representing a temporal amount.
	 * @return a function call for {@code duration({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation duration(Expression temporalAmount) {
		return Functions.duration(temporalAmount);
	}

	/**
	 * Starts building a function invocation for {@code reduce({})}.
	 * @param variable the closure will have a variable introduced in its context. We
	 * decide here which variable to use.
	 * @return an ongoing definition for a function call to {@code reduce({})}.
	 * @since 2023.9.0
	 */
	public static Reduction.OngoingDefinitionWithVariable reduce(SymbolicName variable) {
		return Functions.reduce(variable);
	}

	/**
	 * Creates a function invocation for {@code abs({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-abs">abs</a>.
	 * @param expression the value to pass to the function.
	 * @return a function call for {@code abs({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation abs(Expression expression) {
		return Functions.abs(expression);
	}

	/**
	 * Creates a function invocation for {@code ceil({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-ceil">ceil</a>.
	 * @param expression the value to pass to the function.
	 * @return a function call for {@code ceil({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation ceil(Expression expression) {
		return Functions.ceil(expression);
	}

	/**
	 * Creates a function invocation for {@code floor({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-floor">floor</a>.
	 * @param expression the value to pass to the function.
	 * @return a function call for {@code floor({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation floor(Expression expression) {
		return Functions.floor(expression);
	}

	/**
	 * Creates a function invocation for {@code rand({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-rand">rand</a>.
	 * @return a function call for {@code rand({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation rand() {
		return Functions.rand();
	}

	/**
	 * Creates a function invocation for {@code round({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-round">round</a>.
	 * @param value the value to round
	 * @param expression additional parameters, length must be 0, 1 or 2: First entry is
	 * the precision, second is the rounding mode
	 * @return a function call for {@code round({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation round(Expression value, Expression... expression) {
		return Functions.round(value, expression);
	}

	/**
	 * Creates a function invocation for {@code sign({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-sign">sign</a>.
	 * @param expression the value to pass to the function.
	 * @return a function call for {@code sign({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation sign(Expression expression) {
		return Functions.sign(expression);
	}

	/**
	 * Creates a function invocation for {@code e({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-e">e</a>.
	 * @return a function call for {@code e({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation e() {
		return Functions.e();
	}

	/**
	 * Creates a function invocation for {@code exp({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-exp">exp</a>.
	 * @param expression the value to pass to the function.
	 * @return a function call for {@code exp({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation exp(Expression expression) {
		return Functions.exp(expression);
	}

	/**
	 * Creates a function invocation for {@code log({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-log">log</a>.
	 * @param expression the value to pass to the function.
	 * @return a function call for {@code log({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation log(Expression expression) {
		return Functions.log(expression);
	}

	/**
	 * Creates a function invocation for {@code log10({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-log10">log10</a>.
	 * @param expression the value to pass to the function.
	 * @return a function call for {@code log10({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation log10(Expression expression) {
		return Functions.log10(expression);
	}

	/**
	 * Creates a function invocation for {@code sqrt({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-sqrt">sqrt</a>.
	 * @param expression the value to pass to the function.
	 * @return a function call for {@code sqrt({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation sqrt(Expression expression) {
		return Functions.sqrt(expression);
	}

	/**
	 * Creates a function invocation for {@code acos({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-acos">acos</a>.
	 * @param expression the value to pass to the function.
	 * @return a function call for {@code acos({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation acos(Expression expression) {
		return Functions.acos(expression);
	}

	/**
	 * Creates a function invocation for {@code asin({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-asin">asin</a>.
	 * @param expression the value to pass to the function.
	 * @return a function call for {@code asin({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation asin(Expression expression) {
		return Functions.asin(expression);
	}

	/**
	 * Creates a function invocation for {@code atan({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-atan">atan</a>.
	 * @param expression the value to pass to the function.
	 * @return a function call for {@code atan({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation atan(Expression expression) {
		return Functions.atan(expression);
	}

	/**
	 * Creates a function invocation for {@code atan2({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-atan2">atan2</a>.
	 * @param y the y value of a point
	 * @param x the x value of a point
	 * @return a function call for {@code atan2({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation atan2(Expression y, Expression x) {
		return Functions.atan2(y, x);
	}

	/**
	 * Creates a function invocation for {@code cos({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-cos">cos</a>.
	 * @param expression the value to pass to the function.
	 * @return a function call for {@code cos({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation cos(Expression expression) {
		return Functions.cos(expression);
	}

	/**
	 * Creates a function invocation for {@code cot({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-cot">cot</a>.
	 * @param expression the value to pass to the function.
	 * @return a function call for {@code cot({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation cot(Expression expression) {
		return Functions.cot(expression);
	}

	/**
	 * Creates a function invocation for {@code degrees({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-degrees">degrees</a>.
	 * @param expression the value to pass to the function.
	 * @return a function call for {@code degrees({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation degrees(Expression expression) {
		return Functions.degrees(expression);
	}

	/**
	 * Creates a function invocation for {@code haversin({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-haversin">haversin</a>.
	 * @param expression the value to pass to the function.
	 * @return a function call for {@code haversin({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation haversin(Expression expression) {
		return Functions.haversin(expression);
	}

	/**
	 * Creates a function invocation for {@code pi({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-pi">pi</a>.
	 * @return a function call for {@code pi({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation pi() {
		return Functions.pi();
	}

	/**
	 * Creates a function invocation for {@code radians({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-radians">radians</a>.
	 * @param expression the value to pass to the function.
	 * @return a function call for {@code radians({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation radians(Expression expression) {
		return Functions.radians(expression);
	}

	/**
	 * Creates a function invocation for {@code sin({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-sin">sin</a>.
	 * @param expression the value to pass to the function.
	 * @return a function call for {@code sin({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation sin(Expression expression) {
		return Functions.sin(expression);
	}

	/**
	 * Creates a function invocation for {@code tan({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/mathematical-numeric/#functions-tan">tan</a>.
	 * @param expression the value to pass to the function.
	 * @return a function call for {@code tan({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation tan(Expression expression) {
		return Functions.tan(expression);
	}

	/**
	 * Creates a function invocation for {@code toInteger({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-tointeger">toInteger</a>.
	 * @param expression the value to pass to the function.
	 * @return a function call for {@code toInteger({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation toInteger(Expression expression) {
		return Functions.toInteger(expression);
	}

	/**
	 * Creates a function invocation for {@code toString({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-tostring">toString</a>.
	 * @param expression the value to pass to the function.
	 * @return a function call for {@code toString({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation toString(Expression expression) {
		return Functions.toString(expression);
	}

	/**
	 * Creates a function invocation for {@code toStringOrNull({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/string/#functions-toStringOrNull">toStringOrNull</a>.
	 * @param expression the value to pass to the function.
	 * @return a function call for {@code toStringOrNull({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation toStringOrNull(Expression expression) {
		return Functions.toStringOrNull(expression);
	}

	/**
	 * Creates a function invocation for {@code toFloat({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-tofloat">toFloat</a>.
	 * @param expression the value to pass to the function.
	 * @return a function call for {@code toFloat({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation toFloat(Expression expression) {
		return Functions.toFloat(expression);
	}

	/**
	 * Creates a function invocation for {@code toBoolean({})}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-toboolean">toBoolean</a>.
	 * @param expression the value to pass to the function.
	 * @return a function call for {@code toBoolean({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation toBoolean(Expression expression) {
		return Functions.toBoolean(expression);
	}

	/**
	 * Creates a function invocation for {@code linenumber({})}. Only applicable inside an
	 * {@code LOAD CSV} clause.
	 * @return a function call for {@code linenumber({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation linenumber() {
		return Functions.linenumber();
	}

	/**
	 * Creates a function invocation for {@code file({})}. Only applicable inside an
	 * {@code LOAD CSV} clause.
	 * @return a function call for {@code file({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation file() {
		return Functions.file();
	}

	/**
	 * Creates a function invocation for {@code randomUUID({})}. Only applicable inside an
	 * {@code LOAD CSV} clause.
	 * @return a function call for {@code randomUUID({})}.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation randomUUID() {
		return Functions.randomUUID();
	}

	/**
	 * Creates a function invocation for {@code length()}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-length">length</a>.
	 * @param path the path for which the length should be retrieved
	 * @return a function call for {@code length()} on a path.
	 * @since 2023.9.0
	 */
	public static FunctionInvocation length(NamedPath path) {
		return Functions.length(path);
	}

	/**
	 * Creates a function invocation for {@code graph.names()}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/graph/#functions-graph-names">graph.names</a>.
	 * @return a function call for {@code graph.names()}.
	 * @since 2023.9.0
	 */
	@Neo4jVersion(minimum = "5.0.0")
	public static FunctionInvocation graphNames() {
		return Functions.graphNames();
	}

	/**
	 * Creates a function invocation for {@code graph.propertiesByName()}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/graph/#functions-graph-propertiesByName">graph.propertiesByName</a>.
	 * @param name the name of the graph
	 * @return a function call for {@code graph.propertiesByName()}.
	 * @since 2023.9.0
	 */
	@Neo4jVersion(minimum = "5.0.0")
	public static FunctionInvocation graphPropertiesByName(Expression name) {
		return Functions.graphPropertiesByName(name);
	}

	/**
	 * Creates a function invocation for {@code graph.byName()}. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/graph/#functions-graph-byname">graph.byName</a>.
	 * @param name the name of the graph
	 * @return a function call for {@code graph.byName()}.
	 * @since 2023.9.0
	 */
	@Neo4jVersion(minimum = "5.0.0")
	public static FunctionInvocation graphByName(Expression name) {
		return Functions.graphByName(name);
	}

	/**
	 * Create a new map projection with the given, mixed content.
	 * @param name the symbolic name of this project
	 * @param content the projected content
	 * @return a new map projection
	 * @since 2023.9.0
	 */
	public static MapProjection createProjection(SymbolicName name, Object... content) {
		return MapProjection.create(name, content);
	}

	/**
	 * Creates an unary minus operation.
	 * @param e the expression to which the unary minus should be applied. We don't check
	 * if it's a numeric expression, but in hindsight to generate semantically correct
	 * Cypher, it's recommended that is one.
	 * @return an unary minus operation.
	 * @since 2023.9.0
	 */
	public static Operation minus(Expression e) {
		return Operations.minus(e);
	}

	/**
	 * Creates an unary plus operation.
	 * @param e the expression to which the unary plus should be applied. We don't check
	 * if it's a numeric expression, but in hindsight to generate semantically correct
	 * Cypher, it's recommended that is one.
	 * @return an unary plus operation.
	 * @since 2023.9.0
	 */
	public static Expression plus(Expression e) {
		return Operations.plus(e);
	}

	public static Operation concat(Expression op1, Expression op2) {
		return Operations.concat(op1, op2);
	}

	public static Operation add(Expression op1, Expression op2) {
		return Operations.add(op1, op2);
	}

	public static Operation subtract(Expression op1, Expression op2) {
		return Operations.subtract(op1, op2);
	}

	public static Operation multiply(Expression op1, Expression op2) {
		return Operations.multiply(op1, op2);
	}

	public static Operation divide(Expression op1, Expression op2) {
		return Operations.divide(op1, op2);
	}

	public static Operation remainder(Expression op1, Expression op2) {
		return Operations.remainder(op1, op2);
	}

	public static Operation pow(Expression op1, Expression op2) {
		return Operations.pow(op1, op2);
	}

	/**
	 * Creates a {@code =} operation. The left hand side should resolve to a property or
	 * to something which has labels or types to modify and the right hand side should
	 * either be new properties or labels.
	 * @param target the target that should be modified
	 * @param value the new value of the target
	 * @return a new operation.
	 * @since 2023.9.0
	 */
	public static Operation set(Expression target, Expression value) {
		return Operations.set(target, value);
	}

	/**
	 * Creates a {@code +=} operation. The left hand side must resolve to a container
	 * (either a node or a relationship) of properties and the right hand side must be a
	 * map of new or updated properties
	 * @param target the target container that should be modified
	 * @param value the new properties
	 * @return a new operation.
	 * @since 2023.9.0
	 */
	public static Operation mutate(Expression target, MapExpression value) {
		return Operations.mutate(target, value);
	}

	/**
	 * Creates a {@code +=} operation. The left hand side must resolve to a container
	 * (either a node or a relationship) of properties and the right hand side must be a
	 * map of new or updated properties
	 * @param target the target container that should be modified
	 * @param value the new properties
	 * @return a new operation.
	 * @since 2023.9.0
	 */
	public static Operation mutate(Expression target, Expression value) {
		return Operations.mutate(target, value);
	}

	/**
	 * Creates an operation adding one or more labels to a given {@link Node node}.
	 * @param target the target of the new labels
	 * @param label the labels to be added
	 * @return a set operation
	 * @since 2023.9.0
	 */
	public static Operation setLabel(Node target, String... label) {
		return Operations.set(target, label);
	}

	/**
	 * Creates an operation adding dynamic labels to a {@link Node node}.
	 * @param target the target of the new labels
	 * @param label the labels to be added
	 * @return a set operation
	 * @since 2025.1.0
	 */
	public static Operation setLabel(Node target, Labels label) {
		return Operations.set(target, label);
	}

	/**
	 * Creates an operation removing one or more labels from a given {@link Node node}.
	 * @param target the target of the remove operation
	 * @param label the labels to be removed
	 * @return a remove operation
	 * @since 2023.9.0
	 */
	public static Operation removeLabel(Node target, String... label) {
		return Operations.remove(target, label);
	}

	/**
	 * Creates a new condition based on a function invocation for the {@code exists()}
	 * function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/predicate/#functions-exists">exists</a>.
	 * @param property the property to be passed to {@code exists()}
	 * @return a function call for {@code exists()} for one property
	 * @since 2023.9.0
	 */
	public static Condition exists(Property property) {
		return Predicates.exists(property);
	}

	/**
	 * Creates a new condition based on a function invocation for the {@code exists()}
	 * function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/predicate/#functions-exists">exists</a>.
	 * @param pattern the pattern to be passed to {@code exists()}
	 * @return a function call for {@code exists()} for one pattern
	 * @since 2023.9.0
	 */
	public static Condition exists(RelationshipPattern pattern) {
		return Predicates.exists(pattern);
	}

	/**
	 * Creates a new condition via an <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/syntax/expressions/#existential-subqueries">existential
	 * sub-query</a>. The statement may or may not have a {@literal  RETURN} clause. It
	 * must however not contain any updates. While it would render syntactically correct
	 * Cypher, Neo4j does not support updates inside existential sub-queries.
	 * @param statement the statement to be passed to {@code exists{}}
	 * @param imports optional imports to be used in the statement (will be imported with
	 * {@literal WITH})
	 * @return an existential sub-query.
	 * @since 2023.9.0
	 */
	public static Condition exists(Statement statement, IdentifiableElement... imports) {
		return Predicates.exists(statement, imports);
	}

	/**
	 * Creates a new condition via an <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/syntax/expressions/#existential-subqueries">existential
	 * sub-query</a> based on the list of patterns.
	 * @param pattern the pattern that must exists
	 * @return an existential sub-query.
	 * @since 2023.9.0
	 */
	public static Condition exists(PatternElement pattern) {
		return Predicates.exists(pattern);
	}

	/**
	 * Creates a new condition via an <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/syntax/expressions/#existential-subqueries">existential
	 * sub-query</a> based on the list of patterns.
	 * @param pattern the list of patterns that must exists
	 * @return an existential sub-query.
	 * @since 2023.9.0
	 */
	public static Condition exists(List<PatternElement> pattern) {
		return Predicates.exists(pattern);
	}

	/**
	 * Creates a new condition via an <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/syntax/expressions/#existential-subqueries">existential
	 * sub-query</a> based on the list of patterns and an optional {@link Where
	 * where-clause}.
	 * @param pattern the list of patterns that must exists
	 * @param where an optional where-clause
	 * @return an existential sub-query.
	 * @since 2023.9.0
	 */
	public static Condition exists(List<PatternElement> pattern, Where where) {
		return Predicates.exists(pattern, where);
	}

	/**
	 * Starts building an {@literal ALL} predicate.
	 * @param variable the variable referring to elements of a list
	 * @return a builder for the {@code all()} predicate function
	 * @since 2023.9.0
	 * @see #all(SymbolicName)
	 */
	public static OngoingListBasedPredicateFunction all(String variable) {
		return Predicates.all(variable);
	}

	/**
	 * Starts building a new condition based on a function invocation for the
	 * {@code all()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/predicate/#functions-all">exists</a>.
	 * @param variable the variable referring to elements of a list
	 * @return a builder for the {@code all()} predicate function
	 * @since 2023.9.0
	 */
	public static OngoingListBasedPredicateFunction all(SymbolicName variable) {
		return Predicates.all(variable);
	}

	/**
	 * Starts building a {@literal ANY} predicate.
	 * @param variable the variable referring to elements of a list
	 * @return a builder for the {@code any()} predicate function
	 * @since 2023.9.0
	 * @see #any(SymbolicName)
	 */
	public static OngoingListBasedPredicateFunction any(String variable) {
		return Predicates.any(variable);
	}

	/**
	 * Starts building a new condition based on a function invocation for the
	 * {@code any()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/predicate/#functions-any">exists</a>.
	 * @param variable the variable referring to elements of a list
	 * @return a builder for the {@code any()} predicate function
	 * @since 2023.9.0
	 */
	public static OngoingListBasedPredicateFunction any(SymbolicName variable) {
		return Predicates.any(variable);
	}

	/**
	 * Starts building a {@literal NONE} predicate.
	 * @param variable the variable referring to elements of a list
	 * @return a builder for the {@code none()} predicate function
	 * @since 2023.9.0
	 * @see #none(SymbolicName)
	 */
	public static OngoingListBasedPredicateFunction none(String variable) {
		return Predicates.none(variable);
	}

	/**
	 * Starts building a new condition based on a function invocation for the
	 * {@code none()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/predicate/#functions-none">exists</a>.
	 * @param variable the variable referring to elements of a list
	 * @return a builder for the {@code none()} predicate function
	 * @since 2023.9.0
	 */
	public static OngoingListBasedPredicateFunction none(SymbolicName variable) {
		return Predicates.none(variable);
	}

	/**
	 * Starts building a {@literal SINGLE} predicate.
	 * @param variable the variable referring to elements of a list
	 * @return a builder for the {@code single()} predicate function
	 * @since 2023.9.0
	 * @see #single(SymbolicName)
	 */
	public static OngoingListBasedPredicateFunction single(String variable) {
		return Predicates.single(variable);
	}

	/**
	 * Starts building a new condition based on a function invocation for the
	 * {@code single()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/predicate/#functions-single">single</a>.
	 * @param variable the variable referring to elements of a list
	 * @return a builder for the {@code single()} predicate function
	 * @since 2023.9.0
	 */
	public static OngoingListBasedPredicateFunction single(SymbolicName variable) {
		return Predicates.single(variable);
	}

}
