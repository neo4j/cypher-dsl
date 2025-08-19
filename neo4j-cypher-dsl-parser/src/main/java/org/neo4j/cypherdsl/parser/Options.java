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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.UnaryOperator;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.Clauses;
import org.neo4j.cypherdsl.core.Expression;
import org.neo4j.cypherdsl.core.Match;
import org.neo4j.cypherdsl.core.PatternElement;
import org.neo4j.cypherdsl.core.Return;
import org.neo4j.cypherdsl.core.Where;
import org.neo4j.cypherdsl.core.ast.Visitable;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * Provides arguments to the {@link CypherParser cypher parser}. The options itself are
 * thread safe and can be reused. The listener and modifications you provide are of course
 * out of our control.
 *
 * @author Michael J. Simons
 * @since 2021.3.0
 */
@API(status = STABLE, since = "2021.3.0")
public final class Options {

	private static final Options DEFAULT_OPTIONS = newOptions().build();

	private final BiFunction<LabelParsedEventType, Collection<String>, Collection<String>> labelFilter;

	private final BiFunction<TypeParsedEventType, Collection<String>, Collection<String>> typeFilter;

	private final Map<ExpressionCreatedEventType, List<Function<Expression, ? extends Expression>>> onNewExpressionCallbacks;

	private final Map<PatternElementCreatedEventType, List<UnaryOperator<PatternElement>>> onNewPatternElementCallbacks;

	private final Map<InvocationCreatedEventType, List<UnaryOperator<Visitable>>> onNewInvocationCallbacks;

	private final Function<ReturnDefinition, Return> returnClauseFactory;

	private final Function<MatchDefinition, Match> matchClauseFactory;

	private final boolean createSortedMaps;

	private final Map<String, Object> parameterValues;

	private boolean alwaysCreateRelationshipsLTR;

	private Options(Builder builder) {

		this.labelFilter = builder.labelFilter;
		this.typeFilter = builder.typeFilter;

		Map<ExpressionCreatedEventType, List<Function<Expression, ? extends Expression>>> tmp = new EnumMap<>(
				ExpressionCreatedEventType.class);
		builder.onNewExpressionCallbacks.forEach((k, v) -> tmp.put(k, List.copyOf(v)));
		this.onNewExpressionCallbacks = Map.copyOf(tmp);

		Map<PatternElementCreatedEventType, List<UnaryOperator<PatternElement>>> tmp2 = new EnumMap<>(
				PatternElementCreatedEventType.class);
		builder.onNewPatternElementCallbacks.forEach((k, v) -> tmp2.put(k, List.copyOf(v)));
		this.onNewPatternElementCallbacks = Map.copyOf(tmp2);

		Map<InvocationCreatedEventType, List<UnaryOperator<Visitable>>> tmp3 = new EnumMap<>(
				InvocationCreatedEventType.class);
		builder.onNewInvocationCallbacks.forEach((k, v) -> tmp3.put(k, List.copyOf(v)));
		this.onNewInvocationCallbacks = Map.copyOf(tmp3);

		this.returnClauseFactory = (builder.returnClauseFactory != null) ? builder.returnClauseFactory
				: returnDefinition -> Clauses.returning(returnDefinition.isDistinct(),
						returnDefinition.getExpressions(), returnDefinition.getOptionalSortItems(),
						returnDefinition.getOptionalSkip(), returnDefinition.getOptionalLimit());

		this.matchClauseFactory = (builder.matchClauseFactory != null) ? builder.matchClauseFactory
				: returnDefinition -> (Match) Clauses.match(returnDefinition.optional(),
						returnDefinition.patternElements(), returnDefinition.optionalWhere(),
						returnDefinition.optionalHints());

		this.createSortedMaps = builder.createSortedMaps;
		this.alwaysCreateRelationshipsLTR = builder.alwaysCreateRelationshipsLTR;
		this.parameterValues = builder.parameterValues;
	}

	/**
	 * {@return the default options}
	 */
	public static Options defaultOptions() {
		return DEFAULT_OPTIONS;
	}

	/**
	 * Use this method to start defining new options for a parser.
	 * @return a builder for new options.
	 */
	public static Builder newOptions() {
		return Builder.newConfig();
	}

	BiFunction<LabelParsedEventType, Collection<String>, Collection<String>> getLabelFilter() {
		return this.labelFilter;
	}

	BiFunction<TypeParsedEventType, Collection<String>, Collection<String>> getTypeFilter() {
		return this.typeFilter;
	}

	Map<ExpressionCreatedEventType, List<Function<Expression, ? extends Expression>>> getOnNewExpressionCallbacks() {
		return this.onNewExpressionCallbacks;
	}

	Map<PatternElementCreatedEventType, List<UnaryOperator<PatternElement>>> getOnNewPatternElementCallbacks() {
		return this.onNewPatternElementCallbacks;
	}

	Function<ReturnDefinition, Return> getReturnClauseFactory() {
		return this.returnClauseFactory;
	}

	Function<MatchDefinition, Match> getMatchClauseFactory() {
		return this.matchClauseFactory;
	}

	boolean isCreateSortedMaps() {
		return this.createSortedMaps;
	}

	boolean isAlwaysCreateRelationshipsLTR() {
		return this.alwaysCreateRelationshipsLTR;
	}

	Map<InvocationCreatedEventType, List<UnaryOperator<Visitable>>> getOnNewInvocationCallbacks() {
		return this.onNewInvocationCallbacks;
	}

	/**
	 * {@return <code>true</code> if these are the default options}
	 */
	boolean areDefault() {
		return this == DEFAULT_OPTIONS;
	}

	Map<String, Object> getParameterValues() {
		return this.parameterValues;
	}

	/**
	 * Use this builder to create a new set of options..
	 */
	@SuppressWarnings("HiddenField")
	public static final class Builder {

		private final Map<ExpressionCreatedEventType, List<Function<Expression, ? extends Expression>>> onNewExpressionCallbacks = new EnumMap<>(
				ExpressionCreatedEventType.class);

		private final Map<PatternElementCreatedEventType, List<UnaryOperator<PatternElement>>> onNewPatternElementCallbacks = new EnumMap<>(
				PatternElementCreatedEventType.class);

		private final Map<InvocationCreatedEventType, List<UnaryOperator<Visitable>>> onNewInvocationCallbacks = new EnumMap<>(
				InvocationCreatedEventType.class);

		private BiFunction<LabelParsedEventType, Collection<String>, Collection<String>> labelFilter = (e, l) -> l;

		private BiFunction<TypeParsedEventType, Collection<String>, Collection<String>> typeFilter = (e, t) -> t;

		private Function<ReturnDefinition, Return> returnClauseFactory;

		private Function<MatchDefinition, Match> matchClauseFactory;

		private Map<String, Object> parameterValues = Map.of();

		private boolean createSortedMaps = false;

		private boolean alwaysCreateRelationshipsLTR = false;

		private Builder() {
		}

		static Builder newConfig() {
			return new Builder();
		}

		/**
		 * Configure a filter that is applied to labels.
		 * @param labelFilter takes in an event type, a collection of labels and returns a
		 * probably new collection of labels.
		 * @return this builder.
		 */
		public Builder withLabelFilter(
				BiFunction<LabelParsedEventType, Collection<String>, Collection<String>> labelFilter) {

			if (labelFilter == null) {
				throw new IllegalArgumentException("Label filter may not be null.");
			}
			this.labelFilter = labelFilter;
			return this;
		}

		/**
		 * Configure a filter that is applied to types.
		 * @param typeFilter takes in an event type and the parsed type. May return the
		 * type itself or something else.
		 * @return this builder.
		 */
		public Builder withTypeFilter(
				BiFunction<TypeParsedEventType, Collection<String>, Collection<String>> typeFilter) {

			if (typeFilter == null) {
				throw new IllegalArgumentException("Type filter may not be null.");
			}
			this.typeFilter = typeFilter;
			return this;
		}

		/**
		 * Adds a callback for when the specific {@link ExpressionCreatedEventType
		 * expression is created} event. For one type of event one or more callbacks can
		 * be declared which will be called in order in which they have been declared.
		 * <p>
		 * Parsing will be aborted when a callback throws a {@link RuntimeException}.
		 * @param expressionCreatedEventType the type of the event
		 * @param resultingType the type of the expression the callback returns. Must
		 * match the one of the event type.
		 * @param callback a callback
		 * @param <T> the type of the expression produced by the callback. Must match the
		 * one of the event type
		 * @return this builder
		 */
		public <T extends Expression> Builder withCallback(ExpressionCreatedEventType expressionCreatedEventType,
				Class<T> resultingType, Function<Expression, T> callback) {

			if (!expressionCreatedEventType.getTypeProduced().isAssignableFrom(resultingType)) {
				throw new IllegalArgumentException("The type that is produced by '" + expressionCreatedEventType
						+ "' is not compatible with " + resultingType);
			}
			var callbacks = this.onNewExpressionCallbacks.computeIfAbsent(expressionCreatedEventType,
					k -> new ArrayList<>());
			callbacks.add(callback);
			return this;
		}

		/**
		 * Adds a callback for when a {@link PatternElement} is created during one of the
		 * phases described by {@link PatternElementCreatedEventType}. For one type of
		 * event one or more callbacks can be declared which will be called in order in
		 * which they have been declared. Callbacks can just collect or actually visit the
		 * elements created, or they are free to create new ones, effectively rewriting
		 * the query.
		 * <p>
		 * Parsing will be aborted when a callback throws a {@link RuntimeException}.
		 * @param patternElementCreatedEventType the type of the event
		 * @param callback a callback
		 * @return this builder
		 * @since 2022.2.0
		 */
		public Builder withCallback(PatternElementCreatedEventType patternElementCreatedEventType,
				UnaryOperator<PatternElement> callback) {

			var callbacks = this.onNewPatternElementCallbacks.computeIfAbsent(patternElementCreatedEventType,
					k -> new ArrayList<>());
			callbacks.add(callback);
			return this;
		}

		/**
		 * Adds a callback for when either a CALL-Procedure clause or a
		 * function-invocation expression is created. For one type of event one or more
		 * callbacks can be declared which will be called in order in which they have been
		 * declared. Callbacks can just collect or actually visit the elements created, or
		 * they are free to create new ones, effectively rewriting the query.
		 * <p>
		 * Parsing will be aborted when a callback throws a {@link RuntimeException}.
		 * @param invocationCreatedEventType the event type
		 * @param resultingType the reified type being produced
		 * @param callback a callback
		 * @param <T> the type of the result, must match the one of the event
		 * @return this builder
		 * @since 2022.8.6
		 */
		@SuppressWarnings("unchecked")
		public <T extends Visitable> Builder withCallback(InvocationCreatedEventType invocationCreatedEventType,
				Class<T> resultingType, UnaryOperator<T> callback) {

			if (!invocationCreatedEventType.getTypeProduced().isAssignableFrom(resultingType)) {
				throw new IllegalArgumentException("The type that is produced by '" + invocationCreatedEventType
						+ "' is not compatible with " + resultingType);
			}
			var callbacks = this.onNewInvocationCallbacks.computeIfAbsent(invocationCreatedEventType,
					k -> new ArrayList<>());
			callbacks.add((UnaryOperator<Visitable>) callback);
			return this;
		}

		/**
		 * Configures the factory for return clauses. The idea here is that you might
		 * intercept what is being returned or how it is sorted, limited and the like. The
		 * {@link ReturnDefinition definition} passed to the factory contains all
		 * necessary information for delegating to the
		 * {@link org.neo4j.cypherdsl.core.Clauses#returning(boolean, List, List, Expression, Expression)}
		 * factory.
		 * @param returnClauseFactory the factory producing return classes that should be
		 * used.
		 * @return this builder
		 */
		public Builder withReturnClauseFactory(Function<ReturnDefinition, Return> returnClauseFactory) {
			this.returnClauseFactory = returnClauseFactory;
			return this;
		}

		/**
		 * Configures the factory for return clauses. The idea here is that you might
		 * intercept what is being matched and or how it is restricted. The
		 * {@link MatchDefinition definition} passed to the factory contains all necessary
		 * information for delegating to the
		 * {@link org.neo4j.cypherdsl.core.Clauses#match(boolean, List, Where, List)}
		 * factory.
		 * @param matchClauseFactory the factory producing return classes that should be
		 * used.
		 * @return this builder
		 * @since 2023.0.2
		 */
		public Builder withMatchClauseFactory(Function<MatchDefinition, Match> matchClauseFactory) {
			this.matchClauseFactory = matchClauseFactory;
			return this;
		}

		/**
		 * Set {@code createSortedMaps} to {@literal true} to parse existing maps into
		 * alphabetically sorted maps.
		 * @param createSortedMaps a flag whether to create sorted maps or not
		 * @return this builder
		 * @since 2023.2.0
		 */
		public Builder createSortedMaps(boolean createSortedMaps) {
			this.createSortedMaps = createSortedMaps;
			return this;
		}

		/**
		 * Instructs the parser to turn relationships that are given as
		 * {@code (a:A) <-[:TYPE]- (b:B)} into {@code (b:B) -[:TYPE]-> (a:A)}. Multi-hop
		 * patterns will be split into a relationship enumeration from left to right with
		 * all parts pointing from left to right
		 * @param alwaysCreateRelationshipsLTR a flag whether to only use left-to-right
		 * relationships
		 * @return this builder
		 * @since 2023.9.3
		 */
		public Builder alwaysCreateRelationshipsLTR(boolean alwaysCreateRelationshipsLTR) {
			this.alwaysCreateRelationshipsLTR = alwaysCreateRelationshipsLTR;
			return this;
		}

		/**
		 * Defines a lookup table for parameters. Everytime a parameter is parsed, we do
		 * check if a value in this table exists. If so, the parameter will be created as
		 * a named parameter carrying that value.
		 * <p>
		 * Any previous lookup table will be overwritten when using this method multiple
		 * times.
		 * @param newParameterValues a new lookup table. Use an empty map or
		 * {@literal null} to clear any lookups in the config
		 * @return this builder
		 * @since 2023.4.0
		 */
		public Builder withParameterValues(Map<String, Object> newParameterValues) {
			this.parameterValues = (newParameterValues != null)
					? Collections.unmodifiableMap(new HashMap<>(newParameterValues)) : Map.of();
			return this;
		}

		/**
		 * Returns a new, unmodifiable {@link Options options instance}.
		 * @return a new, unmodifiable {@link Options options instance}
		 */
		public Options build() {
			return new Options(this);
		}

	}

}
