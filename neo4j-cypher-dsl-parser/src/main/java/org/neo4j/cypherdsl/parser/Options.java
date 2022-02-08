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

import static org.apiguardian.api.API.Status.STABLE;

import java.util.ArrayList;
import java.util.Collection;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.UnaryOperator;

import org.apiguardian.api.API;
import org.jetbrains.annotations.Nullable;
import org.neo4j.cypherdsl.core.Clauses;
import org.neo4j.cypherdsl.core.Expression;
import org.neo4j.cypherdsl.core.PatternElement;
import org.neo4j.cypherdsl.core.Return;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * Provides arguments to the {@link CypherParser cypher parser}. The options itself are thread safe and can be reused.
 * The listener and modifications you provide are of course out of our control.
 *
 * @author Michael J. Simons
 * @soundtrack Queen - The Game
 * @since 2021.3.0
 */
@API(status = STABLE, since = "2021.3.0")
public final class Options {

	private static final Options DEFAULT_OPTIONS = newOptions().build();

	/**
	 * @return the default options
	 */
	public static Options defaultOptions() {
		return DEFAULT_OPTIONS;
	}

	/**
	 * Use this method to start defining new options for a parser.
	 *
	 * @return A builder for new options.
	 */
	public static Builder newOptions() {
		return Builder.newConfig();
	}

	/**
	 * Use this builder to create a new set of options..
	 */
	@SuppressWarnings("HiddenField")
	public static final class Builder {

		private BiFunction<LabelParsedEventType, Collection<String>, Collection<String>> labelFilter = (e, l) -> l;

		private BiFunction<TypeParsedEventType, Collection<String>, Collection<String>> typeFilter = (e, t) -> t;

		private final Map<ExpressionCreatedEventType, List<Function<Expression, ? extends Expression>>> onNewExpressionCallbacks
			= new EnumMap<>(ExpressionCreatedEventType.class);

		private final Map<PatternElementCreatedEventType, List<UnaryOperator<PatternElement>>> onNewPatternElementCallbacks
			= new EnumMap<>(PatternElementCreatedEventType.class);

		private Function<ReturnDefinition, Return> returnClauseFactory;

		private Builder() {
		}

		static Builder newConfig() {
			return new Builder();
		}

		/**
		 * Configure a filter that is applied to labels.
		 *
		 * @param labelFilter Takes in an event type, a collection of labels and returns a probably new collection of labels.
		 * @return This builder.
		 */
		public Builder withLabelFilter(BiFunction<LabelParsedEventType, Collection<String>, Collection<String>> labelFilter) {

			Assertions.notNull(labelFilter, "Label filter may not be null.");
			this.labelFilter = labelFilter;
			return this;
		}

		/**
		 * Configure a filter that is applied to types.
		 *
		 * @param typeFilter Takes in an event type and the parsed type. May return the type itself or something else.
		 * @return This builder.
		 */
		public Builder withTypeFilter(BiFunction<TypeParsedEventType, Collection<String>, Collection<String>> typeFilter) {

			Assertions.notNull(typeFilter, "Type filter may not be null.");
			this.typeFilter = typeFilter;
			return this;
		}

		/**
		 * Adds a callback for when the specific {@link ExpressionCreatedEventType expression is created} event. For one type
		 * of event one or more callbacks can be declared which will be called in order in which they have been declared.
		 * <p>
		 * Parsing will be aborted when a callback throws a {@link RuntimeException}.
		 *
		 * @param expressionCreatedEventType The type of the event
		 * @param resultingType              The type of the expression the callback returns. Must match the one of the event type.
		 * @param callback                   A callback
		 * @param <T>                        The type of the expression produced by the callback. Must match the one of the event type
		 * @return This builder
		 */
		public <T extends Expression> Builder withCallback(ExpressionCreatedEventType expressionCreatedEventType,
			Class<T> resultingType, Function<Expression, T> callback) {

			if (!expressionCreatedEventType.getTypeProduced().isAssignableFrom(resultingType)) {
				throw new IllegalArgumentException(
					"The type that is produced by '" + expressionCreatedEventType
					+ "' is not compatible with " + resultingType);
			}
			var callbacks = this.onNewExpressionCallbacks
				.computeIfAbsent(expressionCreatedEventType, k -> new ArrayList<>());
			callbacks.add(callback);
			return this;
		}

		/**
		 * Adds a callback for when a {@link PatternElement} is created during one of the phases described by {@link PatternElementCreatedEventType}.
		 * For one type of event one or more callbacks can be declared which will be called in order in which they have been declared.
		 * Callbacks can just collect or actually visit the elements created or they are free to create new ones, effectively rewriting the query.
		 * <p>
		 * Parsing will be aborted when a callback throws a {@link RuntimeException}.
		 *
		 * @param patternElementCreatedEventType The type of the event
		 * @param callback                   A callback
		 * @return This builder
		 * @since 2022.2.0
		 */
		public Builder withCallback(PatternElementCreatedEventType patternElementCreatedEventType, UnaryOperator<PatternElement> callback) {

			var callbacks = this.onNewPatternElementCallbacks
				.computeIfAbsent(patternElementCreatedEventType, k -> new ArrayList<>());
			callbacks.add(callback);
			return this;
		}

		/**
		 * Configures the factory for return clauses. The idea here is that you might intercept what is being returned
		 * or how it is sorted, limited and the like. The {@link ReturnDefinition definition} passed to the factory contains
		 * all necessary information for delegating to the {@link org.neo4j.cypherdsl.core.Clauses#returning(boolean, List, List, Expression, Expression)}
		 * factory.
		 *
		 * @param returnClauseFactory The factory producing return classes that should be used.
		 * @return This builder
		 */
		public Builder withReturnClauseFactory(@Nullable Function<ReturnDefinition, Return> returnClauseFactory) {
			this.returnClauseFactory = returnClauseFactory;
			return this;
		}

		/**
		 * @return A new, unmodifiable {@link Options options instance}.
		 */
		public Options build() {
			return new Options(this);
		}
	}

	private final BiFunction<LabelParsedEventType, Collection<String>, Collection<String>> labelFilter;

	private final BiFunction<TypeParsedEventType, Collection<String>, Collection<String>> typeFilter;

	private final Map<ExpressionCreatedEventType, List<Function<Expression, ? extends Expression>>> onNewExpressionCallbacks;

	private final Map<PatternElementCreatedEventType, List<UnaryOperator<PatternElement>>> onNewPatternElementCallbacks;

	private final Function<ReturnDefinition, Return> returnClauseFactory;

	private Options(Builder builder) {

		this.labelFilter = builder.labelFilter;
		this.typeFilter = builder.typeFilter;

		Map<ExpressionCreatedEventType, List<Function<Expression, ? extends Expression>>> tmp = new EnumMap<>(ExpressionCreatedEventType.class);
		builder.onNewExpressionCallbacks.forEach((k, v) -> tmp.put(k, List.copyOf(v)));
		this.onNewExpressionCallbacks = Map.copyOf(tmp);

		Map<PatternElementCreatedEventType, List<UnaryOperator<PatternElement>>> tmp2 = new EnumMap<>(PatternElementCreatedEventType.class);
		builder.onNewPatternElementCallbacks.forEach((k, v) -> tmp2.put(k, List.copyOf(v)));
		this.onNewPatternElementCallbacks = Map.copyOf(tmp2);

		this.returnClauseFactory = builder.returnClauseFactory != null ?
			builder.returnClauseFactory :
			returnDefinition -> Clauses
				.returning(returnDefinition.isDistinct(), returnDefinition.getExpressions(),
					returnDefinition.getOptionalSortItems(),
					returnDefinition.getOptionalSkip(), returnDefinition.getOptionalLimit());
	}

	BiFunction<LabelParsedEventType, Collection<String>, Collection<String>> getLabelFilter() {
		return labelFilter;
	}

	BiFunction<TypeParsedEventType, Collection<String>, Collection<String>> getTypeFilter() {
		return typeFilter;
	}

	Map<ExpressionCreatedEventType, List<Function<Expression, ? extends Expression>>> getOnNewExpressionCallbacks() {
		return onNewExpressionCallbacks;
	}

	Map<PatternElementCreatedEventType, List<UnaryOperator<PatternElement>>> getOnNewPatternElementCallbacks() {
		return onNewPatternElementCallbacks;
	}

	Function<ReturnDefinition, Return> getReturnClauseFactory() {
		return returnClauseFactory;
	}

	/**
	 * @return True if this are the default options.
	 */
	boolean areDefault() {
		return this == DEFAULT_OPTIONS;
	}
}
