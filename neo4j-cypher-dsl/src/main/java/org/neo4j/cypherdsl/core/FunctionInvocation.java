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

import java.text.MessageFormat;
import java.util.Arrays;
import java.util.List;
import java.util.function.Predicate;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.TypedSubtree;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

import static org.apiguardian.api.API.Status.INTERNAL;
import static org.apiguardian.api.API.Status.STABLE;

/**
 * See <a href=
 * "https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/FunctionInvocation.html">FunctionInvocation</a>.
 *
 * @author Gerrit Meier
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public final class FunctionInvocation implements Expression {

	private static final MessageFormat MESSAGE_FMT_EXP_REQUIRED = new MessageFormat(
			Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSION_FOR_FUNCTION_REQUIRED));

	private static final MessageFormat MESSAGE_FMT_PATTERN_REQUIRED = new MessageFormat(
			Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_PATTERN_FOR_FUNCTION_REQUIRED));

	private static final MessageFormat MESSAGE_FMT_ARG_REQUIRED = new MessageFormat(
			Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_AT_LEAST_ONE_ARG_REQUIRED));

	private final String functionName;

	private final Visitable arguments;

	private FunctionInvocation(String functionName, Expression... arguments) {

		this.functionName = functionName;
		this.arguments = new ExpressionList(arguments);
	}

	private FunctionInvocation(String functionName, Visitable arguments) {

		this.functionName = functionName;
		this.arguments = arguments;
	}

	/**
	 * Creates a {@link FunctionInvocation} based on a simple definition without any
	 * arguments.
	 * @param definition the definition of a function
	 * @return the invocation (a valid expression)
	 * @since 2021.2.3
	 */
	public static FunctionInvocation create(FunctionDefinition definition) {

		return new FunctionInvocation(definition.getImplementationName());
	}

	/**
	 * Creates a {@link FunctionInvocation} based on a simple definition with arguments.
	 * @param definition the definition of a function
	 * @param expressions the arguments to the function
	 * @return the invocation (a valid expression)
	 * @since 2021.2.3
	 */
	public static FunctionInvocation create(FunctionDefinition definition, Expression... expressions) {

		String message = MESSAGE_FMT_EXP_REQUIRED.format(new Object[] { definition.getImplementationName() });

		Assertions.notEmpty(expressions, message);
		Assertions.notNull(expressions[0], message);

		return new FunctionInvocation(definition.getImplementationName(), expressions);
	}

	/**
	 * Creates a {@link FunctionInvocation} based on a simple definition with arguments
	 * and adds the {@code distinct} operator to it. This is only supported with
	 * {@link FunctionDefinition#isAggregate()} returning {@literal true}.
	 * @param definition the definition of a function
	 * @param expressions the arguments to the function
	 * @return the invocation (a valid expression)
	 * @since 2021.2.3
	 */
	public static FunctionInvocation createDistinct(FunctionDefinition definition, Expression... expressions) {

		Assertions.isTrue(definition.isAggregate(),
				Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_CORRECT_USAGE_OF_DISTINCT));

		String message = MESSAGE_FMT_EXP_REQUIRED.format(new Object[] { definition.getImplementationName() });

		Assertions.notEmpty(expressions, message);
		Assertions.notNull(expressions[0], message);

		Expression[] newExpressions = new Expression[expressions.length];
		newExpressions[0] = new DistinctExpression(expressions[0]);
		System.arraycopy(expressions, 1, newExpressions, 1, expressions.length - 1);

		return new FunctionInvocation(definition.getImplementationName(), newExpressions);
	}

	/**
	 * Creates a new function invocation for a pattern element.
	 * @param definition the definition of the function
	 * @param pattern the argument to the function
	 * @return a function invocation
	 * @since 2021.2.3
	 */
	public static FunctionInvocation create(FunctionDefinition definition, PatternElement pattern) {

		String message = MESSAGE_FMT_PATTERN_REQUIRED.format(new Object[] { definition.getImplementationName() });
		Assertions.notNull(pattern, message);

		Predicate<FunctionDefinition> isShortestPath = d -> BuiltInFunctions.Scalars.SHORTEST_PATH
			.getImplementationName()
			.equals(d.getImplementationName()) || "allShortestPaths".equals(d.getImplementationName());

		return new FunctionInvocation(definition.getImplementationName(),
				isShortestPath.test(definition) ? Pattern.of(List.of(pattern)) : new PatternExpressionImpl(pattern));
	}

	static FunctionInvocation create(FunctionDefinition definition, TypedSubtree<?> arguments) {

		Assertions.notNull(arguments,
				MESSAGE_FMT_ARG_REQUIRED.format(new Object[] { definition.getImplementationName() }));

		return new FunctionInvocation(definition.getImplementationName(), arguments);
	}

	/**
	 * {@return the name of this function}
	 */
	@API(status = INTERNAL)
	public String getFunctionName() {
		return this.functionName;
	}

	@Override
	public void accept(Visitor visitor) {
		visitor.enter(this);
		this.arguments.accept(visitor);
		visitor.leave(this);
	}

	@Override
	public String toString() {
		return RendererBridge.render(this);
	}

	/**
	 * Defines metadata for a function.
	 */
	@API(status = STABLE, since = "2020.1.0")
	public interface FunctionDefinition {

		/**
		 * {@return the Cypher implementation name}
		 */
		String getImplementationName();

		/**
		 * {@return <code>true</code> if this is an aggregating function}
		 */
		default boolean isAggregate() {
			return Arrays.stream(BuiltInFunctions.Aggregates.values())
				.map(BuiltInFunctions.Aggregates::getImplementationName)
				.anyMatch(v -> v.equalsIgnoreCase(getImplementationName()));
		}

	}

}
