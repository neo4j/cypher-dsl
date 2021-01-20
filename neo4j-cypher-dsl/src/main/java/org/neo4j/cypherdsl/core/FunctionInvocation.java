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
import static org.apiguardian.api.API.Status.INTERNAL;

import java.util.Collections;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.support.TypedSubtree;
import org.neo4j.cypherdsl.core.support.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * See
 * <a href="https://s3.amazonaws.com/artifacts.opencypher.org/railroad/FunctionInvocation.html">FunctionInvocation</a>
 *
 * @author Gerrit Meier
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = EXPERIMENTAL, since = "1.0")
public final class FunctionInvocation implements Expression {

	/**
	 * Defines metadata for a function.
	 */
	@API(status = EXPERIMENTAL, since = "2020.1.0")
	interface FunctionDefinition {

		String getImplementationName();

		default boolean isAggregate() {
			return false;
		}
	}

	static FunctionInvocation create(FunctionDefinition definition) {

		return new FunctionInvocation(definition.getImplementationName(), new Expression[0]);
	}

	static FunctionInvocation create(FunctionDefinition definition, Expression... expressions) {

		String message = "The expression for " + definition.getImplementationName() + "() is required.";

		Assertions.notEmpty(expressions, message);
		Assertions.notNull(expressions[0], message);

		return new FunctionInvocation(definition.getImplementationName(), expressions);
	}

	static FunctionInvocation createDistinct(FunctionDefinition definition, Expression... expressions) {

		Assertions
			.isTrue(definition.isAggregate(), "The distinct operator can only be applied within aggregate functions.");

		String message = "The expression for " + definition.getImplementationName() + "() is required.";

		Assertions.notEmpty(expressions, message);
		Assertions.notNull(expressions[0], message);

		Expression[] newExpressions = new Expression[expressions.length];
		newExpressions[0] = new DistinctExpression(expressions[0]);
		System.arraycopy(expressions, 1, newExpressions, 1, expressions.length - 1);

		return new FunctionInvocation(definition.getImplementationName(), newExpressions);
	}

	static FunctionInvocation create(FunctionDefinition definition, PatternElement pattern) {

		Assertions.notNull(pattern, "The pattern for " + definition.getImplementationName() + "() is required.");

		return new FunctionInvocation(definition.getImplementationName(),
			new Pattern(Collections.singletonList(pattern)));
	}

	static FunctionInvocation create(FunctionDefinition definition, TypedSubtree<?, ?> arguments) {

		Assertions.notNull(arguments, definition.getImplementationName() + "() requires at least one argument.");

		return new FunctionInvocation(definition.getImplementationName(), arguments);
	}

	private final String functionName;

	private final TypedSubtree<?, ?> arguments;

	private FunctionInvocation(String functionName, Expression... arguments) {

		this.functionName = functionName;
		this.arguments = new ExpressionList(arguments);
	}

	private <T extends TypedSubtree<?, ?>> FunctionInvocation(String functionName, T pattern) {

		this.functionName = functionName;
		this.arguments = pattern;
	}

	/**
	 * @return The name of this function.
	 */
	@API(status = INTERNAL)
	public String getFunctionName() {
		return functionName;
	}

	@Override
	public void accept(Visitor visitor) {
		visitor.enter(this);
		this.arguments.accept(visitor);
		visitor.leave(this);
	}

	@Override public String toString() {
		return "FunctionInvocation{" +
			"functionName='" + functionName + '\'' +
			'}';
	}
}
