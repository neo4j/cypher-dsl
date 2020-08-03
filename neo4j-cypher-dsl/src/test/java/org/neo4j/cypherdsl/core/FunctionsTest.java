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

import static org.assertj.core.api.Assertions.*;
import static org.mockito.Mockito.*;
import static org.neo4j.cypherdsl.core.TestUtils.*;

import java.lang.reflect.Method;
import java.util.stream.Stream;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.Answers;

/**
 * @author Michael J. Simons
 */
class FunctionsTest {

	private static final String FUNCTION_NAME_FIELD = "functionName";

	@Nested
	class coalesce {

		@Test
		void preconditionsShouldBeAsserted() {

			String message = "The expression for coalesce() is required.";
			assertThatIllegalArgumentException().isThrownBy(() -> Functions.coalesce((Expression[]) null))
				.withMessage(message);

			assertThatIllegalArgumentException().isThrownBy(() -> Functions.coalesce(new Expression[0]))
				.withMessage(message);

			assertThatIllegalArgumentException().isThrownBy(() -> Functions.coalesce(new Expression[] { null }))
				.withMessage(message);
		}

		@Test
		void shouldCreateCorrectInvocation() {

			FunctionInvocation invocation = Functions.coalesce(mock(Expression.class));
			assertThat(invocation).hasFieldOrPropertyWithValue(FUNCTION_NAME_FIELD, "coalesce");
		}
	}

	private static Stream<Arguments> functionsToTest() {
		return Stream.of(
			Arguments.of("id", Node.class),
			Arguments.of("id", Relationship.class),
			Arguments.of("count", Expression.class),
			Arguments.of("collect", Expression.class),
			Arguments.of("head", Expression.class),
			Arguments.of("last", Expression.class),
			Arguments.of("size", RelationshipPattern.class),
			Arguments.of("size", Expression.class)

		);
	}

	@ParameterizedTest
	@MethodSource("functionsToTest")
	void preconditionsShouldBeAsserted(String functionName, Class<?> argumentType) {

		Method method = findMethod(Functions.class, functionName, argumentType);
		assertThatIllegalArgumentException()
			.isThrownBy(() -> invokeMethod(method, null, (Expression) null))
			.withMessageMatching("The (node|relationship|expression|pattern) for " + functionName + "\\(\\) is required.");
	}

	@ParameterizedTest
	@MethodSource("functionsToTest")
	void functionInvocationsShouldBeCreated(String functionName, Class<?> argumentType) {

		Method method = findMethod(Functions.class, functionName, argumentType);
		FunctionInvocation invocation = (FunctionInvocation) invokeMethod(method, null, mock(argumentType, Answers.RETURNS_DEEP_STUBS));
		assertThat(invocation).hasFieldOrPropertyWithValue(FUNCTION_NAME_FIELD, functionName);
	}
}
