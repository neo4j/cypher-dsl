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

import java.lang.reflect.Method;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.Mockito.mock;

/**
 * @author Michael J. Simons
 */
class PredicatesTests {

	private static final String FUNCTION_NAME_FIELD = "functionName";

	private static Stream<Arguments> predicatesToTest() {
		return Stream.of(Arguments.of("exists", Property.class), Arguments.of("exists", RelationshipPattern.class));
	}

	@ParameterizedTest
	@MethodSource("predicatesToTest")
	void preconditionsShouldBeAsserted(String predicateName, Class<?> argumentType) {

		@SuppressWarnings("deprecation")
		Method method = TestUtils.findMethod(Predicates.class, predicateName, argumentType);
		assertThatIllegalArgumentException().isThrownBy(() -> TestUtils.invokeMethod(method, null, (Expression) null))
			.withMessageEndingWith("is required.");
	}

	@ParameterizedTest
	@MethodSource("predicatesToTest")
	void functionInvocationsShouldBeCreated(String functionName, Class<?> argumentType) {

		@SuppressWarnings("deprecation")
		Method method = TestUtils.findMethod(Predicates.class, functionName, argumentType);
		BooleanFunctionCondition invocation = (BooleanFunctionCondition) TestUtils.invokeMethod(method, null,
				mock(argumentType));
		assertThat(invocation).extracting("delegate").hasFieldOrPropertyWithValue(FUNCTION_NAME_FIELD, functionName);
	}

}
