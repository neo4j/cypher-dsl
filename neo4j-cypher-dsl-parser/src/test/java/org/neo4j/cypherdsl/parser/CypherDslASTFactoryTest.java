/*
 * Copyright (c) 2019-2024 "Neo4j,"
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

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;

import java.lang.reflect.InvocationTargetException;
import java.util.List;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.neo4j.cypherdsl.core.Cypher;

/**
 * @author Michael J. Simons
 */
class CypherDslASTFactoryTest {

	@Test
	void isInstanceOfCheckShouldWork() {

		assertThatIllegalArgumentException().isThrownBy(() -> CypherDslASTFactory.isInstanceOf(null, null, null))
			.withMessage("Type to check against must not be null");

		assertThatIllegalArgumentException().isThrownBy(() -> CypherDslASTFactory.isInstanceOf(String.class, 42, "Not the answer"))
			.withMessage("Not the answer");
	}

	@Test
	void infinityLiteralShouldWork() {

		var factory = CypherDslASTFactory.getInstance(null);
		assertThat(factory.newInfinityLiteral(null)).isEqualTo(InfinityLiteral.INSTANCE);
	}

	@Test
	void nanLiteralShouldWork() {

		var factory = CypherDslASTFactory.getInstance(null);
		assertThat(factory.newNaNLiteral(null)).isEqualTo(NaNLiteral.INSTANCE);
	}

	@Test
	void parameterLiteralNullExpressionShouldWork() {

		var factory = CypherDslASTFactory.getInstance(null);
		var parameter = factory.parameterFromSymbolicName(null);
		assertThat(parameter).isNotNull();
		assertThat(parameter.isAnon()).isTrue();
	}

	@Test
	void parameterFromExpressionShouldWork() {

		var factory = CypherDslASTFactory.getInstance(null);
		var parameter = factory.parameterFromSymbolicName(CypherParser.parseExpression("a"));
		assertThat(parameter).isNotNull();
		assertThat(parameter.isAnon()).isFalse();
	}

	@Test
	void databaseName() {
		var factory = CypherDslASTFactory.getInstance(null);
		var databaseName = factory.databaseName(null, List.of("x"));
		assertThat(databaseName).isNotNull();

		databaseName = factory.databaseName(null, List.of("x", "y"));
		assertThat(databaseName).isNotNull();

		var parameter = Cypher.parameter("foo");
		databaseName = factory.databaseName(parameter);
		assertThat(databaseName).isNotNull();
		assertThat(databaseName.value()).isEqualTo(parameter);

		var empty = List.<String>of();
		assertThatIllegalArgumentException().isThrownBy(() -> factory.databaseName(null, empty))
			.withMessage("No database name");
	}

	@Nested
	class HandleNewMethods {

		@ParameterizedTest
		@ValueSource(strings = { "showAliases", "createLocalDatabaseAlias", "createRemoteDatabaseAlias",
			"alterLocalDatabaseAlias", "alterRemoteDatabaseAlias", "fixedPathQuantifier",
			"useGraph", "setOwnPassword", "addDeprecatedIdentifierUnicodeNotification",
			"showAllPrivileges", "showRolePrivileges", "showUserPrivileges", "createDatabase",
			"createCompositeDatabase", "dropDatabase",
			"showDatabase", "startDatabase", "stopDatabase", "createUser", "newSensitiveStringParameter",
			"newSensitiveStringParameter",
			"labelWildcard", "subqueryInTransactionsBatchParameters", "subqueryInTransactionsErrorParameters",
			"subqueryInTransactionsReportParameters",
			"showTransactionsClause", "terminateTransactionsClause", "turnYieldToWith", "alterDatabase",
			"settingQualifier", "showSettingsClause",
			"anyPathSelector", "allPathSelector", "anyShortestPathSelector", "allShortestPathSelector",
			"shortestGroupsSelector", "repeatableElements", "differentRelationships",
			"createConstraint", "showSupportedPrivileges", "isTyped", "isNotTyped", "functionUseClause", "isNormalized",
			"isNotNormalized", "normalizeExpression",
			"insertClause", "insertPathPattern", "subqueryInTransactionsBatchParameters",
			"subqueryInTransactionsConcurrencyParameters", "subqueryInTransactionsErrorParameters" })
		void newMethodsShouldNotBeSupportedOOTB(String methodName) {
			var factory = CypherDslASTFactory.getInstance(null);
			var methods = factory.getClass().getMethods();
			for (var method : methods) {
				if (method.getName().equals(methodName)) {
					Object[] args = new Object[method.getParameterCount()];
					int i = 0;
					for (Class<?> type : method.getParameterTypes()) {
						// Other primitives needs to be dealt with, but right now, not in the mood todo t his.
						args[i++] = type == boolean.class ? false : null;
					}
					assertThatExceptionOfType(InvocationTargetException.class)
						.isThrownBy(() -> method.invoke(factory, args))
						.withRootCauseInstanceOf(UnsupportedOperationException.class);
					return;
				}
			}

			Assertions.fail("Didn't find method " + methodName + " any more");
		}
	}
}
