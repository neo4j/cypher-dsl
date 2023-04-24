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
		var parameter = CypherDslASTFactory.parameterFromSymbolicName(null);
		assertThat(parameter).isNotNull();
		assertThat(parameter.isAnon()).isTrue();
	}

	@Test
	void parameterFromExpressionShouldWork() {
		var parameter = CypherDslASTFactory.parameterFromSymbolicName(CypherParser.parseExpression("a"));
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
		@ValueSource(strings = {"showAliases", "createLocalDatabaseAlias", "createRemoteDatabaseAlias",
			"alterLocalDatabaseAlias", "alterRemoteDatabaseAlias", "intervalPathQuantifier", "fixedPathQuantifier", "plusPathQuantifier",
			"starPathQuantifier", "parenthesizedPathPattern", "quantifiedRelationship", "useGraph", "setOwnPassword",
			"showAllPrivileges", "showRolePrivileges", "showUserPrivileges", "createDatabase", "createCompositeDatabase", "dropDatabase",
			"showDatabase", "startDatabase", "stopDatabase", "createUser", "newSensitiveStringParameter", "newSensitiveStringParameter",
			"labelWildcard", "subqueryInTransactionsBatchParameters", "subqueryInTransactionsErrorParameters", "subqueryInTransactionsReportParameters",
			"showTransactionsClause", "terminateTransactionsClause", "turnYieldToWith", "alterDatabase", "settingQualifier", "showSettingsClause", "collectExpression",
			"anyPathSelector", "allPathSelector", "anyShortestPathSelector", "allShortestPathSelector", "shortestGroupsSelector"})
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
