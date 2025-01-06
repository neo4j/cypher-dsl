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
package org.neo4j.cypherdsl.core.executables;

import static org.apiguardian.api.API.Status.STABLE;

import java.util.Collection;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.apiguardian.api.API;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.neo4j.cypherdsl.core.ResultStatement;
import org.neo4j.cypherdsl.core.Statement;
import org.neo4j.cypherdsl.core.StatementCatalog;
import org.neo4j.driver.QueryRunner;
import org.neo4j.driver.SimpleQueryRunner;
import org.neo4j.driver.async.AsyncQueryRunner;
import org.neo4j.driver.summary.ResultSummary;

/**
 * This interface spots several methods that allow statements to be used with an instance of the Neo4j Java Driver.
 * To use any of the {@code executeWithXXX} methods, the driver must be present on the class path. The corresponding Java module
 * is {@code org.neo4j.driver}.
 * <p>
 * An {@link ExecutableResultStatement} makes a {@link org.neo4j.cypherdsl.core.ResultStatement} executable.
 * <p>
 * We don't add any overhead to the process but only use what's already there. The {@link QueryRunner} passed to any
 * of the methods can be a session, a transaction or also the Spring Data Neo4j specific query runner.
 * <p>
 * <strong>We don't commit or rollback any transaction nor do we close a session. That is up to the caller.</strong>
 * <p>
 * Any parameter in this statement containing a value will be passed directly to the driver. The driver does
 * a relatively good job converting most parameters. If in doubt or the result is wrong, use an appropriate overload of
 * {@code org.neo4j.driver.Values#value(parameter)} to convert the parameter.
 * <p>
 * Use any of the {@link #of(Statement)} or {@link #makeExecutable(Statement)} methods to get an appropriate instance.
 * The {@code of} methods are useful when you are using qualified imports, the {@code makeExecutable}-variant comes in
 * handy with static imports.
 *
 * @author Michael J. Simons
 * @soundtrack Swiss + Die Andern - Randalieren für die Liebe
 * @since 2021.2.1
 */
@API(status = STABLE, since = "2021.2.1")
public interface ExecutableStatement {

	/**
	 * Creates an executable statement based on the given statement
	 *
	 * @param statement Any Cypher-DSL statement
	 * @return An executable statement. Maybe a {@link ExecutableResultStatement}, depending on the input.
	 * @see #of(Statement)
	 */
	static ExecutableStatement makeExecutable(Statement statement) {
		if (statement.doesReturnOrYield()) {
			return new DefaultExecutableResultStatement(statement);
		}
		return new DefaultExecutableStatement(statement);
	}

	/**
	 * Creates an executable result statement based on the given statement
	 *
	 * @param statement A  Cypher-DSL result statement
	 * @return An executable result statement.
	 * @see #of(ResultStatement)
	 */
	static ExecutableResultStatement makeExecutable(ResultStatement statement) {
		return new DefaultExecutableResultStatement(statement);
	}

	/**
	 * Creates an executable statement based on the given statement
	 *
	 * @param statement Any Cypher-DSL statement
	 * @return An executable statement. Maybe a {@link ExecutableResultStatement}, depending on the input.
	 * @see #makeExecutable(Statement)
	 */
	static ExecutableStatement of(Statement statement) {
		return makeExecutable(statement);
	}

	/**
	 * Creates an executable result statement based on the given statement
	 *
	 * @param statement A  Cypher-DSL result statement
	 * @return An executable result statement.
	 * @see #makeExecutable(ResultStatement)
	 */
	static ExecutableResultStatement of(ResultStatement statement) {
		return makeExecutable(statement);
	}

	/**
	 * @return A map of all parameters with a bound value.
	 * @see StatementCatalog#getParameters()
	 */
	@NotNull @Contract(pure = true)
	Map<String, Object> getParameters();

	/**
	 * @return A set of parameter names being used.
	 * @see StatementCatalog#getParameterNames()
	 */
	@NotNull @Contract(pure = true)
	Collection<String> getParameterNames();

	/**
	 * @return A valid Cypher statement
	 * @see Statement#getCypher()
	 */
	@NotNull @Contract(pure = true)
	String getCypher();

	/**
	 * If the Neo4j Java Driver is on the classpath, this method can be used to pass a {@link QueryRunner} to the statement,
	 * execute it and retrieve a result summary.
	 * <p>
	 * All parameters with given values will be passed to the database. Please have a look at {@link ResultStatement} for
	 * further details about parameter conversions
	 * <p>
	 * No resources passed to this method (neither sessions, transactions nor other query runners) will be closed.
	 * Resources opened inside the method will be closed.
	 *
	 * @param queryRunner a query runner
	 * @return A result summary (including server information, counters etc).
	 */
	ResultSummary executeWith(SimpleQueryRunner queryRunner);

	/**
	 * If the Neo4j Java Driver is on the classpath, this method can be used to pass a {@link QueryRunner} to the statement,
	 * execute it and retrieve a result summary in an asynchronous fashion
	 * <p>
	 * All parameters with given values will be passed to the database. Please have a look at {@link ResultStatement} for
	 * further details about parameter conversions
	 * <p>
	 * No resources passed to this method (neither sessions, transactions nor other query runners) will be closed.
	 * Resources opened inside the method will be closed.
	 *
	 * @param queryRunner a query runner
	 * @return A completable future of a result summary (including server information, counters etc).
	 */
	CompletableFuture<ResultSummary> executeWith(AsyncQueryRunner queryRunner);
}
