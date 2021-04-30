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

import reactor.core.publisher.Mono;

import java.util.Collection;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.apiguardian.api.API;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.neo4j.cypherdsl.core.StatementBuilder.OngoingStandaloneCallWithoutArguments;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.internal.ProcedureName;
import org.neo4j.cypherdsl.core.internal.StatementContext;
import org.neo4j.driver.QueryRunner;
import org.neo4j.driver.async.AsyncQueryRunner;
import org.neo4j.driver.reactive.RxQueryRunner;
import org.neo4j.driver.summary.ResultSummary;

/**
 * Shall be the common interfaces for queries that we support.
 * <p>
 * For reference see: <a href="https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/Cypher.html">Cypher</a>.
 * We have skipped the intermediate "Query" structure so a statement in the context of this generator is either a
 * {@link RegularQuery} or a {@code StandaloneCall}.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = EXPERIMENTAL, since = "1.0")
public interface Statement extends Visitable {

	/**
	 * @return A new statement builder.
	 */
	@NotNull @Contract(pure = true)
	static StatementBuilder builder() {

		return new DefaultStatementBuilder();
	}

	/**
	 * @param namespaceAndProcedure The fully qualified name of a stored procedure. Each part can be given as a separate
	 *                              String, but a fully qualified String is ok as well.
	 * @return An entry point into a statement that starts with a call to stored procedure.
	 */
	@NotNull @Contract(pure = true)
	static OngoingStandaloneCallWithoutArguments call(String... namespaceAndProcedure) {

		return new DefaultStatementBuilder.StandaloneCallBuilder(ProcedureName.from(namespaceAndProcedure));
	}

	/**
	 * After a statement has been build, all parameters that have been added to the statement can be retrieved through
	 * this method. The result will only contain parameters with a defined value. If you are interested in all parameter
	 * names, use {@link #getParameterNames()}.
	 * <p>
	 * The map can be used for example as an argument with various methods on the Neo4j Java Driver that allow the
	 * execution of parameterized queries.
	 * <p>
	 * This method is threadsafe
	 *
	 * @return A map of all parameters with a bound value.
	 * @since 2021.0.0
	 */
	@NotNull @Contract(pure = true)
	Map<String, Object> getParameters();

	/**
	 * After the statement has been build, this method returns a list of all parameter names used, regardless whether
	 * a value was bound to the parameter o not.
	 * <p>
	 * This method is threadsafe
	 *
	 * @return A set of parameter names being used.
	 * @since 2021.0.0
	 */
	@NotNull @Contract(pure = true)
	Collection<String> getParameterNames();

	/**
	 * This method uses the default renderer to create a String representation of this statement. The generated Cypher
	 * will use escaped literals and correct placeholders like {@code $param} for parameters. The placeholders for
	 * parameters can be retrieved via {@link #getParameterNames}. Bounded values for parameters can be retrieved via
	 * {@link #getParameters()}.
	 * <p>
	 * This method is threadsafe
	 *
	 * @return A valid Cypher statement
	 * @since 2021.0.0
	 */
	@NotNull @Contract(pure = true)
	String getCypher();

	@API(status = INTERNAL, since = "2021.0.0")
	@NotNull @Contract(pure = true)
	StatementContext getContext();

	/**
	 * Some constants may be rendered as parameters.
	 *
	 * @return True if literal parameters hav
	 */
	@NotNull @Contract(pure = true)
	boolean isRenderConstantsAsParameters();

	/**
	 * Use this method to configure whether some constant values should be rendered as parameters or as literals before
	 * the first call to {@link Statement#getParameters()} or {@link Statement#getCypher()}.
	 * <p>
	 * Renderers are free to chose to ignore this.
	 *
	 * @param renderConstantsAsParameters Set to true to render constants as parameters (when using {@link #getCypher()}.
	 */
	void setRenderConstantsAsParameters(boolean renderConstantsAsParameters);

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
	 * @since 2021.2.1
	 */
	ResultSummary executeWith(QueryRunner queryRunner);

	/**
	 * If the Neo4j Java Driver is on the classpath, this method can be used to pass a {@link RxQueryRunner} to the statement,
	 * execute it and retrieve a result summary in a reactive fashion.
	 * This method also requires Project Reactor to be available.
	 * <p>
	 * All parameters with given values will be passed to the database. Please have a look at {@link ResultStatement} for
	 * further details about parameter conversions
	 * <p>
	 * No resources passed to this method (neither sessions, transactions nor other query runners) will be closed.
	 * Resources opened inside the method will be closed.
	 * <p>
	 * No statement will be generated and no parameters will be converted unless something subscribes to the result.
	 *
	 * @param queryRunner a reactive query runner
	 * @return A publisher of a result summary (including server information, counters etc).
	 * @since 2021.2.1
	 */
	Mono<ResultSummary> executeWith(RxQueryRunner queryRunner);

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
	 * @since 2021.2.1
	 */
	CompletableFuture<ResultSummary> executeWith(AsyncQueryRunner queryRunner);

	/**
	 * Represents {@code RegularQuery}.
	 *
	 * @since 1.0
	 */
	interface RegularQuery extends Statement {
	}

	/**
	 * Represents a {@code SingleQuery}.
	 *
	 * @since 1.0
	 */
	interface SingleQuery extends RegularQuery {
	}
}
