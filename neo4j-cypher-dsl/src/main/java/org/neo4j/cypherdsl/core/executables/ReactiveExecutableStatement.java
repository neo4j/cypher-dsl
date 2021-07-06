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
package org.neo4j.cypherdsl.core.executables;

import static org.apiguardian.api.API.Status.EXPERIMENTAL;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ResultStatement;
import org.neo4j.cypherdsl.core.Statement;
import org.neo4j.driver.reactive.RxQueryRunner;
import org.neo4j.driver.summary.ResultSummary;
import org.reactivestreams.Publisher;

/**
 * This is an extended version of the {@link ExecutableStatement}, spotting an additional {@link #executeWith(RxQueryRunner)}
 * that runs a statement in a reactive fashion.
 * <p>
 * The usage of this interface require Project Reactor on the class path with {@code org.reactivestreams} and
 * {@code reactor.core} being the corresponding Java modules.
 *
 * @author Michael J. Simons
 * @soundtrack Swiss + Die Andern - Randalieren für die Liebe
 * @since 2021.2.1
 */
@API(status = EXPERIMENTAL, since = "2021.2.1")
public interface ReactiveExecutableStatement extends ExecutableStatement {

	/**
	 * Creates an executable statement based on the given statement
	 *
	 * @param statement Any Cypher-DSL statement
	 * @return An executable statement. Maybe a {@link ExecutableResultStatement}, depending on the input.
	 * @see #of(Statement)
	 */
	static ReactiveExecutableStatement makeExecutable(Statement statement) {
		if (statement.doesReturnOrYield()) {
			return new DefaultReactiveExecutableResultStatement(statement);
		}
		return new DefaultReactiveExecutableStatement(statement);
	}

	/**
	 * Creates an executable result statement based on the given statement
	 *
	 * @param statement A  Cypher-DSL result statement
	 * @return An executable result statement.
	 * @see #of(ResultStatement)
	 */
	static ReactiveExecutableResultStatement makeExecutable(ResultStatement statement) {
		return new DefaultReactiveExecutableResultStatement(statement);
	}

	/**
	 * Creates an executable statement based on the given statement
	 *
	 * @param statement Any Cypher-DSL statement
	 * @return An executable statement. Maybe a {@link ExecutableResultStatement}, depending on the input.
	 * @see #makeExecutable(Statement)
	 */
	static ReactiveExecutableStatement of(Statement statement) {
		return makeExecutable(statement);
	}

	/**
	 * Creates an executable result statement based on the given statement
	 *
	 * @param statement A  Cypher-DSL result statement
	 * @return An executable result statement.
	 * @see #makeExecutable(ResultStatement)
	 */
	static ReactiveExecutableResultStatement of(ResultStatement statement) {
		return makeExecutable(statement);
	}

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
	Publisher<ResultSummary> executeWith(RxQueryRunner queryRunner);
}
