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

import reactor.core.publisher.Flux;

import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Stream;

import org.apiguardian.api.API;
import org.neo4j.driver.QueryRunner;
import org.neo4j.driver.Record;
import org.neo4j.driver.reactive.RxQueryRunner;
import org.neo4j.driver.summary.ResultSummary;

/**
 * A statement that returns items from the graph. The shape of those items can be pretty much anything:
 * A list of records containing only properties, or nodes with properties mixed with relationships and
 * so on. The only guarantee given is that the query will return some data if a match happens.
 * <p>
 * This class spots several methods that allow statements to be used with an instance of the Neo4j Java Driver.
 * To use any of the {@code fetchXXX} methods, the driver must be present on the class path. The corresponding Java module
 * is {@code org.neo4j.driver}.
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
 * The methods taking in any reactive type also require Project Reactor on the class path with
 * {@code reactor.core} being the corresponding Java module.
 *
 * @author Michael J. Simons
 * @soundtrack Motörhead - Live At Brixton Academy
 * @since 2021.2.1
 */
@API(status = EXPERIMENTAL, since = "2021.2.1")
public interface ResultStatement extends Statement {

	/**
	 * Fetches a list of things from a database via the given {@code queryRunner}. The {@code mappingFunction} is used
	 * for converting records into a custom types.
	 *
	 * @param queryRunner     Any type of query runner. Neither sessions nor transactions will be closed.
	 * @param mappingFunction A mapping function.
	 * @param <T>             The type of the returned objects.
	 * @return A list of objects.
	 */
	<T> List<T> fetchWith(QueryRunner queryRunner, Function<Record, T> mappingFunction);

	/**
	 * Fetches a list of {@link Record records} from a database via the given {@code queryRunner}.
	 *
	 * @param queryRunner Any type of query runner. Neither sessions nor transactions will be closed.
	 * @return A list of records.
	 */
	default List<Record> fetchWith(QueryRunner queryRunner) {
		return fetchWith(queryRunner, Function.identity());
	}

	/**
	 * Fetches a publisher of things from a database via the given {@code queryRunner} in a reactive fashion.
	 * The {@code mappingFunction} is used for converting records into a custom types.
	 *
	 * @param queryRunner     Any type of reactive query runner. Neither sessions nor transactions will be closed.
	 * @param mappingFunction A mapping function.
	 * @param <T>             The type of the returned objects
	 * @return A publisher of objects.
	 */
	<T> Flux<T> fetchWith(RxQueryRunner queryRunner, Function<Record, T> mappingFunction);

	/**
	 * Fetches a publisher of records from a database via the given {@code queryRunner} in a reactive fashion.
	 *
	 * @param queryRunner Any type of reactive query runner. Neither sessions nor transactions will be closed.
	 * @return A publisher of records.
	 */
	default Flux<Record> fetchWith(RxQueryRunner queryRunner) {
		return fetchWith(queryRunner, Function.identity());
	}

	/**
	 * This method creates (and closes) a stream of records from the result of a given query. The stream is not
	 * meant to live outside the scope of the consumer passed to this method as it should either be closed or fully
	 * consumed (we make sure of this).
	 * <p>
	 * This method is especially useful when profiling a statement: You will be able to safely process what's needed and
	 * also get hold of the result summary, which will include the profile.
	 *
	 * @param queryRunner   Any type of query runner. Neither sessions nor transactions will be closed
	 * @param streamHandler A handler for the stream: Do whatever you want with the items.
	 * @return The result summary. There is no need to actually use this.
	 */
	ResultSummary streamWith(QueryRunner queryRunner, Consumer<Stream<Record>> streamHandler);
}
