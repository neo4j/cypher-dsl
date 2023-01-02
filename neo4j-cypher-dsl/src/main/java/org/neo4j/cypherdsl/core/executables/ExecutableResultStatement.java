/*
 * Copyright (c) 2019-2023 "Neo4j,"
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

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Stream;

import org.neo4j.driver.QueryRunner;
import org.neo4j.driver.Record;
import org.neo4j.driver.async.AsyncQueryRunner;
import org.neo4j.driver.summary.ResultSummary;

/**
 * This interface extends {@link ExecutableStatement} and adds several {@code fetchWithXXX} methods that can be used
 * with any {@link org.neo4j.cypherdsl.core.ResultStatement} to retrieve their results.
 * <p>
 * The same requirements for the needed classes apply as with {@link ExecutableStatement}.
 *
 * @author Michael J. Simons
 * @soundtrack Swiss + Die Andern - Randalieren für die Liebe
 * @since 2021.2.1
 */
public interface ExecutableResultStatement extends ExecutableStatement {

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
	 * Asynchronously fetches a list of records from a database via the given {@code queryRunner}.
	 * The {@code mappingFunction} is used for converting records into a custom types.
	 *
	 * @param queryRunner     Any type of asynchronous query runner. Neither sessions nor transactions will be closed.
	 * @param mappingFunction A mapping function.
	 * @param <T>             The type of the returned objects
	 * @return A completable future of a list of records
	 */
	<T> CompletableFuture<List<T>> fetchWith(AsyncQueryRunner queryRunner, Function<Record, T> mappingFunction);

	/**
	 * Asynchronously fetches a list of records from a database via the given {@code queryRunner}.
	 *
	 * @param queryRunner Any type of asynchronous query runner. Neither sessions nor transactions will be closed.
	 * @return A completable future of a list of records
	 */
	default CompletableFuture<List<Record>> fetchWith(AsyncQueryRunner queryRunner) {
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
