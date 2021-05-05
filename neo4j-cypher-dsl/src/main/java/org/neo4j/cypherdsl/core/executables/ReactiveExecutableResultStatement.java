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

import java.util.function.Function;

import org.neo4j.driver.Record;
import org.neo4j.driver.reactive.RxQueryRunner;
import org.reactivestreams.Publisher;

/**
 * This interface extends {@link ReactiveExecutableStatement} and adds several {@code fetchWithXXX} methods that can be used
 * with any {@link org.neo4j.cypherdsl.core.ResultStatement} to retrieve their results in a reactive fashion.
 * <p>
 * The same requirements for the needed classes apply as with {@link ReactiveExecutableStatement}.
 *
 * @author Michael J. Simons
 * @soundtrack Swiss + Die Andern - Randalieren für die Liebe
 * @since 2021.2.1
 */
public interface ReactiveExecutableResultStatement extends ReactiveExecutableStatement {

	/**
	 * Fetches a publisher of things from a database via the given {@code queryRunner} in a reactive fashion.
	 * The {@code mappingFunction} is used for converting records into a custom types.
	 *
	 * @param queryRunner     Any type of reactive query runner. Neither sessions nor transactions will be closed.
	 * @param mappingFunction A mapping function.
	 * @param <T>             The type of the returned objects
	 * @return A publisher of objects.
	 */
	<T> Publisher<T> fetchWith(RxQueryRunner queryRunner, Function<Record, T> mappingFunction);

	/**
	 * Fetches a publisher of records from a database via the given {@code queryRunner} in a reactive fashion.
	 *
	 * @param queryRunner Any type of reactive query runner. Neither sessions nor transactions will be closed.
	 * @return A publisher of records.
	 */
	default Publisher<Record> fetchWith(RxQueryRunner queryRunner) {
		return fetchWith(queryRunner, Function.identity());
	}
}
