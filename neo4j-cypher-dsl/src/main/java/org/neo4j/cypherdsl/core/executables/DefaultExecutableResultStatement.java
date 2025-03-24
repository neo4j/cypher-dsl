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

import static org.apiguardian.api.API.Status.INTERNAL;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Stream;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.Statement;
import org.neo4j.driver.Record;
import org.neo4j.driver.Result;
import org.neo4j.driver.SimpleQueryRunner;
import org.neo4j.driver.async.AsyncQueryRunner;
import org.neo4j.driver.summary.ResultSummary;

/**
 * @author Michael J. Simons
 * @soundtrack Swiss + Die Andern - Randalieren für die Liebe
 * @since 2021.2.1
 */
@SuppressWarnings("removal")
@API(status = INTERNAL, since = "2021.2.1")
class DefaultExecutableResultStatement extends DefaultExecutableStatement implements ExecutableResultStatement {

	DefaultExecutableResultStatement(Statement delegate) {
		super(delegate);
	}

	@Override
	public final <T> List<T> fetchWith(SimpleQueryRunner queryRunner, Function<Record, T> mappingFunction) {

		return queryRunner.run(this.createQuery()).list(mappingFunction);
	}

	@Override
	public final <T> CompletableFuture<List<T>> fetchWith(AsyncQueryRunner queryRunner,
		Function<Record, T> mappingFunction) {

		return queryRunner.runAsync(createQuery())
			.thenCompose(c -> c.listAsync(mappingFunction))
			.toCompletableFuture();
	}

	@Override
	public final ResultSummary streamWith(SimpleQueryRunner queryRunner, Consumer<Stream<Record>> consumer) {

		Result result = queryRunner.run(this.createQuery());
		try (Stream<Record> stream = result.stream()) {
			consumer.accept(stream);
		}
		return result.consume();
	}
}
