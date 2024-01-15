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
package org.neo4j.cypherdsl.core.executables;

import static org.apiguardian.api.API.Status.INTERNAL;

import java.util.Collection;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.apiguardian.api.API;
import org.jetbrains.annotations.NotNull;
import org.neo4j.cypherdsl.core.Statement;
import org.neo4j.driver.Query;
import org.neo4j.driver.QueryRunner;
import org.neo4j.driver.async.AsyncQueryRunner;
import org.neo4j.driver.async.ResultCursor;
import org.neo4j.driver.summary.ResultSummary;

/**
 * @author Michael J. Simons
 * @soundtrack Swiss + Die Andern - Randalieren für die Liebe
 * @since 2021.2.1
 */
@API(status = INTERNAL, since = "2021.2.1")
class DefaultExecutableStatement implements ExecutableStatement {

	private final Statement delegate;

	DefaultExecutableStatement(Statement delegate) {
		this.delegate = delegate;
	}

	@NotNull
	@Override
	public final Map<String, Object> getParameters() {
		return delegate.getParameters();
	}

	@NotNull
	@Override
	public final Collection<String> getParameterNames() {
		return delegate.getParameterNames();
	}

	@NotNull
	@Override
	public final String getCypher() {
		return delegate.getCypher();
	}

	@Override
	public final ResultSummary executeWith(QueryRunner queryRunner) {

		return queryRunner.run(createQuery()).consume();
	}

	@Override
	public final CompletableFuture<ResultSummary> executeWith(AsyncQueryRunner queryRunner) {

		return queryRunner.runAsync(createQuery())
			.thenCompose(ResultCursor::consumeAsync)
			.toCompletableFuture();
	}

	/**
	 * Turns this statement into a query that the Neo4j driver can understand, including any named and anonymous
	 * parameter that have a value assigned.
	 *
	 * @return A query.
	 */
	final Query createQuery() {

		return new Query(delegate.getCypher(), delegate.getParameters());
	}
}
