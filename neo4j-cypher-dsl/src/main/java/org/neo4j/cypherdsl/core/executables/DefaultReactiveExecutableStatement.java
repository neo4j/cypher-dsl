/*
 * Copyright (c) 2019-2022 "Neo4j,"
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

import reactor.core.publisher.Mono;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.Statement;
import org.neo4j.driver.reactive.RxQueryRunner;
import org.neo4j.driver.summary.ResultSummary;
import org.reactivestreams.Publisher;

/**
 * @author Michael J. Simons
 * @soundtrack Swiss + Die Andern - Randalieren für die Liebe
 * @since 2021.2.1
 */
@API(status = INTERNAL, since = "2021.2.1")
class DefaultReactiveExecutableStatement extends DefaultExecutableStatement implements ReactiveExecutableStatement {

	DefaultReactiveExecutableStatement(Statement delegate) {
		super(delegate);
	}

	@Override
	public final Publisher<ResultSummary> executeWith(RxQueryRunner queryRunner) {

		return Mono.fromCallable(this::createQuery)
			.flatMap(q -> Mono.from(queryRunner.run(q).consume()));
	}
}
