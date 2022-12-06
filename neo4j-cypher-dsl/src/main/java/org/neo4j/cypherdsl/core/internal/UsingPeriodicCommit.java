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
package org.neo4j.cypherdsl.core.internal;

import static org.apiguardian.api.API.Status.INTERNAL;

import org.apiguardian.api.API;
import org.jetbrains.annotations.Nullable;
import org.neo4j.cypherdsl.core.Neo4jVersion;
import org.neo4j.cypherdsl.core.ast.Visitable;

/**
 * A visitable representing a {@code USING PERIODIC COMMIT} clause. Not meant to be used outside the Cypher-DSL directly.
 * Will be changed without further notice.
 *
 * @author Michael J. Simons
 * @since 2021.2.1
 */
@API(status = INTERNAL, since = "2021.2.1")
@Neo4jVersion(minimum = "3.5", last = "4.4")
public final class UsingPeriodicCommit implements Visitable {

	@Nullable
	private final Integer rate;

	/**
	 * Creates a new using periodic commit clause
	 *
	 * @param rate The rate to be applied
	 */
	public UsingPeriodicCommit(@Nullable Integer rate) {
		this.rate = rate;
	}

	/**
	 * @return the rate for periodic commit
	 */
	@Nullable
	public Integer getRate() {
		return rate;
	}
}
