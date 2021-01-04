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

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.support.Neo4jVersion;
import org.neo4j.cypherdsl.core.support.Visitor;

/**
 * An existential subquery can only be used in a where clause. The subquery must consisted only of a match statement
 * which may have a {@code WHERE} clause on its own but is not not allowed to return anything.
 *
 * @author Michael J. Simons
 * @soundtrack Die Ärzte - Seitenhirsch
 * @neo4j.version 4.0.0
 * @since 2020.1.2
 */
@API(status = EXPERIMENTAL, since = "2020.1.2")
@Neo4jVersion(minimum = "4.0.0")
public final class ExistentialSubquery implements Condition {

	static ExistentialSubquery exists(Match fragment) {

		return new ExistentialSubquery(fragment);
	}

	private final Match fragment;

	ExistentialSubquery(Match fragment) {
		this.fragment = fragment;
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		fragment.accept(visitor);
		visitor.leave(this);
	}
}
