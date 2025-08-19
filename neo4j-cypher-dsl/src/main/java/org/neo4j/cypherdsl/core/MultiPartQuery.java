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
package org.neo4j.cypherdsl.core;

import java.util.ArrayList;
import java.util.List;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitor;

import static org.apiguardian.api.API.Status.INTERNAL;

/**
 * See <a href=
 * "https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/MultiPartQuery.html">MultiPartQuery</a>.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = INTERNAL, since = "1.0")
class MultiPartQuery extends AbstractStatement implements Statement.SingleQuery {

	private final List<MultiPartElement> parts;

	private final SinglePartQuery remainder;

	private MultiPartQuery(List<MultiPartElement> parts, SinglePartQuery remainder) {

		this.parts = new ArrayList<>(parts);
		this.remainder = remainder;
	}

	static MultiPartQuery create(List<MultiPartElement> parts, SinglePartQuery remainder) {

		if (remainder instanceof ResultStatement) {
			return new MultiPartQueryWithResult(parts, remainder);
		}
		else {
			return new MultiPartQuery(parts, remainder);
		}
	}

	@Override
	public void accept(Visitor visitor) {

		this.parts.forEach(p -> p.accept(visitor));
		this.remainder.accept(visitor);
	}

	static final class MultiPartQueryWithResult extends MultiPartQuery implements ResultStatement {

		private MultiPartQueryWithResult(List<MultiPartElement> parts, SinglePartQuery remainder) {
			super(parts, remainder);
		}

	}

}
