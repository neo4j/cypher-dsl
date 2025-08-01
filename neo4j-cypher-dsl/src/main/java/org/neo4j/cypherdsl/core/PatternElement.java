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

import static org.apiguardian.api.API.Status.STABLE;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitable;

/**
 * See <a href="https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/PatternElement.html">PatternElement</a>.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public interface PatternElement extends Visitable {

	/**
	 * Creates a new {@link PatternElement} which including an additional filter. Returns {@code this} pattern.
	 * when {@code predicate} is literal {@code null}.
	 * <p>
	 * The pattern might be a {@link Node node pattern} or a {@link RelationshipPattern relationship pattern}.
	 * <p>
	 * A {@code WHERE} on a pattern is only supported from Neo4j 5.0 onwards.
	 *
	 * @param predicate the predicate to filter on
	 * @return a new pattern element or this instance if the predicate to this method was literal {@code null}
	 * @throws UnsupportedOperationException In cases the underlying element does not support a {@code WHERE} clause
	 * @since 2023.9.0
	 */
	@Neo4jVersion(minimum = "5.0")
	default PatternElement where(Expression predicate) {
		throw new UnsupportedOperationException();
	}
}
