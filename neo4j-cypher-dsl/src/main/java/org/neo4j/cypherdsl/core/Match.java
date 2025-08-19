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
import java.util.Collections;
import java.util.List;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;

import static org.apiguardian.api.API.Status.INTERNAL;
import static org.apiguardian.api.API.Status.STABLE;

/**
 * See <a href=
 * "https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/Match.html">Match</a>.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public final class Match extends AbstractClause implements ReadingClause {

	final Pattern pattern;

	private final boolean optional;

	/**
	 * A Neo4j extension to the match clause that allows to specify hints via the
	 * {@code USING} clause.
	 */
	private final List<Hint> hints;

	private final Where optionalWhere;

	Match(boolean optional, Pattern pattern, Where optionalWhere, List<Hint> optionalHints) {
		this.optional = optional;
		this.pattern = pattern;
		this.optionalWhere = optionalWhere;
		this.hints = (optionalHints != null) ? new ArrayList<>(optionalHints) : Collections.emptyList();
	}

	/**
	 * {@return true if this is an optional match}
	 */
	@API(status = INTERNAL)
	public boolean isOptional() {
		return this.optional;
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		this.pattern.accept(visitor);
		this.hints.forEach(value -> value.accept(visitor));
		Visitable.visitIfNotNull(this.optionalWhere, visitor);
		visitor.leave(this);
	}

}
