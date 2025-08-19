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
package org.neo4j.cypherdsl.core.internal;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.Condition;
import org.neo4j.cypherdsl.core.Operator;
import org.neo4j.cypherdsl.core.RelationshipPattern;
import org.neo4j.cypherdsl.core.ast.Visitor;

import static org.apiguardian.api.API.Status.INTERNAL;

/**
 * Internal wrapper for marking a path pattern as a condition.
 *
 * @author Michael J. Simons
 * @since 1.0.1
 */
@API(status = INTERNAL, since = "1.0")
public final class RelationshipPatternCondition implements Condition {

	private final boolean not;

	private final RelationshipPattern pathPattern;

	private RelationshipPatternCondition(boolean not, RelationshipPattern pathPattern) {
		this.not = not;
		this.pathPattern = pathPattern;
	}

	/**
	 * Creates a new {@link Condition} matching the given pattern.
	 * @param pathPattern the pattern to be matched
	 * @return a new condition
	 */
	public static RelationshipPatternCondition of(RelationshipPattern pathPattern) {
		return new RelationshipPatternCondition(false, pathPattern);
	}

	/**
	 * Creates a new {@link Condition} that evaluates to {@literal true} when the pattern
	 * does not match.
	 * @param pathPattern the pattern to be matched
	 * @return a new condition
	 */
	public static RelationshipPatternCondition not(RelationshipPattern pathPattern) {
		return new RelationshipPatternCondition(true, pathPattern);
	}

	@Override
	public Condition not() {
		return new RelationshipPatternCondition(!this.not, this.pathPattern);
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		if (this.not) {
			Operator.NOT.accept(visitor);
		}
		this.pathPattern.accept(visitor);
		visitor.leave(this);
	}

}
