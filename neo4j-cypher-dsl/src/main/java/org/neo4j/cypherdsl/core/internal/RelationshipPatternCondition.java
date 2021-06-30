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
package org.neo4j.cypherdsl.core.internal;

import static org.apiguardian.api.API.Status.INTERNAL;

import org.apiguardian.api.API;
import org.jetbrains.annotations.NotNull;
import org.neo4j.cypherdsl.core.Condition;
import org.neo4j.cypherdsl.core.Operator;
import org.neo4j.cypherdsl.core.RelationshipPattern;
import org.neo4j.cypherdsl.core.ast.Visitor;

/**
 * Internal wrapper for marking a path pattern as a condition.
 *
 * @author Michael J. Simons
 * @soundtrack Red Hot Chili Peppers - Red Hot Chili Peppers: Greatest Hits
 * @since 1.0.1
 */
@API(status = INTERNAL, since = "1.0")
public final class RelationshipPatternCondition implements Condition {

	public static RelationshipPatternCondition of(RelationshipPattern pathPattern) {
		return new RelationshipPatternCondition(false, pathPattern);
	}

	public static RelationshipPatternCondition not(RelationshipPattern pathPattern) {
		return new RelationshipPatternCondition(true, pathPattern);
	}

	private final boolean not;

	private final RelationshipPattern pathPattern;

	private RelationshipPatternCondition(boolean not, RelationshipPattern pathPattern) {
		this.not = not;
		this.pathPattern = pathPattern;
	}

	@Override
	@NotNull
	public Condition not() {
		return new RelationshipPatternCondition(!this.not, this.pathPattern);
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		if (not) {
			Operator.NOT.accept(visitor);
		}
		pathPattern.accept(visitor);
		visitor.leave(this);
	}
}
