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
package org.neo4j.cypherdsl.core;

import static org.apiguardian.api.API.Status.STABLE;
import static org.apiguardian.api.API.Status.INTERNAL;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;

/**
 * An action or event that happens after a {@code MERGE} clause. It can either be one of two types:
 * {@link Type#ON_CREATE} or {@link Type#ON_MATCH}.
 *
 * <p>Both events supports the setting of properties, but not removing or adding labels. Multiple
 * properties should be set in one action, but Cypher and
 * <a href="https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/Merge.html">openCypher</a>
 * allow for multiple {@link MergeAction merge actions}, with the same or different types.
 *
 * @author Michael J. Simons
 * @soundtrack System Of A Down - Protect The Land &amp; Genocidal Humanoidz
 * @since 2020.1.2
 */
@API(status = STABLE, since = "2020.1.2")
public final class MergeAction implements Visitable {

	/**
	 * The type of the action.
	 */
	public enum Type {
		/**
		 * Triggered when a pattern has been created.
		 */
		ON_CREATE,
		/**
		 * Triggered when a pattern has been fully matched.
		 */
		ON_MATCH
	}

	/**
	 * Creates a new merge action. Mostly useful when building the AST outside the fluent DSL.
	 *
	 * @param type The type of the action
	 * @param set  The corresponding set clause
	 * @return An immutable action
	 * @since 2021.3.0
	 */
	public static MergeAction of(Type type, Set set) {
		return new MergeAction(type, set);
	}

	private final Type type;
	private final Set set;

	private MergeAction(Type type, Set set) {
		this.type = type;
		this.set = set;
	}

	/**
	 * @return Event type of this action.
	 */
	@API(status = INTERNAL)
	public Type getType() {
		return type;
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		this.set.accept(visitor);
		visitor.leave(this);
	}
}
