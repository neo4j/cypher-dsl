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

import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.internal.SchemaNamesBridge;

/**
 * The factory methods are on the concrete class, as they should not be exposed to the
 * outside via the interface.
 *
 * @param target the target of the {@code USE} clause
 * @param dynamic {@code true} if dynamic graph lookups are to be used
 * @author Michael J. Simons
 * @since 2023.0.0
 */
record UseClauseImpl(Expression target, boolean dynamic) implements Use {

	static Use of(String target) {
		var components = target.split("\\.");
		Expression targetExpression;
		if (components.length == 1) {
			targetExpression = Cypher.raw(SchemaNamesBridge.sanitize(components[0], false).orElseThrow());
		}
		else {
			targetExpression = Cypher.raw(SchemaNamesBridge.sanitize(components[0], false)
				.flatMap(composite -> SchemaNamesBridge.sanitize(components[1], false).map(v -> composite + "." + v))
				.orElseThrow());
		}

		return new UseClauseImpl(targetExpression, false);
	}

	static Use of(Expression target) {
		return new UseClauseImpl(target,
				!(target instanceof FunctionInvocation fi) || !"graph.byName".equals(fi.getFunctionName()));
	}

	@Override
	public void accept(Visitor visitor) {
		visitor.enter(this);
		this.target.accept(visitor);
		visitor.leave(this);
	}
}
