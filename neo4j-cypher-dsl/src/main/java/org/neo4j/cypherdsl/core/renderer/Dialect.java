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
package org.neo4j.cypherdsl.core.renderer;

import static org.apiguardian.api.API.Status.STABLE;

import org.apiguardian.api.API;
import org.jetbrains.annotations.Nullable;
import org.neo4j.cypherdsl.core.Comparison;
import org.neo4j.cypherdsl.core.FunctionInvocation;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;

/**
 * The dialect to be used when rendering a statement into Cypher.
 *
 * @author Michael J. Simons
 * @soundtrack Red Hot Chili Peppers - Unlimited Love
 * @since TBA
 */
@API(status = STABLE, since = "TBA")
public enum Dialect {

	/**
	 * Neo4j 4.4 and earlier
	 */
	DEFAULT,

	/**
	 * Neo4j 5
	 */
	NEO4J_5 {
		@Override
		@Nullable Class<? extends Visitor> getHandler(Visitable visitable) {
			if (visitable instanceof FunctionInvocation) {
				return Neo4j5FunctionInvocationVisitor.class;
			} else if (visitable instanceof Comparison) {
				return Neo4j5ComparisonVisitor.class;
			}
			return super.getHandler(visitable);
		}
	};

	@Nullable Class<? extends Visitor> getHandler(Visitable visitable) {
		return null;
	}
}
