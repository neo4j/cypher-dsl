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

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitor;

import static org.apiguardian.api.API.Status.INTERNAL;

/**
 * A condition that uses its bound expression.
 *
 * @author Andreas Berger
 * @since 2021.0.0
 */
@API(status = INTERNAL, since = "2021.0.0")
class ExpressionCondition implements Condition {

	private final Expression value;

	ExpressionCondition(Expression value) {
		this.value = value;
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		this.value.accept(visitor);
		visitor.leave(this);
	}

}
