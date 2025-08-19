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

import static org.apiguardian.api.API.Status.STABLE;

/**
 * A constant condition that is either always {@literal true} or {@literal false}.
 *
 * @author Michael J. Simons
 * @author Andreas Berger
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public final class ConstantCondition extends ExpressionCondition {

	static final ConstantCondition TRUE = new ConstantCondition(BooleanLiteral.TRUE);
	static final ConstantCondition FALSE = new ConstantCondition(BooleanLiteral.FALSE);

	private ConstantCondition(BooleanLiteral value) {
		super(value);
	}

}
