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
package org.neo4j.cypherdsl.core;

import static org.apiguardian.api.API.Status.EXPERIMENTAL;

import org.apiguardian.api.API;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.neo4j.cypherdsl.core.internal.LiteralBase;

/**
 * The boolean literal.
 *
 * @author Michael J. Simons
 * @soundtrack Bad Religion - Age Of Unreason
 * @since 1.0
 */
@API(status = EXPERIMENTAL, since = "1.0")
public final class BooleanLiteral extends LiteralBase<Boolean> {

	static final BooleanLiteral TRUE = new BooleanLiteral(true);
	static final BooleanLiteral FALSE = new BooleanLiteral(false);

	static Literal<Boolean> of(@Nullable Boolean value) {

		if (value != null && value) {
			return TRUE;
		} else {
			return FALSE;
		}
	}

	private BooleanLiteral(boolean content) {
		super(content);
	}

	@NotNull
	@Override
	public String asString() {
		return getContent().toString();
	}
}
