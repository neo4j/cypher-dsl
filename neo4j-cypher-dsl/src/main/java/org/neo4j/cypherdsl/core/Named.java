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

import java.util.Optional;

import org.apiguardian.api.API;

/**
 * A named thing exposes {@link #getSymbolicName()}, making the thing identifiable.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public non-sealed interface Named extends IdentifiableElement {

	/**
	 * @return An optional symbolic name.
	 */
	Optional<SymbolicName> getSymbolicName();

	/**
	 * @return A symbolic name
	 * @throws IllegalStateException If this has not been named yet.
	 */
	default SymbolicName getRequiredSymbolicName() {
		return getSymbolicName().orElseThrow(() -> new IllegalStateException("No name present."));
	}

	@Override default Expression asExpression() {
		return getRequiredSymbolicName();
	}
}
