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
package org.neo4j.cypherdsl.core.ast;

import static org.apiguardian.api.API.Status.STABLE;

import org.apiguardian.api.API;

/**
 * Result of entering a {@link Visitable}. Visitables are not required to check the result and can enter their child elements
 * nevertheless.
 *
 * @author Michael J. Simons
 * @since 2022.3.0
 */
@API(status = STABLE, since = "2022.3.0")
public enum EnterResult {

	/**
	 * Continue with all child elements
	 */
	CONTINUE,

	/**
	 * Skip child elements
	 */
	SKIP_CHILDREN
}
