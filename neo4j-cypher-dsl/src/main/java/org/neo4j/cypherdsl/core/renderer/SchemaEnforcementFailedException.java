/*
 * Copyright (c) 2019-2023 "Neo4j,"
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

import static org.apiguardian.api.API.Status.INTERNAL;

import java.io.Serial;

import org.apiguardian.api.API;

/**
 * Thrown when schema validation fails.
 *
 * @author Michael J. Simons
 * @since 2023.7.0
 */
@API(status = INTERNAL, since = "2023.7.0")
public final class SchemaEnforcementFailedException extends RuntimeException {

	@Serial
	private static final long serialVersionUID = 5473466301617398078L;

	public SchemaEnforcementFailedException() {
		// Make the Java module system happy.
	}
}
