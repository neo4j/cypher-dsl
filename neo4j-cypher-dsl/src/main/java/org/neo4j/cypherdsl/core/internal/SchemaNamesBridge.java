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
package org.neo4j.cypherdsl.core.internal;

import java.util.Optional;

import org.neo4j.cypherdsl.support.schema_name.SchemaNames;

/**
 * This is a bridge to {@link SchemaNames}. Having this indirection avoids changing
 * of the bytecode of classes using the sanitizer: When the byte code changes, JaCoCo won't be able to analyze it. Of course,
 * one solution would be shading the sanitizer after the integration tests, but the integration tests are meant to test
 * the modularized Jar file proper.
 *
 * @author Michael J. Simons
 * @since 2023.0.0
 */
public final class SchemaNamesBridge {

	public static Optional<String> sanitize(String value, boolean enforceQuotes) {
		return SchemaNames.sanitize(value, enforceQuotes);
	}

	private SchemaNamesBridge() {
	}
}
