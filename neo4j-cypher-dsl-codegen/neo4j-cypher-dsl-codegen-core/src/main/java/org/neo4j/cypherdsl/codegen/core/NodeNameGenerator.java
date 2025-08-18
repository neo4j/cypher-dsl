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
package org.neo4j.cypherdsl.codegen.core;

import org.apiguardian.api.API;

import static org.apiguardian.api.API.Status.INTERNAL;

/**
 * Default generator for creating names of node classes. Just creates a valid java
 * identifier from a suggested name. No further optimization is done. Suggested name can
 * be a list of labels etc.
 *
 * @author Michael J. Simons
 * @since 2021.1.0
 */
@API(status = INTERNAL, since = "2021.1.0")
final class NodeNameGenerator extends AbstractClassNameGenerator implements ClassNameGenerator {

	@Override
	public String generate(String suggestedName) {

		return generateTypeName(suggestedName).toString();
	}

}
