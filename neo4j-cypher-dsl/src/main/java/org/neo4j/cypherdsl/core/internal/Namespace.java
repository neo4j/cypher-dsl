/*
 * Copyright (c) 2019-2026 "Neo4j,"
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

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.Literal;

import static org.apiguardian.api.API.Status.INTERNAL;

/**
 * Representation of a namespace (i.e. for procedures) element.
 *
 * @author Michael J. Simons
 * @since 2020.0.1
 */
@API(status = INTERNAL, since = "2020.0.1")
public final class Namespace implements Literal<String[]> {

	private final String[] content;

	Namespace(String[] value) {
		this.content = value;
	}

	@Override
	public String asString() {

		return String.join(".", this.content);
	}

}
