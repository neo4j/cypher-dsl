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
package org.neo4j.cypherdsl.core;

import org.apiguardian.api.API;
import org.jetbrains.annotations.NotNull;

import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * A map of literals.
 *
 * @author Aaron Hiniker
 * @since 2023.1.1
 */
@API(status = STABLE, since = "2023.1.1")
public class MapLiteral extends LiteralBase<Map<String, Literal<?>>> {

	MapLiteral(Map<String, Literal<?>> content) {
		super(content);
	}

	@NotNull
	@Override
	public String asString() {
		return StreamSupport.stream(getContent().entrySet().spliterator(), false)
				.map(entry -> entry.getKey() + ": " + entry.getValue().asString()).collect(
				Collectors.joining(", ", "{", "}"));
	}
}
