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
package org.neo4j.cypherdsl.codegen.sdn6.models.primitives;

import java.util.UUID;

import org.springframework.data.neo4j.core.schema.GeneratedValue;
import org.springframework.data.neo4j.core.schema.Id;
import org.springframework.data.neo4j.core.schema.Node;
import org.springframework.data.neo4j.core.schema.Relationship;

/**
 * @author Florent Biville
 * @author Michael J. Simons
 */
@Node(primaryLabel = "Connector")
public class Connector {

	@Id
	@GeneratedValue(GeneratedValue.UUIDGenerator.class)
	private UUID id;

	private final String uri;

	private final boolean official;

	private int anInt;

	private float aFloat;

	private double aDouble;

	@Relationship
	private char aChar;

	public Connector(String uri, boolean official) {
		this.uri = uri;
		this.official = official;
	}
}
