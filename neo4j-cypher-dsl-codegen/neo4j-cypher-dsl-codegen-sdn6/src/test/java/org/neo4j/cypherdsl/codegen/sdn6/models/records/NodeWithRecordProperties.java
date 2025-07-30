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
package org.neo4j.cypherdsl.codegen.sdn6.models.records;

import org.springframework.data.neo4j.core.convert.ConvertWith;
import org.springframework.data.neo4j.core.convert.Neo4jPersistentPropertyConverter;
import org.springframework.data.neo4j.core.schema.Id;
import org.springframework.data.neo4j.core.schema.Node;
import org.springframework.data.neo4j.core.schema.Property;
import org.springframework.data.neo4j.core.schema.Relationship;

/**
 * Random test class.
 */
@Node
public class NodeWithRecordProperties {

	@Id
	private String id;

	@ConvertWith(converter = RecordConverter.class)
	private RecordAsProperty recordAsPropertyWithConversion;

	@Property
	private RecordAsProperty yoloingNoConversion;

	// Inner types for relationships are not supported
	@Relationship
	private RecordTarget recordAsRelationship;

	record RecordAsProperty(String value) {
	}

	static abstract class RecordConverter implements Neo4jPersistentPropertyConverter<RecordAsProperty> {
	}
}
