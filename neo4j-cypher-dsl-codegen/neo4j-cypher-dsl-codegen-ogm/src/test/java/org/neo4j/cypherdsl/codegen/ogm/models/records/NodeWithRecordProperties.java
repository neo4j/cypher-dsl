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
package org.neo4j.cypherdsl.codegen.ogm.models.records;

import org.neo4j.ogm.annotation.Id;
import org.neo4j.ogm.annotation.NodeEntity;
import org.neo4j.ogm.annotation.Property;
import org.neo4j.ogm.annotation.Relationship;
import org.neo4j.ogm.annotation.typeconversion.Convert;
import org.neo4j.ogm.typeconversion.AttributeConverter;

/**
 * Random test class.
 */
@NodeEntity
public class NodeWithRecordProperties {

	@Id
	private String id;

	@Convert(RecordConverter.class)
	private RecordAsProperty recordAsPropertyWithConversion;

	@Property
	private RecordAsProperty yoloingNoConversion;

	// Inner types for relationships are not supported
	@Relationship
	private RecordTarget recordAsRelationship;

	record RecordAsProperty(String value) {
	}

	abstract static class RecordConverter implements AttributeConverter<RecordAsProperty, String> {

	}

}
