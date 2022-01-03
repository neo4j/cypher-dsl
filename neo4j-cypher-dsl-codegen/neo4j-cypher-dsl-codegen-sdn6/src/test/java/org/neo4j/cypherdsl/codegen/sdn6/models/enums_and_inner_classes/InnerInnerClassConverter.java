/*
 * Copyright (c) 2019-2022 "Neo4j,"
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
package org.neo4j.cypherdsl.codegen.sdn6.models.enums_and_inner_classes;

import org.neo4j.cypherdsl.codegen.sdn6.models.enums_and_inner_classes.ConnectorTransport.InnerClass.InnerInnerClass;
import org.neo4j.driver.Value;
import org.springframework.data.neo4j.core.convert.Neo4jPersistentPropertyConverter;

/**
 * @author Michael J. Simons
 */
public class InnerInnerClassConverter implements Neo4jPersistentPropertyConverter<InnerInnerClass> {

	@Override public Value write(InnerInnerClass innerInnerClass) {
		return null;
	}

	@Override public InnerInnerClass read(Value value) {
		return null;
	}
}
