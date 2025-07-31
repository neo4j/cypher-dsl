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
package org.neo4j.cypherdsl.codegen.ogm.models.enums_and_inner_classes;

import org.neo4j.ogm.annotation.Id;
import org.neo4j.ogm.annotation.NodeEntity;
import org.neo4j.ogm.annotation.typeconversion.Convert;

/**
 * @author Michael J. Simons
 */
@NodeEntity(label = "Transport")
public class ConnectorTransport {

	/**
	 * Some enum
	 */
	public enum ConnectorTransportType {

		HTTP, BOLT
	}

	/**
	 * An inner class
	 */
	public static class InnerClass {

		/**
		 * With a nested inner class (used to check whether the recursive algorithm stops at some point)
		 */
		public static class InnerInnerClass {
		}
	}

	@Id
	private final ConnectorTransportType value;

	private OtherEnum otherEnum;

	@Convert(value = AnotherConverter.class)
	private InnerClass.InnerInnerClass innerInnerClass;

	@Convert(value = InnerInnerClassConverter.class)
	private InnerClass.InnerInnerClass innerInnerClassButWithoutConverter;

	public ConnectorTransport(ConnectorTransportType value) {
		this.value = value;
	}
}
