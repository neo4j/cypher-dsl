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
package org.neo4j.cypherdsl.build.processor;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonInclude;

/**
 * An entry that should find its way into GraalVM reflection-config.json.
 * @author Michael J. Simons
 */
@JsonAutoDetect(
	getterVisibility = JsonAutoDetect.Visibility.NON_PRIVATE,
	isGetterVisibility = JsonAutoDetect.Visibility.NON_PRIVATE
)
final class Entry {

	private final String name;

	private boolean allDeclaredMethods;

	private boolean allDeclaredConstructors;

	Entry(String name) {
		this.name = name;
	}

	String getName() {
		return name;
	}

	@JsonInclude(JsonInclude.Include.NON_DEFAULT)
	boolean isAllDeclaredMethods() {
		return allDeclaredMethods;
	}

	void setAllDeclaredMethods(boolean allDeclaredMethods) {
		this.allDeclaredMethods = allDeclaredMethods;
	}

	@JsonInclude(JsonInclude.Include.NON_DEFAULT)
	boolean isAllDeclaredConstructors() {
		return allDeclaredConstructors;
	}

	void setAllDeclaredConstructors(boolean allDeclaredConstructors) {
		this.allDeclaredConstructors = allDeclaredConstructors;
	}
}
