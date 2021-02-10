/*
 * Copyright (c) 2019-2021 "Neo4j,"
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
package org.neo4j.cypherdsl.core.parameter;

import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author Andreas Berger
 */
public class ConflictingParametersException extends RuntimeException {
	private final Map<String, Set<Object>> errors;

	public ConflictingParametersException(Map<String, Set<Object>> errors) {
		super(createMessage(errors));
		this.errors = errors;
	}

	public Map<String, Set<Object>> getErrors() {
		return errors;
	}

	private static String createMessage(Map<String, Set<Object>> errors) {
		StringBuilder sb = new StringBuilder();
		String prefix;
		if (errors.size() > 1) {
			sb.append("There are conflicting parameter values:");
			prefix = "\n\t";
		} else {
			prefix = "";
		}
		errors.forEach((key, values) -> {
			sb.append(prefix);
			sb.append("Parameter '").append(key).append("' is defined multiple times with different bound values: ");
			sb.append(values.stream().map(Objects::toString).collect(Collectors.joining(" != ")));
		});
		return sb.toString();
	}
}
