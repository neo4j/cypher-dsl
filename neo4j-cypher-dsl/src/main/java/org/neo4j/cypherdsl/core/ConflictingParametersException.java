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

import static org.apiguardian.api.API.Status.STABLE;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import org.apiguardian.api.API;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

/**
 * Exception thrown when extracting parameters from a statement leads to one parameter with a given name appearing
 * with different values.
 *
 * @author Andreas Berger
 * @author Michael J. Simons
 * @since 2021.0.0
 */
@API(status = STABLE, since = "2021.0.0")
public final class ConflictingParametersException extends RuntimeException {

	private static final long serialVersionUID = -45456411835790492L;

	private final transient Map<String, Set<Object>> erroneousParameters;

	public ConflictingParametersException(Map<String, Set<Object>> erroneousParameters) {
		super(createMessage(erroneousParameters));
		this.erroneousParameters = new HashMap<>(erroneousParameters.size());
		erroneousParameters.forEach((k, v) -> this.erroneousParameters.put(k, new HashSet<>(v)));
	}

	@NotNull @Contract(pure = true)
	public Map<String, Set<Object>> getErroneousParameters() {
		return Collections.unmodifiableMap(erroneousParameters);
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
		errors.forEach((param, values) -> {
			sb.append(prefix);
			sb.append("Parameter '").append(param).append("' is defined multiple times with different bound values: ");
			sb.append(values.stream()
				.map(o -> o == Parameter.NO_VALUE ? "(UNDEFINED VALUE)" : Objects.toString(o))
				.collect(Collectors.joining(" != ")));
		});
		return sb.toString();
	}
}
