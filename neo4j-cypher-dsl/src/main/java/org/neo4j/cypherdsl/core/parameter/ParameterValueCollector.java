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

import org.neo4j.cypherdsl.core.Statement;

/**
 * Helper class to collect all parameters with bound value into a map of used parameters
 *
 * @author Andreas Berger
 */
public final class ParameterValueCollector {

	/**
	 * Collects all bound parameters from the statement.
	 *
	 * @param statement the statement to collect all bound parameters from
	 * @return a map of used parameters with its bound values
	 * @see org.neo4j.cypherdsl.core.Parameter#withValue(Object)
	 */
	public static Map<String, Object> collectBoundParameters(Statement statement) {
		ParameterValueCollectorVisitor parameterValueCollectorVisitor = new ParameterValueCollectorVisitor();
		statement.accept(parameterValueCollectorVisitor);
		return parameterValueCollectorVisitor.getParameterValues();
	}
}
