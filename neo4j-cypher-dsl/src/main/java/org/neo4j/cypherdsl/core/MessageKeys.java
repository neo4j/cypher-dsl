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
package org.neo4j.cypherdsl.core;

/**
 * The message keys available in the {@code messages}-bundle.
 *
 * @author Michael J. Simons
 */
final class MessageKeys {

	public static final String ASSERTIONS_EXPRESSION_REQUIRED = "assertions.expression-required";
	public static final String ASSERTIONS_EXPRESSIONS_REQUIRED = "assertions.expressions-required";
	public static final String ASSERTIONS_AT_LEAST_ONE_EXPRESSION_REQUIRED = "assertions.at-least-one-expression-required";
	public static final String ASSERTIONS_NODE_REQUIRED = "assertions.node-required";
	public static final String ASSERTIONS_RELATIONSHIP_REQUIRED = "assertions.relationship-required";
	public static final String ASSERTIONS_VARIABLE_REQUIRED = "assertions.variable-required";
	public static final String ASSERTIONS_COMPONENTS_REQUIRED = "assertions.components-required";
	public static final String ASSERTIONS_TEMPORAL_VALUE_REQUIRED = "assertions.temporal-value-required";
	public static final String ASSERTIONS_YEAR_REQUIRED = "assertions.year-required";
	public static final String ASSERTIONS_MONTH_REQUIRED = "assertions.month-required";
	public static final String ASSERTIONS_DAY_REQUIRED = "assertions.day-required";
	public static final String ASSERTIONS_TZ_REQUIRED = "assertions.tz-required";
	public static final String ASSERTIONS_RANGE_TARGET_REQUIRED = "assertions.range-target-required";
	public static final String ASSERTIONS_RANGE_INDEX_REQUIRED = "assertions.range-index-required";
	public static final String ASSERTIONS_RANGE_START_REQUIRED = "assertions.range-start-required";
	public static final String ASSERTIONS_RANGE_END_REQUIRED = "assertions.range-end-required";
	public static final String ASSERTIONS_EXPRESSION_FOR_FUNCTION_REQUIRED = "assertions.expression-for-function-required";
	public static final String ASSERTIONS_PATTERN_FOR_FUNCTION_REQUIRED = "assertions.pattern-for-function-required";
	public static final String ASSERTIONS_AT_LEAST_ONE_ARG_REQUIRED = "assertions.at-least-one-arg-required";
	public static final String ASSERTIONS_CORRECT_USAGE_OF_DISTINCT = "assertions.correct-usage-of-distinct";
	public static final String ASSERTIONS_NAMED_PATH_REQUIRED = "assertions.named-path-required";

	public static final String ASSERTIONS_REQUIRES_NAME_FOR_MUTATION = "assertions.requires-name-for-mutation";

	private MessageKeys() {
	}
}
