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

import org.apiguardian.api.API;
import org.apiguardian.api.API.Status;
import org.neo4j.cypherdsl.core.FunctionInvocation.FunctionDefinition;

/**
 * A (non-exhaustive) list of builtin functions.
 *
 * @author Michael J. Simons
 * @since 2020.1.0
 */
@API(status = Status.INTERNAL, since = "2020.1.0")
final class BuiltInFunctions {

	private BuiltInFunctions() {
	}

	enum Predicates implements FunctionDefinition {

		ALL("all"), ANY("any"), EXISTS("exists"), NONE("none"), SINGLE("single"), IS_EMPTY("isEmpty");

		private final String implementationName;

		Predicates(String implementationName) {
			this.implementationName = implementationName;
		}

		@Override
		public String getImplementationName() {
			return this.implementationName;
		}

	}

	enum Graph implements FunctionDefinition {

		NAMES("names"), PROPERTIES_BY_NAME("propertiesByName"), BY_NAME("byName");

		private final String implementationName;

		Graph(String implementationName) {
			this.implementationName = "graph." + implementationName;
		}

		@Override
		public String getImplementationName() {
			return this.implementationName;
		}

	}

	enum Scalars implements FunctionDefinition {

		COALESCE("coalesce"), END_NODE("endNode"), HEAD("head"), ID("id"), ELEMENT_ID("elementId"), LAST("last"),
		LENGTH("length"),

		PROPERTIES("properties"), SHORTEST_PATH("shortestPath"), SIZE("size"),

		START_NODE("startNode"), TYPE("type"),

		TO_STRING("toString"), TO_INTEGER("toInteger"), TO_FLOAT("toFloat"), TO_BOOLEAN("toBoolean");

		private final String implementationName;

		Scalars(String implementationName) {
			this.implementationName = implementationName;
		}

		@Override
		public String getImplementationName() {
			return this.implementationName;
		}

	}

	enum Strings implements FunctionDefinition {

		LEFT("left"), LTRIM("ltrim"), REPLACE("replace"), REVERSE("reverse"), RIGHT("right"), RTRIM("rtrim"),
		SPLIT("split"), SUBSTRING("substring"), TO_LOWER("toLower"), TO_STRING("toString"),
		TO_STRING_OR_NULL("toStringOrNull"), TO_UPPER("toUpper"), TRIM("trim");

		private final String implementationName;

		Strings(String implementationName) {
			this.implementationName = implementationName;
		}

		@Override
		public String getImplementationName() {
			return this.implementationName;
		}

	}

	enum Spatials implements FunctionDefinition {

		POINT("point"), DISTANCE("distance");

		private final String implementationName;

		Spatials(String implementationName) {
			this.implementationName = implementationName;
		}

		@Override
		public String getImplementationName() {
			return this.implementationName;
		}

	}

	enum Aggregates implements FunctionDefinition {

		AVG("avg"), COLLECT("collect"), COUNT("count"), MAX("max"), MIN("min"), PERCENTILE_CONT("percentileCont"),
		PERCENTILE_DISC("percentileDisc"), ST_DEV("stDev"), ST_DEV_P("stDevP"), SUM("sum");

		private final String implementationName;

		Aggregates(String implementationName) {
			this.implementationName = implementationName;
		}

		@Override
		public String getImplementationName() {
			return this.implementationName;
		}

		@Override
		public boolean isAggregate() {
			return true;
		}

	}

	enum Lists implements FunctionDefinition {

		KEYS("keys"), LABELS("labels"), NODES("nodes"), RANGE("range"), REDUCE("reduce"),
		RELATIONSHIPS("relationships");

		private final String implementationName;

		Lists(String implementationName) {
			this.implementationName = implementationName;
		}

		@Override
		public String getImplementationName() {
			return this.implementationName;
		}

	}

	enum Temporals implements FunctionDefinition {

		DATE("date"), DATETIME("datetime"), LOCALDATETIME("localdatetime"), LOCALTIME("localtime"), TIME("time"),
		DURATION("duration");

		private final String implementationName;

		Temporals(String implementationName) {
			this.implementationName = implementationName;
		}

		@Override
		public String getImplementationName() {
			return this.implementationName;
		}

	}

	enum MathematicalFunctions implements FunctionDefinition {

		ABS("abs"), CEIL("ceil"), FLOOR("floor"), RAND("rand", 0, 0), ROUND("round", 1, 3), SIGN("sign"), E("e", 0, 0),
		EXP("exp"), LOG("log"), LOG10("log10"), SQRT("sqrt"), ACOS("acos"), ASIN("asin"), ATAN("atan"),
		ATAN2("atan2", 2, 2), COS("cos"), COT("cot"), DEGREES("degrees"), HAVERSIN("haversin"), PI("pi", 0, 0),
		RADIANS("radians"), SIN("sin"), TAN("tan");

		private final String implementationName;

		private final int minArgs;

		private final int maxArgs;

		MathematicalFunctions(String implementationName) {
			this(implementationName, 1, 1);
		}

		MathematicalFunctions(String implementationName, int minArgs, int maxArgs) {
			this.implementationName = implementationName;
			this.minArgs = minArgs;
			this.maxArgs = maxArgs;
		}

		int getMinArgs() {
			return this.minArgs;
		}

		int getMaxArgs() {
			return this.maxArgs;
		}

		@Override
		public String getImplementationName() {
			return this.implementationName;
		}

	}

}
