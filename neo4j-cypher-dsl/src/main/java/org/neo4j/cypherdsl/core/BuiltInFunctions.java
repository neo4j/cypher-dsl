/*
 * Copyright (c) 2019-2020 "Neo4j,"
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
 * A (non exhaustive) list of builtin functions.
 *
 * @author Michael J. Simons
 * @soundtrack Metallica - ReLoad
 * @since 2020.1.0
 */
@API(status = Status.INTERNAL, since = "2020.1.0")
final class BuiltInFunctions {

	enum Predicates implements FunctionDefinition {

		ALL("all"),
		ANY("any"),
		EXISTS("exists"),
		NONE("none"),
		SINGLE("single");

		private final String implementationName;

		Predicates(String implementationName) {
			this.implementationName = implementationName;
		}

		@Override
		public String getImplementationName() {
			return implementationName;
		}
	}

	enum Scalars implements FunctionDefinition {

		COALESCE("coalesce"),
		END_NODE("endNode"),
		HEAD("head"),
		ID("id"),
		LAST("last"),

		PROPERTIES("properties"),
		SHORTEST_PATH("shortestPath"),
		SIZE("size"),

		START_NODE("startNode"),
		TYPE("type");

		private final String implementationName;

		Scalars(String implementationName) {
			this.implementationName = implementationName;
		}

		@Override
		public String getImplementationName() {
			return implementationName;
		}
	}

	enum Strings implements FunctionDefinition {

		TO_LOWER("toLower");

		private final String implementationName;

		Strings(String implementationName) {
			this.implementationName = implementationName;
		}

		@Override
		public String getImplementationName() {
			return implementationName;
		}
	}

	enum Spatials implements FunctionDefinition {

		POINT("point"),
		DISTANCE("distance");

		private final String implementationName;

		Spatials(String implementationName) {
			this.implementationName = implementationName;
		}

		@Override
		public String getImplementationName() {
			return implementationName;
		}
	}

	enum Aggregates implements FunctionDefinition {

		AVG("avg"),
		COLLECT("collect"),
		COUNT("count"),
		MAX("max"),
		MIN("min"),
		PERCENTILE_CONT("percentileCont"),
		PERCENTILE_DISC("percentileDisc"),
		ST_DEV("stDev"),
		ST_DEV_P("stDevP"),
		SUM("sum");

		private final String implementationName;

		Aggregates(String implementationName) {
			this.implementationName = implementationName;
		}

		@Override
		public String getImplementationName() {
			return implementationName;
		}

		@Override public boolean isAggregate() {
			return true;
		}

	}

	enum Lists implements FunctionDefinition {
		LABELS("labels"),
		NODES("nodes"),
		RANGE("range"),
		RELATIONSHIPS("relationships");

		private final String implementationName;

		Lists(String implementationName) {
			this.implementationName = implementationName;
		}

		@Override
		public String getImplementationName() {
			return implementationName;
		}

	}

	private BuiltInFunctions() {
	}
}
