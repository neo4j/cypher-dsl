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
module org.neo4j.cypherdsl.core {

	requires static org.neo4j.driver;
	requires static org.jetbrains.annotations;
	requires static org.reactivestreams;
	requires static com.querydsl.core;
	requires static reactor.core;

	requires org.apiguardian.api;

	exports org.neo4j.cypherdsl.core;
	exports org.neo4j.cypherdsl.core.ast;
	exports org.neo4j.cypherdsl.core.executables;
	exports org.neo4j.cypherdsl.core.renderer;

	exports org.neo4j.cypherdsl.core.utils to org.neo4j.cypherdsl.codegen.core, org.neo4j.cypherdsl.parser;
	exports org.neo4j.cypherdsl.core.internal to org.neo4j.cypherdsl.parser;
}
