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
/**
 * @author Michael J. Simons
 * @since 2023.0.0
 */
@SuppressWarnings({"requires-automatic", "requires-transitive-automatic"})
module org.neo4j.cypherdsl.core {

	requires static com.querydsl.core;
	requires static java.sql;
	requires static org.neo4j.cypherdsl.build.annotations;

	requires transitive org.apiguardian.api;
	requires org.neo4j.cypherdsl.support.schema_name;

	exports org.neo4j.cypherdsl.core;
	exports org.neo4j.cypherdsl.core.ast;
	exports org.neo4j.cypherdsl.core.renderer;
	exports org.neo4j.cypherdsl.core.annotations;
}
