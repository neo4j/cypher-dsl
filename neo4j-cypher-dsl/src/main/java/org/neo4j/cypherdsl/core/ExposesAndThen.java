/*
 * Copyright (c) 2019-2024 "Neo4j,"
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
 * Allows chaining of statements to some other builders.
 * @author Michael J. Simons
 * @param <S> The source type of the builder that ultimately builds the statement ("SELF")
 * @param <R> The result type of the statement
 */
public interface ExposesAndThen<
	S extends ExposesAndThen<S, R>, R extends Statement> extends StatementBuilder.BuildableStatement<R> {

	ExposesAndThen<S, R> andThen(Statement statement);
}
