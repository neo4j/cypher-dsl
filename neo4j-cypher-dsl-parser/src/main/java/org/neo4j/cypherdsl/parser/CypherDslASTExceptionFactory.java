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
package org.neo4j.cypherdsl.parser;

import static org.apiguardian.api.API.Status.INTERNAL;

import java.util.List;

import org.apiguardian.api.API;
import org.neo4j.cypher.internal.parser.common.ast.factory.ASTExceptionFactory;
import org.neo4j.gqlstatus.ErrorGqlStatusObject;

/**
 * @author Michael J. Simons
 * @since 2021.3.0
 */
@API(status = INTERNAL, since = "2021.3.0")
enum CypherDslASTExceptionFactory implements ASTExceptionFactory {
	INSTANCE;

	@Override
	public Exception syntaxException(ErrorGqlStatusObject errorGqlStatusObject, String s, List<String> list,
		Exception e, int i, int i1, int i2) {
		return new RuntimeException(errorGqlStatusObject.getMessage());
	}

	@Override
	public Exception syntaxException(String got, List<String> expected, Exception source, int offset, int line, int column) {
		return new RuntimeException(source.getMessage());
	}

	@Override
	public Exception syntaxException(Exception source, int offset, int line, int column) {
		return new RuntimeException(source.getMessage());
	}

	@Override
	public Exception syntaxException(ErrorGqlStatusObject errorGqlStatusObject, Exception e, int i, int i1, int i2) {
		return new RuntimeException(errorGqlStatusObject.getMessage());
	}
}
