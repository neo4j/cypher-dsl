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
package org.neo4j.cypherdsl.core;

import static org.apiguardian.api.API.Status.INTERNAL;

import org.apiguardian.api.API;

import com.querydsl.core.types.Expression;

/**
 * This factory is meant to decouple the instantiating respectively concrete usage of classes on the provided path as
 * much as possible, to avoid eager loading by some JVM and in turn, a class not found exception.
 *
 * @author Michael J. Simons
 * @soundtrack Various - Chef Aid: The South Park Album
 * @since 2021.1.0
 */
@API(status = INTERNAL, since = "2021.1.0")
final class ForeignAdapterFactory {

	@SuppressWarnings("unchecked") // We do check the type of expression
	<FE> ForeignAdapter<FE> getAdapterFor(FE expression) {

		if (expression == null) {
			throw new IllegalArgumentException("Cannot adapt literal NULL expressions.");
		}

		if (!(expression instanceof Expression)) {
			throw new IllegalArgumentException(
				"Cannot adapt expressions of type " + expression.getClass().getName() + " to Cypher-DSL expressions.");
		}

		return (ForeignAdapter<FE>) new QueryDSLAdapter((Expression<?>) expression);
	}
}
