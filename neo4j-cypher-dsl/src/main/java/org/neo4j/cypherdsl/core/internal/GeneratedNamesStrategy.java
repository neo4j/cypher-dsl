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
package org.neo4j.cypherdsl.core.internal;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

import org.neo4j.cypherdsl.core.Parameter;
import org.neo4j.cypherdsl.core.StatementContext;
import org.neo4j.cypherdsl.core.SymbolicName;

final class GeneratedNamesStrategy implements NameResolvingStrategy {

	record Key(Object value) {
	}

	private final AtomicInteger variableCount = new AtomicInteger(-1);

	private final AtomicInteger parameterCount = new AtomicInteger(-1);

	private final StatementContext statementContext;

	private final Map<Key, String> nameLookup = new ConcurrentHashMap<>();

	GeneratedNamesStrategy(StatementContext statementContext) {
		this.statementContext = statementContext;
	}

	@Override
	public String resolve(SymbolicName symbolicName, boolean inEntity) {
		return nameLookup.computeIfAbsent(
			new Key(symbolicName),
			key -> inEntity ? String.format("v%d", variableCount.incrementAndGet()) : statementContext.resolve((SymbolicName) key.value())
		);
	}

	@Override
	public String resolve(Parameter<?> parameter) {
		return nameLookup.computeIfAbsent(
			new Key(parameter),
			key -> {
				var p = (Parameter<?>) key.value();
				return !p.isAnon() ? String.format("p%d", parameterCount.incrementAndGet()) : statementContext.getParameterName(p);
			});
	}

	@Override
	public boolean isResolved(SymbolicName symbolicName) {
		return statementContext.isResolved(symbolicName);
	}
}
