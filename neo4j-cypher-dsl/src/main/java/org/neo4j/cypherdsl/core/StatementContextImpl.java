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
package org.neo4j.cypherdsl.core;

import static org.apiguardian.api.API.Status.INTERNAL;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.internal.SchemaNamesBridge;
import org.neo4j.cypherdsl.core.utils.Strings;

/**
 * @author Michael J. Simons
 * @soundtrack Various - Guardians Of The Galaxy: Awesome Mix Vol. 1
 * @since 2021.1.0
 */
@API(status = INTERNAL, since = "2021.1.0")
final class StatementContextImpl implements StatementContext {

	private final AtomicInteger parameterCount = new AtomicInteger();
	private final Map<Parameter<?>, String> parameterNames = new ConcurrentHashMap<>();
	private boolean renderConstantsAsParameters = false;

	/**
	 * Keeps track of unresolved symbolic names.
	 */
	private final Map<SymbolicName, String> resolvedSymbolicNames = new ConcurrentHashMap<>();

	@Override
	public String getParameterName(Parameter<?> parameter) {

		return parameterNames
			.computeIfAbsent(parameter,
				p -> p.isAnon() ? String.format("pcdsl%02d", parameterCount.incrementAndGet()) : p.getName());
	}

	@Override
	public boolean isRenderConstantsAsParameters() {
		return renderConstantsAsParameters;
	}

	void setRenderConstantsAsParameters(boolean renderConstantsAsParameters) {
		this.renderConstantsAsParameters = renderConstantsAsParameters;
	}

	@Override
	public String resolve(SymbolicName symbolicName) {

		return this.resolvedSymbolicNames.computeIfAbsent(symbolicName, k -> {
			String value = k.getValue();
			if (Strings.hasText(value)) {
				return SchemaNamesBridge.sanitize(value, false).orElse(value);
			}
			return String.format("%s%03d", Strings.randomIdentifier(8), resolvedSymbolicNames.size());
		});
	}

	@Override
	public boolean isResolved(SymbolicName symbolicName) {
		return this.resolvedSymbolicNames.containsKey(symbolicName);
	}
}
