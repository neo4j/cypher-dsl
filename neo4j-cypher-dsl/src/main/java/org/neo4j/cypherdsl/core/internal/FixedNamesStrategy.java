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
package org.neo4j.cypherdsl.core.internal;

import org.neo4j.cypherdsl.core.AliasedExpression;
import org.neo4j.cypherdsl.core.Parameter;
import org.neo4j.cypherdsl.core.StatementContext;
import org.neo4j.cypherdsl.core.SymbolicName;

/**
 * A strategy that will always use fixed names for aliased expressions.
 *
 * @author Michael J. Simons
 * @since 2023.2.0
 */
final class FixedNamesStrategy implements NameResolvingStrategy {

	private final StatementContext context;

	FixedNamesStrategy(StatementContext context) {
		this.context = context;
	}

	@Override
	public String resolve(SymbolicName symbolicName, boolean inEntity, boolean inPropertyLookup) {
		return this.context.resolve(symbolicName);
	}

	@Override
	public String resolve(AliasedExpression aliasedExpression, boolean isNew, boolean inLastReturn) {
		return aliasedExpression.getAlias();
	}

	@Override
	public boolean isResolved(SymbolicName symbolicName) {
		return this.context.isResolved(symbolicName);
	}

	@Override
	public String resolve(Parameter<?> parameter) {
		return this.context.getParameterName(parameter);
	}

}
