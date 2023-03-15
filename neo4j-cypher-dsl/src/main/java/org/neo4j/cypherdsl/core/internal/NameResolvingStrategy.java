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

import static org.apiguardian.api.API.Status.INTERNAL;

import java.util.Collection;
import java.util.EnumSet;
import java.util.Set;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.AliasedExpression;
import org.neo4j.cypherdsl.core.IdentifiableElement;
import org.neo4j.cypherdsl.core.Parameter;
import org.neo4j.cypherdsl.core.StatementContext;
import org.neo4j.cypherdsl.core.SymbolicName;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.renderer.Configuration.GeneratedNames;

/**
 * This class acts as facade towards the {@link StatementContext statement context} and can generate variable and
 * parameter names throughout the lifetime of this visitor.
 *
 * @author Michael J. Simons
 */
@API(status = INTERNAL, since = "2023.2.0")
public sealed interface NameResolvingStrategy permits FixedNamesStrategy, GeneratedNamesStrategy {

	/**
	 * Creates a strategy for using generated names in the given context
	 *
	 * @param context A statement context
	 * @param config for which generated names should be used
	 * @return A new strategy
	 */
	static NameResolvingStrategy useGeneratedNames(StatementContext context, Set<GeneratedNames> config) {
		return new GeneratedNamesStrategy(context, config);
	}

	/**
	 * Creates a strategy that uses generated parameter names.
	 *
	 * @param context A statement context
	 * @return A new strategy
	 */
	static NameResolvingStrategy useGeneratedParameterNames(StatementContext context) {
		return new GeneratedNamesStrategy(context, EnumSet.of(GeneratedNames.PARAMETER_NAMES));
	}

	/**
	 * Creates a strategy that uses the given names.
	 *
	 * @param context A statement context
	 * @return A new strategy
	 */
	static NameResolvingStrategy useGivenNames(StatementContext context) {
		return new FixedNamesStrategy(context);
	}

	/**
	 * Resolves a symbolic name
	 *
	 * @param symbolicName     The name to resolve
	 * @param inEntity         {@literal true} if this happens inside an entity
	 * @param inPropertyLookup {@literal true} if this happens for a property lookup
	 * @return A value
	 */
	String resolve(SymbolicName symbolicName, boolean inEntity, boolean inPropertyLookup);

	/**
	 * Resolves an aliased expression.
	 *
	 * @param aliasedExpression The aliased expression to resolve
	 * @return A value
	 */
	String resolve(AliasedExpression aliasedExpression, boolean isNew, boolean inLastReturn);

	/**
	 * @param symbolicName The name that might be already resolved
	 * @return {@literal true} if the {@code symbolicName} has been resolved
	 */
	boolean isResolved(SymbolicName symbolicName);

	/**
	 * Resolves a parameter name
	 *
	 * @param parameter The name to resolv
	 * @return A value
	 */
	String resolve(Parameter<?> parameter);

	default void enterScope(Visitable cause, Collection<IdentifiableElement> imports) {
	}

	default void leaveScope(Visitable cause, Collection<IdentifiableElement> exports) {
	}
}
