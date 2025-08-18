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

import static org.apiguardian.api.API.Status.INTERNAL;

/**
 * This class acts as facade towards the {@link StatementContext statement context} and
 * can generate variable and parameter names throughout the lifetime of this visitor.
 *
 * @author Michael J. Simons
 */
@API(status = INTERNAL, since = "2023.2.0")
public sealed interface NameResolvingStrategy permits FixedNamesStrategy, GeneratedNamesStrategy {

	/**
	 * Creates a strategy for using generated names in the given context.
	 * @param context a statement context
	 * @param config for which generated names should be used
	 * @return a new strategy
	 */
	static NameResolvingStrategy useGeneratedNames(StatementContext context, Set<GeneratedNames> config) {
		return new GeneratedNamesStrategy(context, config);
	}

	/**
	 * Creates a strategy that uses generated parameter names.
	 * @param context a statement context
	 * @return a new strategy
	 */
	static NameResolvingStrategy useGeneratedParameterNames(StatementContext context) {
		return new GeneratedNamesStrategy(context, EnumSet.of(GeneratedNames.PARAMETER_NAMES));
	}

	/**
	 * Creates a strategy that uses the given names.
	 * @param context a statement context
	 * @return a new strategy
	 */
	static NameResolvingStrategy useGivenNames(StatementContext context) {
		return new FixedNamesStrategy(context);
	}

	/**
	 * Resolves a symbolic name.
	 * @param symbolicName the name to resolve
	 * @param inEntity {@literal true} if this happens inside an entity
	 * @param inPropertyLookup {@literal true} if this happens for a property lookup
	 * @return a value
	 */
	String resolve(SymbolicName symbolicName, boolean inEntity, boolean inPropertyLookup);

	/**
	 * Resolves an aliased expression.
	 * @param isNew true if it's a newly created {@link AliasedExpression}
	 * @param aliasedExpression the aliased expression to resolve
	 * @param inLastReturn true if the name is resolved as part of the ultimate
	 * {@code RETURN} clause of a statement
	 * @return a value
	 */
	String resolve(AliasedExpression aliasedExpression, boolean isNew, boolean inLastReturn);

	/**
	 * Returns {@code true} if the {@code symbolicName} has been resolved.
	 * @param symbolicName the name that might be already resolved
	 * @return {@code true} if the {@code symbolicName} has been resolved
	 */
	boolean isResolved(SymbolicName symbolicName);

	/**
	 * Resolves a parameter name.
	 * @param parameter the name to resolv
	 * @return a value
	 */
	String resolve(Parameter<?> parameter);

	/**
	 * A callback used together with a {@link ScopingStrategy} to deal with imports into a
	 * local scope.
	 * @param cause the clause that caused the creation of a new scope
	 * @param imports the imports
	 */
	default void enterScope(Visitable cause, Collection<IdentifiableElement> imports) {
	}

	/**
	 * A callback used together with a {@link ScopingStrategy} to deal with exports when
	 * leaving a local scope.
	 * @param cause the clause being left
	 * @param exports the exports
	 */
	default void leaveScope(Visitable cause, Collection<IdentifiableElement> exports) {
	}

}
