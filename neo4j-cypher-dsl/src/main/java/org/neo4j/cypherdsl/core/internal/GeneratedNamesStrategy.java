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
package org.neo4j.cypherdsl.core.internal;

import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import org.neo4j.cypherdsl.core.AliasedExpression;
import org.neo4j.cypherdsl.core.IdentifiableElement;
import org.neo4j.cypherdsl.core.Parameter;
import org.neo4j.cypherdsl.core.StatementContext;
import org.neo4j.cypherdsl.core.SymbolicName;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.renderer.Configuration.GeneratedNames;

/**
 * @author Michael J. Simons
 * @since 2023.2.0
 */
final class GeneratedNamesStrategy implements NameResolvingStrategy {

	record Key(Object value) {

		static Key of(Object o) {
			if (o instanceof AliasedExpression aliasedExpression) {
				return new Key(aliasedExpression.asName());
			}
			return new Key(o);
		}
	}

	/**
	 * Unscoped counter for parameters.
	 */
	private final AtomicInteger parameterCount = new AtomicInteger(0);

	/**
	 * Stack of counters per scope for the variable names.
	 */
	private final Deque<AtomicInteger> scopedVariableCount = new ArrayDeque<>();

	/**
	 * Scope for the already generated names.
	 */
	private final Deque<Map<Key, String>> scopedNameLookup = new ArrayDeque<>();

	/**
	 * Used names that are either brought into a child scope or exported into the containing scope.
	 */
	private final Deque<Set<String>> scopedNamesUsed = new ArrayDeque<>();

	private final StatementContext statementContext;

	private final Set<GeneratedNames> config;

	GeneratedNamesStrategy(StatementContext statementContext, Set<GeneratedNames> config) {
		this.statementContext = statementContext;
		this.config = config;
		this.enterScope(null, List.of());
	}

	/**
	 * @return the lookup table for names in the current scope
	 */
	Map<Key, String> nameLookup() {
		return Objects.requireNonNull(scopedNameLookup.peek());
	}

	@Override
	public void enterScope(Visitable cause, Collection<IdentifiableElement> imports) {

		var newNameLookup = new HashMap<Key, String>();
		var newUsedNames = new HashSet<String>();

		var outerNameLookup = scopedNameLookup.peek();
		if (outerNameLookup != null) {
			for (IdentifiableElement anImport : imports) {
				var theKey = Key.of(anImport);
				if (outerNameLookup.containsKey(theKey)) {
					newNameLookup.put(theKey, outerNameLookup.get(theKey));
				}
			}
			newUsedNames.addAll(outerNameLookup.values());
		}

		this.scopedVariableCount.push(new AtomicInteger(0));
		this.scopedNameLookup.push(newNameLookup);
		this.scopedNamesUsed.push(newUsedNames);
	}

	@Override
	public void leaveScope(Visitable cause, Collection<IdentifiableElement> exports) {

		this.scopedVariableCount.pop();
		var innerNameLookup = this.scopedNameLookup.pop();
		this.scopedNamesUsed.pop();

		var outerNameLookup = Objects.requireNonNull(this.scopedNameLookup.peek());
		var previouslyUsedNames = Objects.requireNonNull(this.scopedNamesUsed.peek());

		for (IdentifiableElement anExport : exports) {
			var theKey = Key.of(anExport);
			if (innerNameLookup.containsKey(theKey)) {
				outerNameLookup.put(theKey, innerNameLookup.get(theKey));
			} else if (anExport instanceof AliasedExpression name) {
				outerNameLookup.put(theKey, name.getAlias());
			} else if (anExport instanceof SymbolicName name) {
				outerNameLookup.put(theKey, name.getValue());
			}
		}
		previouslyUsedNames.addAll(innerNameLookup.values());
	}

	@Override
	public String resolve(SymbolicName symbolicName, boolean inEntity, boolean inPropertyLookup) {

		if (inPropertyLookup) {
			return statementContext.resolve(symbolicName);
		}

		// Maybe it has been used as an alias, so we can't skip early
		var theKey = Key.of(symbolicName);
		var nameLookup = nameLookup();

		if (!config.contains(GeneratedNames.ENTITY_NAMES) || (!inEntity && !config.contains(GeneratedNames.ALL_ALIASES) && !config.contains(GeneratedNames.INTERNAL_ALIASES_ONLY))) {
			// Not using nameLookup.getOrDefault() to not resolve the name early
			if (nameLookup.containsKey(theKey)) {
				return nameLookup.get(theKey);
			}
			return statementContext.resolve(symbolicName);
		}

		return nameLookup.computeIfAbsent(theKey, key -> newName());
	}

	private String newName() {

		String name;
		var namesUsed = Objects.requireNonNull(scopedNamesUsed.peek());
		var variableCount = Objects.requireNonNull(this.scopedVariableCount.peek());
		do {
			name = String.format("v%d", variableCount.getAndIncrement());
		} while (namesUsed.contains(name));
		return name;
	}

	@Override
	public String resolve(AliasedExpression aliasedExpression, boolean isNew, boolean inLastReturn) {

		if (!(config.contains(GeneratedNames.ALL_ALIASES) || (config.contains(GeneratedNames.INTERNAL_ALIASES_ONLY) && !inLastReturn))) {
			return aliasedExpression.getAlias();
		}

		var result = newName();
		nameLookup().put(Key.of(aliasedExpression), result);
		return result;
	}

	@Override
	public String resolve(Parameter<?> parameter) {

		if (!config.contains(GeneratedNames.PARAMETER_NAMES)) {
			return statementContext.getParameterName(parameter);
		}

		return nameLookup().computeIfAbsent(
			Key.of(parameter),
			key -> {
				var p = (Parameter<?>) key.value();
				return !p.isAnon() ? String.format("p%d", parameterCount.getAndIncrement()) : statementContext.getParameterName(p);
			});
	}

	@Override
	public boolean isResolved(SymbolicName symbolicName) {
		return statementContext.isResolved(symbolicName);
	}
}
