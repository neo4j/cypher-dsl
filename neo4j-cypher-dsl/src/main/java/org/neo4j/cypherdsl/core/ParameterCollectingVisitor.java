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
package org.neo4j.cypherdsl.core;

import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.internal.ConstantParameterHolder;
import org.neo4j.cypherdsl.core.internal.NameResolvingStrategy;
import org.neo4j.cypherdsl.core.internal.ReflectiveVisitor;

/**
 * This is an implementation of a visitor to the Cypher AST created by the Cypher builder
 * based on the {@link ReflectiveVisitor reflective visitor}.
 * <p>
 * It collects all parameters with bound value into a map of used parameters.
 * <p>
 *
 * @author Andreas Berger
 * @author Michael J. Simons
 * @since 2021.0.0
 */
final class ParameterCollectingVisitor implements Visitor {

	final Map<String, String> parameterMapping = new TreeMap<>();

	private final StatementContext statementContext;

	private final boolean renderConstantsAsParameters;

	private final Set<String> parameterNames = new TreeSet<>();

	private final Map<String, Object> parameterValues = new TreeMap<>();

	private final Map<String, Set<Object>> erroneousParameters = new TreeMap<>();

	private final NameResolvingStrategy nameGenerator;

	ParameterCollectingVisitor(StatementContext statementContext, boolean renderConstantsAsParameters) {
		this.statementContext = statementContext;
		this.nameGenerator = NameResolvingStrategy.useGeneratedParameterNames(statementContext);
		this.renderConstantsAsParameters = renderConstantsAsParameters;
	}

	@Override
	public void enter(Visitable segment) {

		if (!(segment instanceof Parameter<?> parameter)) {
			return;
		}

		String parameterName = this.statementContext.getParameterName(parameter);
		Object newValue = parameter.getValue();
		if (newValue instanceof ConstantParameterHolder constantParameterHolder) {
			if (!this.renderConstantsAsParameters) {
				return;
			}
			newValue = constantParameterHolder.getValue();
		}
		boolean knownParameterName = !this.parameterNames.add(parameterName);
		if (!(knownParameterName || parameter.isAnon())) {
			this.parameterMapping.put(parameterName, this.nameGenerator.resolve(parameter));
		}

		Object oldValue = (knownParameterName && this.parameterValues.containsKey(parameterName))
				? this.parameterValues.get(parameterName) : Parameter.NO_VALUE;
		if (parameter.hasValue()) {
			this.parameterValues.put(parameterName, newValue);
		}
		if (knownParameterName && !Objects.equals(oldValue, newValue)) {
			Set<Object> conflictingObjects = this.erroneousParameters.computeIfAbsent(parameterName, s -> {
				HashSet<Object> list = new HashSet<>();
				list.add(oldValue);
				return list;
			});
			conflictingObjects.add(newValue);
		}
	}

	ParameterInformation getResult() {

		if (!this.erroneousParameters.isEmpty()) {
			throw new ConflictingParametersException(this.erroneousParameters);
		}

		return new ParameterInformation(this.parameterNames, this.parameterValues, this.parameterMapping);
	}

	static final class ParameterInformation {

		final Set<String> names;

		final Map<String, Object> values;

		final Map<String, String> renames;

		ParameterInformation(Set<String> names, Map<String, Object> values, Map<String, String> renames) {
			this.names = Collections.unmodifiableSet(names);
			this.values = Collections.unmodifiableMap(values);
			this.renames = Collections.unmodifiableMap(renames);
		}

	}

}
