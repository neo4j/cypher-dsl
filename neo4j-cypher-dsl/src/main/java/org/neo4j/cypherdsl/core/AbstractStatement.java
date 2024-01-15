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

import static org.apiguardian.api.API.Status.INTERNAL;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

import org.apiguardian.api.API;
import org.jetbrains.annotations.NotNull;
import org.neo4j.cypherdsl.core.ParameterCollectingVisitor.ParameterInformation;
import org.neo4j.cypherdsl.core.internal.StatementContext;
import org.neo4j.cypherdsl.core.renderer.Renderer;

/**
 * The abstract statement provides possible state shared across various statement implementations. Use cases are collecting
 * bound parameter values, possible configuration of the renderer and similar.
 *
 * @author Michael J. Simons
 * @author Andreas Berger
 * @since 2021.0.0
 */
@API(status = INTERNAL, since = "2021.0.0")
abstract class AbstractStatement implements Statement {

	/**
	 * Provides context during visiting of this statement.
	 */
	private final StatementContextImpl context = new StatementContextImpl();

	/**
	 * The collected parameter information (names only and names + values). Will be initialized with Double-checked locking into an unmodifiable object.
	 */
	@SuppressWarnings("squid:S3077")
	private volatile ParameterInformation parameterInformation;

	/**
	 * The rendered Cypher statement.
	 */
	private volatile String cypher;

	/**
	 * Identifiable expressions. Will be initialized with Double-checked locking into an unmodifiable collection.
	 */
	@SuppressWarnings("squid:S3077")
	private volatile Collection<Expression> identifiables;

	@NotNull
	@Override
	public StatementContext getContext() {
		return context;
	}

	@Override
	public synchronized boolean isRenderConstantsAsParameters() {
		return this.context.isRenderConstantsAsParameters();
	}

	@Override
	public void setRenderConstantsAsParameters(boolean renderConstantsAsParameters) {

		synchronized (this) {
			this.context.setRenderConstantsAsParameters(renderConstantsAsParameters);
			this.cypher = null;
			this.parameterInformation = null;
		}
	}

	@NotNull
	@Override
	public Map<String, Object> getParameters() {
		return getParameterInformation().values;
	}

	@NotNull
	@Override
	public Set<String> getParameterNames() {
		return getParameterInformation().names;
	}

	@NotNull
	public Collection<Expression> getIdentifiableExpressions() {

		Collection<Expression> result = this.identifiables;
		if (result == null) {
			synchronized (this) {
				result = this.identifiables;
				if (result == null) {
					IdentifiableExpressionCollectingVisitor visitor = new IdentifiableExpressionCollectingVisitor();
					this.accept(visitor);
					this.identifiables = visitor.getResult();
					result = this.identifiables;
				}
			}
		}
		return result;
	}

	@NotNull
	@Override
	public String getCypher() {

		String result = this.cypher;
		if (result == null) {
			synchronized (this) {
				result = this.cypher;
				if (result == null) {
					this.cypher = Renderer.getDefaultRenderer().render(this);
					result = this.cypher;
				}
			}
		}
		return result;
	}

	private ParameterInformation getParameterInformation() {

		ParameterInformation result = this.parameterInformation;
		if (result == null) {
			synchronized (this) {
				result = this.parameterInformation;
				if (result == null) {
					this.parameterInformation = collectParameters();
					result = this.parameterInformation;
				}
			}
		}
		return result;
	}

	/**
	 * Collects all bound parameters from the statement.
	 *
	 * @return a map of used parameters with its bound values
	 * @see org.neo4j.cypherdsl.core.Parameter#withValue(Object)
	 */
	private ParameterInformation collectParameters() {

		ParameterCollectingVisitor parameterCollectingVisitor = new ParameterCollectingVisitor(getContext());
		this.accept(parameterCollectingVisitor);
		return parameterCollectingVisitor.getResult();
	}
}
