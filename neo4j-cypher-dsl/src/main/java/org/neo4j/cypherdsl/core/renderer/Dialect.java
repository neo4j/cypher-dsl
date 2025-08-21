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
package org.neo4j.cypherdsl.core.renderer;

import java.util.Optional;
import java.util.function.Function;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.Comparison;
import org.neo4j.cypherdsl.core.FunctionInvocation;
import org.neo4j.cypherdsl.core.Subquery;
import org.neo4j.cypherdsl.core.With;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * The dialect to be used when rendering a statement into Cypher.
 *
 * @author Michael J. Simons
 * @since 2022.3.0
 */
@API(status = STABLE, since = "2022.3.0")
public enum Dialect {

	/**
	 * Neo4j 4.4 and earlier.
	 */
	NEO4J_4,

	/**
	 * Neo4j 5.
	 */
	NEO4J_5 {

		private final DefaultNeo4j5HandlerSupplier handlerSupplier = new DefaultNeo4j5HandlerSupplier();

		@Override
		Class<? extends Visitor> getHandler(Visitable visitable) {
			return this.handlerSupplier.apply(visitable).orElseGet(() -> super.getHandler(visitable));
		}
	},

	/**
	 * Enhanced Neo4j 5 dialect that renders importing with statements for call subqueries
	 * as variable scoped subqueries, suitable for Neo4j >= 5.23.
	 *
	 * @since 2024.1.0
	 */
	NEO4J_5_23 {
		private final DefaultNeo4j5HandlerSupplier handlerSupplier = new DefaultNeo4j5HandlerSupplierWithNewImportScopeSubquerySupport();

		@Override
		Class<? extends Visitor> getHandler(Visitable visitable) {
			return this.handlerSupplier.apply(visitable).orElseGet(() -> super.getHandler(visitable));
		}
	},

	/**
	 * A Neo4j 5.26 and higher dialect that always renders the {@code CYPHER 5} prefix.
	 * @since 2024.5.0
	 * @deprecated Use {@link #NEO4J_5_CYPHER_5} if neccessary or
	 * {@link #NEO4J_5_DEFAULT_CYPHER} in all other cases.
	 */
	@Deprecated(forRemoval = true)
	NEO4J_5_26 {
		@Override
		Class<? extends Visitor> getHandler(Visitable visitable) {
			return NEO4J_5_23.getHandler(visitable);
		}

		@Override
		Optional<String> getPrefix() {
			return Optional.of("CYPHER 5 ");
		}
	},

	/**
	 * Essentially the same as {@link #NEO4J_5_23} using the databases default Cypher
	 * version. This is the default dialect for Cypher-DSL 2025.0.0 and onwards.
	 *
	 * @since 2025.0.0
	 */
	NEO4J_5_DEFAULT_CYPHER {
		@Override
		Class<? extends Visitor> getHandler(Visitable visitable) {
			return NEO4J_5_23.getHandler(visitable);
		}
	},

	/**
	 * Essentially the same as {@link #NEO4J_5_23} but also enabling the {@code CYPHER 5}
	 * prefix on generated statements. This dialect works on Neo4j 5.26 and higher.
	 * Genrally, {@link #NEO4J_5_DEFAULT_CYPHER} is preferrable.
	 *
	 * @since 2025.0.0
	 */
	NEO4J_5_CYPHER_5 {
		@Override
		Class<? extends Visitor> getHandler(Visitable visitable) {
			return NEO4J_5_23.getHandler(visitable);
		}

		@Override
		Optional<String> getPrefix() {
			return Optional.of("CYPHER 5 ");
		}
	},

	/**
	 * Essentially the same as {@link #NEO4J_5_23} but also enabling the {@code CYPHER 5}
	 * prefix on generated statements. This dialect works on Neo4j 5.26 and higher.
	 * Genrally, {@link #NEO4J_5_23} is preferable.
	 *
	 * @since 2025.0.0
	 */
	NEO4J_5_CYPHER_25 {
		@Override
		Class<? extends Visitor> getHandler(Visitable visitable) {
			return NEO4J_5_23.getHandler(visitable);
		}

		@Override
		Optional<String> getPrefix() {
			return Optional.of("CYPHER 25 ");
		}
	},

	/**
	 * A placeholder for Neo4j 2025 calver. Same behaviour as
	 * {@link #NEO4J_5_DEFAULT_CYPHER} in this version of Cypher-DSL.
	 *
	 * @since 2025.0.0
	 */
	NEO4J_2025 {
		@Override
		Class<? extends Visitor> getHandler(Visitable visitable) {
			return NEO4J_5_23.getHandler(visitable);
		}
	};

	Class<? extends Visitor> getHandler(Visitable visitable) {
		return null;
	}

	Optional<String> getPrefix() {
		return Optional.empty();
	}

	private static class DefaultNeo4j5HandlerSupplier
			implements Function<Visitable, Optional<Class<? extends Visitor>>> {

		@Override
		public Optional<Class<? extends Visitor>> apply(Visitable visitable) {
			if (visitable instanceof FunctionInvocation) {
				return Optional.of(Neo4j5FunctionInvocationVisitor.class);
			}
			else if (visitable instanceof Comparison) {
				return Optional.of(Neo4j5ComparisonVisitor.class);
			}
			return Optional.empty();
		}

	}

	private static final class DefaultNeo4j5HandlerSupplierWithNewImportScopeSubquerySupport
			extends DefaultNeo4j5HandlerSupplier {

		@Override
		public Optional<Class<? extends Visitor>> apply(Visitable visitable) {
			return super.apply(visitable).or(() -> {
				if (visitable instanceof Subquery || visitable instanceof With) {
					return Optional.of(Neo4j523SubqueryVisitor.class);
				}
				return Optional.empty();
			});
		}

	}

}
