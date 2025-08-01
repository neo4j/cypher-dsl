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

import static org.apiguardian.api.API.Status.STABLE;
import static org.apiguardian.api.API.Status.INTERNAL;

import java.util.List;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.StatementBuilder.OngoingStandaloneCallWithoutArguments;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.internal.LoadCSV;
import org.neo4j.cypherdsl.core.internal.ProcedureName;
import org.neo4j.cypherdsl.core.internal.UsingPeriodicCommit;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * Shall be the common interfaces for queries that we support.
 * <p>
 * For reference see: <a href="https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/Cypher.html">Cypher</a>.
 * We have skipped the intermediate "Query" structure so a statement in the context of this generator is either a
 * {@link RegularQuery} or a {@code StandaloneCall}.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public interface Statement extends Visitable {

	/**
	 * @return A new statement builder.
	 */
	static StatementBuilder builder() {

		return new DefaultStatementBuilder();
	}

	/**
	 * Creates a statement based on a list of {@link Clause clauses}. It is your task to provide a sanity check of the
	 * clauses. The builder will use the clauses "as is", neither change their order nor their type.
	 *
	 * @param clauses A list of clauses, must not be null
	 * @return A statement
	 * @since 2021.3.0
	 */
	static Statement of(List<Clause> clauses) {

		Assertions.notNull(clauses, "Clauses must not be null.");
		return new ClausesBasedStatement(clauses, null);
	}

	/**
	 * Creates a statement based on a list of {@link Clause clauses} and prepends it with {@literal USING PERIODIC COMMIT}.
	 * It is your task to provide a sanity check of the clauses. The builder will use the clauses "as is",
	 * neither change their order nor their type.
	 * <p>
	 * The {@literal USING PERIODIC HINT} is only valid right before the {@literal LOAD CSV clause}.
	 *
	 * @param batchSize The batch size to pass to {@literal PERIODIC COMMIT}.
	 * @param clauses   A list of clauses, must not be null
	 * @return A statement
	 * @since 2021.3.0
	 */
	@API(status = STABLE, since = "2021.3.0")
	static Statement usingPeriodic(Integer batchSize, List<Clause> clauses) {

		Assertions.notNull(clauses, "Clauses must not be null.");
		Assertions.isTrue(!clauses.isEmpty(), "Clauses must not be empty.");
		Assertions.isInstanceOf(LoadCSV.class, clauses.get(0), "First clause must be a LOAD CSV clause.");
		return new ClausesBasedStatement(clauses, new UsingPeriodicCommit(batchSize));
	}

	/**
	 * @param namespaceAndProcedure The fully qualified name of a stored procedure. Each part can be given as a separate
	 *                              String, but a fully qualified String is ok as well.
	 * @return An entry point into a statement that starts with a call to stored procedure.
	 */
	static OngoingStandaloneCallWithoutArguments call(String... namespaceAndProcedure) {

		return new DefaultStatementBuilder.StandaloneCallBuilder(ProcedureName.from(namespaceAndProcedure));
	}

	/**
	 * Analyzes the statement and provides access to the resolved properties, their (potential) owners and the context
	 * in which they have been resolved. Identifiable expressions may be retrieved via {@link StatementCatalog#getIdentifiableExpressions()}.
	 *
	 * @return An immutable object representing properties resolved in a statement together with their context and owner
	 * @since 2023.1.0
	 */
	StatementCatalog getCatalog();

	/**
	 * This method uses the default renderer to create a String representation of this statement. The generated Cypher
	 * will use escaped literals and correct placeholders like {@code $param} for parameters. The placeholders for
	 * parameters can be retrieved via {@link StatementCatalog#getParameterNames}. Bounded values for parameters can be
	 * retrieved via {@link StatementCatalog#getParameters()}.
	 * <p>
	 * This method is thread safe.
	 *
	 * @return A valid Cypher statement
	 * @since 2021.0.0
	 */
	String getCypher();

	/**
	 * @return The context of this statement, allowing access to parameter names etc.
	 */
	@API(status = INTERNAL, since = "2021.0.0")
	StatementContext getContext();

	/**
	 * Some constants may be rendered as parameters.
	 *
	 * @return True if literal parameters hav
	 */
	boolean isRenderConstantsAsParameters();

	/**
	 * Use this method to configure whether some constant values should be rendered as parameters or as literals before
	 * the first call to {@link StatementCatalog#getParameters()} or {@link Statement#getCypher()}.
	 * <p>
	 * Renderers are free to chose to ignore this.
	 *
	 * @param renderConstantsAsParameters Set to true to render constants as parameters (when using {@link #getCypher()}.
	 */
	void setRenderConstantsAsParameters(boolean renderConstantsAsParameters);

	/**
	 * Represents {@code RegularQuery}.
	 *
	 * @since 1.0
	 */
	interface RegularQuery extends Statement {
	}

	/**
	 * Represents a {@code SingleQuery}.
	 *
	 * @since 1.0
	 */
	interface SingleQuery extends RegularQuery {
	}

	/**
	 * Represents a {@code UnionQuery}.
	 *
	 * @since 2023.0.0
	 */
	sealed interface UnionQuery extends RegularQuery permits UnionQueryImpl {
	}

	/**
	 * Represents a {@code USE} statement, utilizing a composite graph call. A statement utilizing composite databases
	 * might use an {@code EXPLAIN} clause but cannot be profiled (as of Neo4j 5.3).
	 *
	 * @since 2023.0.0
	 */
	sealed interface UseStatement extends Statement permits DecoratedQuery {

		/**
		 * @return Creates a statement that returns an explain plan for the original statement.
		 */
		default Statement explain() {
			return DecoratedQuery.explain(this);
		}
	}

	/**
	 * @return True if this statement can be assured to return something.
	 */
	default boolean doesReturnOrYield() {
		return this instanceof ResultStatement || this instanceof UnionQueryImpl;
	}
}
