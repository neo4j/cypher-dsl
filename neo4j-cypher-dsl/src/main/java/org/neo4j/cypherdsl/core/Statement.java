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

import static org.apiguardian.api.API.Status.STABLE;
import static org.apiguardian.api.API.Status.INTERNAL;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.apiguardian.api.API;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
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
	@NotNull @Contract(pure = true)
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
	@NotNull
	static Statement of(@NotNull List<Clause> clauses) {

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
	@NotNull
	static Statement usingPeriodic(Integer batchSize, @NotNull List<Clause> clauses) {

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
	@NotNull @Contract(pure = true)
	static OngoingStandaloneCallWithoutArguments call(String... namespaceAndProcedure) {

		return new DefaultStatementBuilder.StandaloneCallBuilder(ProcedureName.from(namespaceAndProcedure));
	}

	/**
	 * After a statement has been build, all parameters that have been added to the statement can be retrieved through
	 * this method. The result will only contain parameters with a defined value. If you are interested in all parameter
	 * names, use {@link #getParameterNames()}.
	 * <p>
	 * The map can be used for example as an argument with various methods on the Neo4j Java Driver that allow the
	 * execution of parameterized queries.
	 * <p>
	 * This method is threadsafe
	 *
	 * @return A map of all parameters with a bound value.
	 * @since 2021.0.0
	 */
	@NotNull @Contract(pure = true)
	Map<String, Object> getParameters();

	/**
	 * After the statement has been build, this method returns a list of all parameter names used, regardless whether
	 * a value was bound to the parameter o not.
	 * <p>
	 * This method is threadsafe
	 *
	 * @return A set of parameter names being used.
	 * @since 2021.0.0
	 */
	@NotNull @Contract(pure = true)
	Collection<String> getParameterNames();

	/**
	 * @return The set of identifiable expressions that are available when this statement ends.
	 * @since 2021.3.2
	 */
	@NotNull @Contract(pure = true)
	Collection<Expression> getIdentifiableExpressions();

	/**
	 * This method uses the default renderer to create a String representation of this statement. The generated Cypher
	 * will use escaped literals and correct placeholders like {@code $param} for parameters. The placeholders for
	 * parameters can be retrieved via {@link #getParameterNames}. Bounded values for parameters can be retrieved via
	 * {@link #getParameters()}.
	 * <p>
	 * This method is threadsafe
	 *
	 * @return A valid Cypher statement
	 * @since 2021.0.0
	 */
	@NotNull @Contract(pure = true)
	String getCypher();

	/**
	 * @return The context of this statement, allowing access to parameter names etc.
	 */
	@API(status = INTERNAL, since = "2021.0.0")
	@NotNull @Contract(pure = true)
	StatementContext getContext();

	/**
	 * Some constants may be rendered as parameters.
	 *
	 * @return True if literal parameters hav
	 */
	@Contract(pure = true)
	boolean isRenderConstantsAsParameters();

	/**
	 * Use this method to configure whether some constant values should be rendered as parameters or as literals before
	 * the first call to {@link Statement#getParameters()} or {@link Statement#getCypher()}.
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
	 * @return True if this statement can be assured to return something.
	 */
	default boolean doesReturnOrYield() {
		return this instanceof ResultStatement || this instanceof UnionQuery;
	}
}
