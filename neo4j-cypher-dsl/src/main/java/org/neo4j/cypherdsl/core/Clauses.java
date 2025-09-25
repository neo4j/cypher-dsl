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

import java.net.URI;
import java.util.Collections;
import java.util.List;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.internal.LoadCSV;
import org.neo4j.cypherdsl.core.internal.ProcedureName;
import org.neo4j.cypherdsl.core.internal.YieldItems;
import org.neo4j.cypherdsl.core.utils.Assertions;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * Builder / factory for various {@link Clause clauses}. It's mostly useful for building a
 * Cypher-DSL AST outside the fluent API.
 *
 * @author Michael J. Simons
 * @since 2021.3.0
 */
@API(status = STABLE, since = "2021.3.0")
public final class Clauses {

	/**
	 * Not to be instantiated.
	 */
	private Clauses() {
	}

	/**
	 * Builds a {@code MATCH} clause. The result can be safely cast to a {@link Match} if
	 * needed.
	 * @param optional should this be an optional match?
	 * @param patternElements the pattern elements to match
	 * @param optionalWhere an optional where sub-clause
	 * @param optionalHints optional hints to be used
	 * @return an immutable match clause
	 * @since 2022.0.0
	 */
	public static Clause match(boolean optional, List<PatternElement> patternElements, Where optionalWhere,
			List<Hint> optionalHints) {

		return new Match(optional, Pattern.of(patternElements), optionalWhere, optionalHints);
	}

	/**
	 * Builds a {@code DELETE} clause.
	 * @param detach should this be an detach delete?
	 * @param expressions the expressions pointing to the things to be deleted
	 * @return an immutable delete clause
	 */
	public static Clause delete(boolean detach, List<Expression> expressions) {

		return new Delete(new ExpressionList(expressions), detach);
	}

	/**
	 * Builds a {@code RETURN} clause.
	 * @param distinct should this be a distinct return
	 * @param expressions the expressions to be returned
	 * @param optionalSortItems an optional list of sort items
	 * @param optionalSkip an optional {@link NumberLiteral} of how many items to skip
	 * @param optionalLimit an optional {@link NumberLiteral} of how many items to be
	 * returned
	 * @return an immutable return clause
	 */
	public static Return returning(boolean distinct, List<Expression> expressions, List<SortItem> optionalSortItems,
			Expression optionalSkip, Expression optionalLimit) {

		DefaultStatementBuilder.OrderBuilder orderBuilder = new DefaultStatementBuilder.OrderBuilder();
		orderBuilder.orderBy(optionalSortItems);
		orderBuilder.skip(optionalSkip);
		orderBuilder.limit(optionalLimit);
		return Return.create(false, distinct, expressions, orderBuilder);
	}

	/**
	 * Builds a {@code CREATE} clause.
	 * @param patternElements the pattern elements to create
	 * @return an immutable create clause
	 */
	public static Clause create(List<PatternElement> patternElements) {

		return new Create(Pattern.of(patternElements));
	}

	/**
	 * Builds a {@code MERGE} clause.
	 * @param patternElements the pattern elements to merge
	 * @param mergeActions an optional list of {@link MergeAction merge actions}
	 * @return an immutable merge clause
	 */
	public static Clause merge(List<PatternElement> patternElements, List<MergeAction> mergeActions) {

		return new Merge(Pattern.of(patternElements), (mergeActions != null) ? mergeActions : Collections.emptyList());
	}

	/**
	 * Retrofits an existing {@link Return return clause} into an equivalent {@link With
	 * with clause}, optionally adding a {@link Where where}.
	 * @param returnClause the return clause that defines the fields, order and limit of
	 * what the with clause should return
	 * @param optionalWhere an optional {@literal WHERE }expression to define a where
	 * clause.
	 * @return an immutable with clause
	 * @since 2022.0.0
	 */
	public static Clause with(Return returnClause, Where optionalWhere) {

		return new With(returnClause, optionalWhere);
	}

	/**
	 * Creates a {@link Remove remove clause}, removing labels or properties.
	 * @param expressions expressions pointing to a list of properties or labels that
	 * shall be removed
	 * @return an immutable remove clause
	 */
	public static Clause remove(List<Expression> expressions) {

		return new Remove(new ExpressionList(expressions));
	}

	/**
	 * Creates a {@link Set remove clause}, setting labels or properties.
	 * @param expressions expressions pointing to a list of properties or labels that
	 * shall be set
	 * @return an immutable set clause
	 */
	public static Clause set(List<Expression> expressions) {

		return new Set(new ExpressionList(expressions));
	}

	/**
	 * Creates an {@link Unwind unwind clause}.
	 * @param expression the expression to unwind
	 * @param name the name on which to unwind
	 * @return an immutable unwind clause
	 */
	public static Clause unwind(Expression expression, SymbolicName name) {

		return new Unwind(expression, name.getValue());
	}

	/**
	 * Creates an {@link LoadCSV LOAD CSV clause}.
	 * @param withHeaders set to true to render the WITH HEADERS options
	 * @param uri the source to load from
	 * @param alias the alias for the lines
	 * @param fieldTerminator the field terminator
	 * @return an immutable clause
	 */
	public static Clause loadCSV(boolean withHeaders, StringLiteral uri, SymbolicName alias, String fieldTerminator) {

		return new LoadCSV(URI.create(uri.getContent().toString()), withHeaders, alias.getValue())
			.withFieldTerminator(fieldTerminator);
	}

	/**
	 * Creates a {@literal CALL} clause.
	 * @param namespace an optional namespace, maybe empty
	 * @param name the name of the stored procedure to call
	 * @param arguments the arguments, maybe null or empty
	 * @param resultItems the result items, maybe null or empty
	 * @param optionalWhere an optional where
	 * @return an immutable clause
	 * @since 2022.0.0
	 */
	public static Clause callClause(List<String> namespace, String name, List<Expression> arguments,
			List<Expression> resultItems, Where optionalWhere) {

		return ProcedureCallImpl.create(ProcedureName.from(namespace, name),
				new Arguments((arguments != null) ? arguments.toArray(new Expression[0]) : new Expression[0]),
				(resultItems != null) ? YieldItems.yieldAllOf(resultItems.toArray(new Expression[0])) : null,
				optionalWhere);
	}

	/**
	 * Creates a {@literal CALL {}} sub-query clause. No checking is done whether the
	 * statement passed in returns anything meaningful so that the resulting clause will
	 * be runnable or not.
	 * @param statement a statement to be used inside the sub-query.
	 * @return an immutable sub-query clause.
	 */
	public static Clause callClause(Statement statement) {

		return Subquery.call(statement);
	}

	/**
	 * Creates a literal for each clause.
	 * @param v the name of the variable that should be available in the list of updating
	 * clauses
	 * @param list the list on which to iterate
	 * @param updatingClauses the updating clauses
	 * @return an immutable foreach clause
	 */
	public static Clause forEach(SymbolicName v, Expression list, List<Clause> updatingClauses) {

		Assertions.isTrue(updatingClauses.stream().allMatch(UpdatingClause.class::isInstance),
				"Only updating clauses SET, REMOVE, CREATE, MERGE, DELETE, and FOREACH are allowed as clauses applied inside FOREACH.");
		return new Foreach(v, list, updatingClauses.stream().map(UpdatingClause.class::cast).toList());
	}

	/**
	 * Creates a standalone <code>ORDER BY</code> clause.
	 * @param sortItems the items to sort by
	 * @param skip a literal number item specifying the skipped items, may be
	 * {@literal null}
	 * @param limit a literal number item specifying the total limit of items, may be
	 * {@literal null}
	 * @return an immutable order by clause
	 * @since 2024.7.4
	 */
	public static Clause orderBy(List<SortItem> sortItems, Expression skip, Expression limit) {

		return new OrderByClause(new Order(sortItems), (skip != null) ? Skip.create(skip) : null,
				(limit != null) ? Limit.create(limit) : null);
	}

	static final class OrderByClause extends AbstractClause {

		private final Order order;

		private final Skip skip;

		private final Limit limit;

		OrderByClause(Order order, Skip skip, Limit limit) {
			this.order = order;
			this.skip = skip;
			this.limit = limit;
		}

		@Override
		public void accept(Visitor visitor) {
			this.order.accept(visitor);
			Visitable.visitIfNotNull(this.skip, visitor);
			Visitable.visitIfNotNull(this.limit, visitor);
		}

	}

}
