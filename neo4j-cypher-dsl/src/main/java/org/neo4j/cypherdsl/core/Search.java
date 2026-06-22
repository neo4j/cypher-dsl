/*
 * Copyright (c) 2019-2026 "Neo4j,"
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

import java.util.Objects;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

import static org.apiguardian.api.API.Status.INTERNAL;
import static org.apiguardian.api.API.Status.STABLE;

/**
 * See <a href= "https://neo4j.com/docs/cypher-manual/current/clauses/search/">SEARCH</a>.
 *
 * @author Michael J. Simons
 * @since 2025.3.0
 */
@API(status = STABLE, since = "2025.3.0")
public final class Search implements Visitable, Clause {

	private final SymbolicName name;

	private final VectorIndexClause vectorIndexClause;

	private final ForClause forClause;

	private final TopKClause topKClause;

	private final Where where;

	private final SymbolicName scoreAlias;

	private Search(Builder builder) {
		this.name = builder.name;
		this.vectorIndexClause = new VectorIndexClause(builder.indexName);
		this.forClause = new ForClause(builder.vector);
		this.topKClause = new TopKClause(builder.topK);
		if (builder.where != null) {
			this.where = new Where(builder.where);
		}
		else {
			this.where = null;
		}
		this.scoreAlias = builder.scoreAlias;
	}

	@Override
	public void accept(Visitor visitor) {
		visitor.enter(this);
		this.vectorIndexClause.accept(visitor);
		this.forClause.accept(visitor);
		Visitable.visitIfNotNull(this.where, visitor);
		this.topKClause.accept(visitor);
		visitor.leave(this);
	}

	@Override
	public String toString() {
		return RendererBridge.render(this);
	}

	@API(status = INTERNAL)
	public SymbolicName getName() {
		return this.name;
	}

	@API(status = INTERNAL)
	public SymbolicName getScoreAlias() {
		return this.scoreAlias;
	}

	/**
	 * Step specifying the index name.
	 */
	public interface SpecifyIndexName {

		/**
		 * Specifies the index name.
		 * @param indexName required index name
		 * @return the next step
		 */
		SpecifyVector in(String indexName);

	}

	public interface SpecifyVector {

		/**
		 * Specifies the vector a vector as literal.
		 * @param vector the vector to search with
		 * @return the next step
		 */
		SpecifyWhereOrLimit forVector(Expression vector);

	}

	/**
	 * Final step building the {@link Search} clause.
	 */
	public interface Buildable {

		/**
		 * {@return the ready to use search clause}
		 */
		Search build();

	}

	/**
	 * The optional WHERE subclause used to do vector search with filters is a restricted
	 * version of the normal Cypher WHERE clause and only allows certain types of
	 * predicates. The predicates must only be property predicates.
	 */
	public interface SpecifyWhere {

		/**
		 * Specify one or more property predicates. All predicates will be put together as
		 * conjunction.
		 * @param predicate a required predicate
		 * @param more one or more optional more predicates
		 * @return the next step
		 */
		SpecifyLimit where(Condition predicate, Condition... more);

	}

	/**
	 * Required limit step.
	 */
	public interface SpecifyLimit {

		/**
		 * Specifies the limit.
		 * @param topK the topK, required to be 0 &le; topK &le; 2147483647
		 * @return the buildable search clause
		 */
		SpecifyScoreOrBuild limit(int topK);

	}

	/**
	 * The union step of where and limit.
	 */
	public interface SpecifyWhereOrLimit extends SpecifyLimit, SpecifyWhere {

	}

	/**
	 * The optional SCORE subclause makes the SEARCH clause return the similarity score
	 * for each node or relationship in addition to the node or relationship itself. In
	 * the result, the column for the similarity scores will be called score_alias.
	 */
	public interface SpecifyScoreOrBuild extends Buildable {

		/**
		 * Adds the score clause.
		 * @param alias the alias to use
		 * @return the buildable search clause
		 */
		Buildable score(SymbolicName alias);

	}

	@API(status = INTERNAL)
	public static final class VectorIndexClause implements Visitable {

		private final String name;

		VectorIndexClause(String name) {
			this.name = name;
		}

		@API(status = INTERNAL)
		public String name() {
			return this.name;
		}

	}

	@API(status = INTERNAL)
	public static final class ForClause implements Visitable {

		private final Expression target;

		private ForClause(Expression target) {
			this.target = target;
		}

		@Override
		public void accept(Visitor visitor) {
			visitor.enter(this);
			this.target.accept(visitor);
			visitor.leave(this);
		}

	}

	@API(status = INTERNAL)
	public static final class TopKClause implements Visitable {

		private final int value;

		TopKClause(int value) {
			this.value = value;
		}

		@API(status = INTERNAL)
		public int value() {
			return this.value;
		}

	}

	static final class Builder implements SpecifyIndexName, SpecifyVector, SpecifyWhereOrLimit, SpecifyScoreOrBuild {

		private final SymbolicName name;

		private String indexName;

		private Expression vector;

		private Integer topK;

		private Condition where;

		private SymbolicName scoreAlias;

		Builder(SymbolicName name) {
			this.name = Objects.requireNonNull(name, "Name is required");
		}

		static Condition assertCondition(Condition condition) {
			if (!(Objects.requireNonNull(condition) instanceof Comparison)) {
				throw new IllegalArgumentException("Only comparison conditions are supported");
			}
			return condition;
		}

		@Override
		public SpecifyVector in(String indexName) {
			Assertions.hasText(indexName, "The index name must not be null or empty");
			this.indexName = indexName;
			return this;
		}

		@Override
		public SpecifyWhereOrLimit forVector(Expression vector) {
			Objects.requireNonNull(vector);
			if (!(vector instanceof VectorLiteral || vector instanceof ListLiteral || vector instanceof ListExpression
					|| vector instanceof Parameter<?> || vector instanceof Property)) {
				throw new IllegalArgumentException("Unsupported vector expression %s".formatted(vector.getClass()));
			}
			this.vector = vector;
			return this;
		}

		@Override
		public SpecifyScoreOrBuild limit(int topK) {
			if (topK < 0) {
				throw new IllegalArgumentException("topK must be greater than or equal to 0");
			}
			this.topK = topK;
			return this;
		}

		@Override
		public SpecifyLimit where(Condition predicate, Condition... more) {
			var finalCondition = assertCondition(predicate);

			if (more != null) {
				for (var additional : more) {
					finalCondition = finalCondition.and(assertCondition(additional));
				}
			}

			this.where = finalCondition;
			return this;
		}

		@Override
		public Buildable score(SymbolicName alias) {
			this.scoreAlias = alias;
			return this;
		}

		@Override
		public Search build() {
			return new Search(this);
		}

	}

}
