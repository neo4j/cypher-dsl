/*
 * Copyright (c) 2019-2021 "Neo4j,"
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

import static org.apiguardian.api.API.Status.EXPERIMENTAL;

import java.util.ArrayList;
import java.util.List;

import org.apiguardian.api.API;
import org.jetbrains.annotations.NotNull;
import org.neo4j.cypherdsl.core.utils.CheckReturnValue;
import org.neo4j.cypherdsl.core.internal.CaseElse;
import org.neo4j.cypherdsl.core.internal.CaseWhenThen;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;

/**
 * See <a href="https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/CaseExpression.html">CaseExpression</a>.
 *
 * @author Gerrit Meier
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = EXPERIMENTAL, since = "1.0")
public abstract class Case implements Visitable {

	private CaseElse caseElse;
	private final List<CaseWhenThen> caseWhenThens;

	static Case create(Expression caseExpression) {
		return new SimpleCase(caseExpression, new ArrayList<>());
	}

	static Case create() {
		return new GenericCase(new ArrayList<>());
	}

	Case(List<CaseWhenThen> caseWhenThens) {
		this.caseWhenThens = caseWhenThens;
	}

	abstract Expression getCaseExpression();

	void setCaseElse(CaseElse caseElse) {
		this.caseElse = caseElse;
	}

	/**
	 * Creates a new case/when expression with an additional {@code WHEN} block.
	 *
	 * @param nextExpression The next expression to use.
	 * @return An ongoing when builder.
	 */
	@NotNull @CheckReturnValue
	public OngoingWhenThen when(Expression nextExpression) {
		return new OngoingWhenThen(nextExpression);
	}

	/**
	 * Specification for a renderable, complete CASE statement
	 */
	@API(status = EXPERIMENTAL, since = "1.0")
	public interface CaseEnding extends Condition {

		/**
		 * Adds a new {@code WHEN} block.
		 *
		 * @param expression A new when expression.
		 * @return An ongoing when builder.
		 */
		@NotNull @CheckReturnValue
		OngoingWhenThen when(Expression expression);

		/**
		 * Ends this case expression with a default expression to evaluate.
		 *
		 * @param defaultExpression The new default expression
		 * @return An ongoing when builder.
		 */
		@NotNull @CheckReturnValue
		CaseEnding elseDefault(Expression defaultExpression);
	}

	/**
	 * Special implementation of the {@link Case} class to support simple case with an initial expression / condition.
	 */
	@API(status = EXPERIMENTAL, since = "1.0")
	public static class SimpleCase extends Case {

		private final Expression caseExpression;

		private SimpleCase(Expression caseExpression, List<CaseWhenThen> caseWhenThens) {
			super(caseWhenThens);
			this.caseExpression = caseExpression;
		}

		@Override
		Expression getCaseExpression() {
			return caseExpression;
		}

		/**
		 * The renderable implementation of {@link SimpleCase}.
		 */
		final static class EndingSimpleCase extends SimpleCase implements CaseEnding {

			private EndingSimpleCase(Expression caseExpression, List<CaseWhenThen> caseWhenThens) {
				super(caseExpression, caseWhenThens);
			}

			@Override
			public CaseEnding elseDefault(Expression defaultExpression) {
				this.setCaseElse(new CaseElse(defaultExpression));
				return this;
			}
		}
	}

	/**
	 * Implementation of the {@link Case} class to support generic case.
	 */
	@API(status = EXPERIMENTAL, since = "1.0")
	public static class GenericCase extends Case {

		private GenericCase(List<CaseWhenThen> caseWhenThens) {
			super(caseWhenThens);
		}

		@Override
		Expression getCaseExpression() {
			return null;
		}

		/**
		 * The renderable implementation of {@link GenericCase}.
		 */
		public final static class EndingGenericCase extends GenericCase implements CaseEnding {

			private EndingGenericCase(List<CaseWhenThen> caseWhenThens) {
				super(caseWhenThens);
			}

			@Override
			public CaseEnding elseDefault(Expression defaultExpression) {
				this.setCaseElse(new CaseElse(defaultExpression));
				return this;
			}
		}
	}

	@Override
	public void accept(Visitor visitor) {
		visitor.enter(this);
		if (getCaseExpression() != null) {
			getCaseExpression().accept(visitor);
		}

		caseWhenThens.forEach(caseWhenThen -> caseWhenThen.accept(visitor));

		if (caseElse != null) {
			caseElse.accept(visitor);
		}

		visitor.leave(this);
	}

	/**
	 * Helper class to collect `when` expressions and create {@link CaseWhenThen} instances when the `then` is provided.
	 */
	@API(status = EXPERIMENTAL, since = "1.0")
	public final class OngoingWhenThen {

		final Expression whenExpression;

		private OngoingWhenThen(Expression whenExpression) {
			this.whenExpression = whenExpression;
		}

		/**
		 * Ends this {@code WHEN} block with an expression.
		 *
		 * @param expression The expression for the ongoing {@code WHEN} block.
		 * @return An ongoing when builder.
		 */
		@NotNull @CheckReturnValue
		public CaseEnding then(Expression expression) {

			CaseWhenThen caseWhenThen = new CaseWhenThen(whenExpression, expression);
			caseWhenThens.add(caseWhenThen);
			if (getCaseExpression() != null) {
				return new SimpleCase.EndingSimpleCase(Case.this.getCaseExpression(), caseWhenThens);
			} else {
				return new GenericCase.EndingGenericCase(caseWhenThens);
			}
		}
	}

}
