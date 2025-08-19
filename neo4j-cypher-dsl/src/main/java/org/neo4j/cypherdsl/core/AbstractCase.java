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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.neo4j.cypherdsl.core.annotations.CheckReturnValue;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.internal.CaseElse;
import org.neo4j.cypherdsl.core.internal.CaseWhenThen;

/**
 * Abstract base class for a {@link Case}.
 *
 * @author Gerrit Meier
 * @author Michael J. Simons
 * @since 2021.2.3
 */
abstract class AbstractCase implements Case {

	private final List<CaseWhenThen> caseWhenThens;

	private CaseElse caseElse;

	private Optional<String> prefix = Optional.empty();

	private Optional<String> suffix = Optional.empty();

	AbstractCase(List<CaseWhenThen> caseWhenThens) {
		this.caseWhenThens = new ArrayList<>(caseWhenThens);
	}

	abstract Expression getCaseExpression();

	@Override
	public String toString() {
		return RendererBridge.render(this);
	}

	void setCaseElse(CaseElse caseElse) {
		this.caseElse = caseElse;
	}

	/**
	 * Creates a new case/when expression with an additional {@code WHEN} block.
	 * @param nextExpression the next expression to use.
	 * @return an ongoing when builder.
	 */
	@Override
	@CheckReturnValue
	public OngoingWhenThen when(Expression nextExpression) {

		return new DefaultOngoingWhenThen(nextExpression);
	}

	@Override
	public Optional<String> getPrefix() {
		return this.prefix;
	}

	@Override
	public Optional<String> getSuffix() {
		return this.suffix;
	}

	@Override
	public Property property(String... names) {

		this.prefix = Optional.of("(");
		this.suffix = Optional.of(")");
		return Case.super.property(names);
	}

	@Override
	public void accept(Visitor visitor) {
		visitor.enter(this);
		if (getCaseExpression() != null) {
			getCaseExpression().accept(visitor);
		}

		this.caseWhenThens.forEach(caseWhenThen -> caseWhenThen.accept(visitor));

		if (this.caseElse != null) {
			this.caseElse.accept(visitor);
		}

		visitor.leave(this);
	}

	static class SimpleCaseImpl extends AbstractCase implements SimpleCase {

		private final Expression caseExpression;

		SimpleCaseImpl(Expression caseExpression) {
			this(caseExpression, Collections.emptyList());
		}

		SimpleCaseImpl(Expression caseExpression, List<CaseWhenThen> caseWhenThens) {
			super(caseWhenThens);
			this.caseExpression = caseExpression;
		}

		@Override
		Expression getCaseExpression() {
			return this.caseExpression;
		}

		/**
		 * The renderable implementation of {@link SimpleCase}.
		 */
		static final class EndingSimpleCase extends SimpleCaseImpl implements CaseEnding {

			private EndingSimpleCase(Expression caseExpression, List<CaseWhenThen> caseWhenThens) {
				super(caseExpression, caseWhenThens);
			}

			@Override
			public Case elseDefault(Expression defaultExpression) {
				this.setCaseElse(new CaseElse(defaultExpression));
				return this;
			}

		}

	}

	static class GenericCaseImpl extends AbstractCase implements GenericCase {

		GenericCaseImpl() {
			this(Collections.emptyList());
		}

		GenericCaseImpl(List<CaseWhenThen> caseWhenThens) {
			super(caseWhenThens);
		}

		@Override
		Expression getCaseExpression() {
			return null;
		}

		/**
		 * The renderable implementation of {@link GenericCase}.
		 */
		static final class EndingGenericCase extends GenericCaseImpl implements CaseEnding {

			private EndingGenericCase(List<CaseWhenThen> caseWhenThens) {
				super(caseWhenThens);
			}

			@Override
			public Case elseDefault(Expression defaultExpression) {
				this.setCaseElse(new CaseElse(defaultExpression));
				return this;
			}

		}

	}

	private final class DefaultOngoingWhenThen implements OngoingWhenThen {

		final Expression whenExpression;

		private DefaultOngoingWhenThen(Expression whenExpression) {
			this.whenExpression = whenExpression;
		}

		/**
		 * Ends this {@code WHEN} block with an expression.
		 * @param expression the expression for the ongoing {@code WHEN} block.
		 * @return an ongoing when builder.
		 */
		@Override
		@CheckReturnValue
		public CaseEnding then(Expression expression) {

			CaseWhenThen caseWhenThen = new CaseWhenThen(this.whenExpression, expression);
			AbstractCase.this.caseWhenThens.add(caseWhenThen);
			if (getCaseExpression() != null) {
				return new SimpleCaseImpl.EndingSimpleCase(AbstractCase.this.getCaseExpression(),
						AbstractCase.this.caseWhenThens);
			}
			else {
				return new GenericCaseImpl.EndingGenericCase(AbstractCase.this.caseWhenThens);
			}
		}

	}

}
