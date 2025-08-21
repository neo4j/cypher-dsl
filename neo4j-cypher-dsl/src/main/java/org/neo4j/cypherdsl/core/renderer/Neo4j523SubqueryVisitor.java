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

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Objects;

import org.neo4j.cypherdsl.build.annotations.RegisterForReflection;
import org.neo4j.cypherdsl.core.ReturnBody;
import org.neo4j.cypherdsl.core.Subquery;
import org.neo4j.cypherdsl.core.With;
import org.neo4j.cypherdsl.core.ast.EnterResult;
import org.neo4j.cypherdsl.core.ast.TypedSubtree;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.ast.VisitorWithResult;

/**
 * Rendering <a href="https://neo4j.com/docs/cypher-manual/5/subqueries/call-subquery/#variable-scope-clause">variable scope call clauses</a>.
 * @author Michael J. Simons
 * @since 2024.1.0
 */
@RegisterForReflection(allDeclaredConstructors = true)
final class Neo4j523SubqueryVisitor extends VisitorWithResult {

	private final DefaultVisitor delegate;

	private With importing;

	Neo4j523SubqueryVisitor(DefaultVisitor delegate) {
		this.delegate = delegate;
	}

	@Override
	public EnterResult enterWithResult(Visitable segment) {

		if (segment instanceof Subquery subquery) {
			this.importing = subquery.importingWith();
			this.delegate.enter(subquery);
			this.delegate.builder.replace(this.delegate.builder.length() - 1, this.delegate.builder.length(), "(");
			if (this.importing != null) {
				this.importing.accept(innerSegment -> {
					if (innerSegment instanceof ReturnBody returnBody) {
						returnBody.accept(this.delegate);
					}
				});
			} else if (this.delegate.hasIdentifiables()) {
				this.delegate.builder.append("*");
			}
			this.delegate.builder.append(") {");
		} else if (segment instanceof With possibleImporting) {
			if (!Objects.equals(this.importing, possibleImporting)) {
				this.delegate.enter(possibleImporting);
			} else {
				possibleImporting.accept(new Visitor() {
					private final Deque<Boolean> entered = new ArrayDeque<>();

					@Override
					public void enter(Visitable segment) {
						this.entered.push(Neo4j523SubqueryVisitor.this.delegate.preEnter(segment));
					}

					@Override
					public void leave(Visitable segment) {
						if (this.entered.pop()) {
							Neo4j523SubqueryVisitor.this.delegate.postLeave(segment);
							var currentLength = Neo4j523SubqueryVisitor.this.delegate.builder.length();
							if (segment instanceof TypedSubtree<?> t && Neo4j523SubqueryVisitor.this.delegate.builder
								.subSequence(currentLength - t.separator().length(), currentLength)
								.equals(t.separator())) {
								Neo4j523SubqueryVisitor.this.delegate.builder
									.setLength(currentLength - t.separator().length());
							}
						}
					}
				});
				return EnterResult.SKIP_CHILDREN;
			}
		}

		return EnterResult.CONTINUE;
	}

	@Override
	public void leave(Visitable segment) {

		if (segment instanceof Subquery subquery) {
			delegate.leave(subquery);
		} else if (segment instanceof With possibleImporting) {
			if (Objects.equals(this.importing, possibleImporting)) {
				this.importing = null;
			}  else {
				this.delegate.leave(possibleImporting);
			}
		}
	}

}
