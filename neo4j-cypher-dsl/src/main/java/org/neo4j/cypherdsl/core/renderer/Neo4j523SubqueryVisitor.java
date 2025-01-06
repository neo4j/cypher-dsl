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

import java.util.Objects;

import org.neo4j.cypherdsl.build.annotations.RegisterForReflection;
import org.neo4j.cypherdsl.core.ReturnBody;
import org.neo4j.cypherdsl.core.Subquery;
import org.neo4j.cypherdsl.core.With;
import org.neo4j.cypherdsl.core.ast.EnterResult;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.VisitorWithResult;

/**
 * Rendering <a href="https://neo4j.com/docs/cypher-manual/5/subqueries/call-subquery/#variable-scope-clause">variable scope call clauses</a>.
 * @author Michael J. Simons
 * @since 2024.1.0
 */
@RegisterForReflection(allDeclaredConstructors = true) final class Neo4j523SubqueryVisitor extends VisitorWithResult {

	private final static GeneralizedRenderer RETURN_BODY_RENDERER = Renderer.getRenderer(
		Configuration.newConfig().withDialect(Dialect.NEO4J_5_23).build(), GeneralizedRenderer.class);

	private final DefaultVisitor delegate;

	Neo4j523SubqueryVisitor(DefaultVisitor delegate) {
		this.delegate = delegate;
	}

	private With importing;

	public EnterResult enterWithResult(Visitable segment) {

		if (segment instanceof Subquery subquery) {
			var replacement = new StringBuilder("(");
			this.importing = subquery.importingWith();
			if (this.importing != null) {
				this.importing.accept(innerSegment -> {
					if (innerSegment instanceof ReturnBody returnBody) {
						replacement.append(RETURN_BODY_RENDERER.render(returnBody));
					}
				});
			}
			replacement.append(") {");
			delegate.enter(subquery);
			delegate.builder
				.replace(delegate.builder.length() - 1, delegate.builder.length(), replacement.toString());
		} else if (segment instanceof With possibleImporting) {
			if (!Objects.equals(this.importing, possibleImporting)) {
				delegate.enter(possibleImporting);
			} else {
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
			} else {
				delegate.leave(possibleImporting);
			}
		}
	}
}
