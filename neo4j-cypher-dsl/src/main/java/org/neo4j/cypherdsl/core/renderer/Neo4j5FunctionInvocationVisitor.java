/*
 * Copyright (c) 2019-2022 "Neo4j,"
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

import java.util.concurrent.atomic.AtomicReference;

import org.neo4j.cypherdsl.build.annotations.RegisterForReflection;
import org.neo4j.cypherdsl.core.FunctionInvocation;
import org.neo4j.cypherdsl.core.NestedExpression;
import org.neo4j.cypherdsl.core.Property;
import org.neo4j.cypherdsl.core.ast.EnterResult;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.ast.VisitorWithResult;

/**
 * Some logic to rewrite a couple of functions for Neo4j 5, among them:
 * <ul>
 *     <li><pre>exists(n.prop)</pre></li>
 *     <li><pre>distance(p)</pre></li>
 * </ul>
 *
 * @author Michael J. Simons
 * @soundtrack Red Hot Chili Peppers - Unlimited Love
 * @since 2022.3.0
 */
@RegisterForReflection(allDeclaredConstructors = true) final class Neo4j5FunctionInvocationVisitor implements Visitor {

	private final DefaultVisitor delegate;

	Neo4j5FunctionInvocationVisitor(DefaultVisitor delegate) {
		this.delegate = delegate;
	}

	@Override
	public void enter(Visitable visitable) {

		FunctionInvocation functionInvocation = (FunctionInvocation) visitable;
		if ("distance".equals(functionInvocation.getFunctionName())) {
			delegate.builder.append("point.distance(");
		} else if ("elementId".equals(functionInvocation.getFunctionName())) {
			delegate.builder.append("elementId(");
		} else if (!isNPropExists(visitable)) {
			delegate.enter(functionInvocation);
		}
	}

	@Override
	public void leave(Visitable visitable) {

		FunctionInvocation functionInvocation = (FunctionInvocation) visitable;
		if (isNPropExists(visitable)) {
			delegate.builder.append(" IS NOT NULL");
		} else if ("elementId".equals(functionInvocation.getFunctionName())) {
			delegate.builder.append(")");
		} else {
			delegate.leave(functionInvocation);
		}
	}

	static boolean isNPropExists(Visitable visitable) {

		if (visitable instanceof NestedExpression) {
			AtomicReference<Visitable> capture = new AtomicReference<>();
			visitable.accept(new VisitorWithResult() {
				@Override
				public EnterResult enterWithResult(Visitable segment) {
					if (segment instanceof NestedExpression) { // The same object
						return EnterResult.CONTINUE;
					}
					return capture.compareAndSet(null, segment) ? EnterResult.SKIP_CHILDREN : EnterResult.CONTINUE;
				}
			});
			visitable = capture.get();
		}

		if (!(visitable instanceof FunctionInvocation functionInvocation)) {
			return false;
		}

		if ("exists".equals(functionInvocation.getFunctionName())) {
			SingleArgExtractor<Property> singleArgExtractor = new SingleArgExtractor<>(Property.class);
			functionInvocation.accept(singleArgExtractor);
			return singleArgExtractor.singleArg != null;
		}
		return false;
	}

	static class SingleArgExtractor<T> extends VisitorWithResult {

		final Class<T> expectedType;

		boolean insideArguments;
		int level = 0;
		T singleArg;

		SingleArgExtractor(Class<T> expectedType) {
			this.expectedType = expectedType;
		}

		@SuppressWarnings("unchecked")
		@Override
		public EnterResult enterWithResult(Visitable segment) {
			if (segment instanceof NestedExpression) {
				return EnterResult.CONTINUE;
			}

			if (++level == 2) {
				insideArguments = true;
			}

			if (insideArguments && level == 3 && expectedType.isInstance(segment)) {
				singleArg = singleArg == null ? (T) segment : null;
			}
			return EnterResult.CONTINUE;
		}

		@Override
		public void leave(Visitable segment) {
			insideArguments = level-- != 2;
		}
	}
}
