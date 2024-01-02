/*
 * Copyright (c) 2019-2024 "Neo4j,"
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

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import org.neo4j.cypherdsl.build.annotations.RegisterForReflection;
import org.neo4j.cypherdsl.core.Comparison;
import org.neo4j.cypherdsl.core.Operator;
import org.neo4j.cypherdsl.core.Property;
import org.neo4j.cypherdsl.core.ast.EnterResult;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.VisitorWithResult;
import org.neo4j.cypherdsl.core.renderer.Neo4j5FunctionInvocationVisitor.SingleArgExtractor;

/**
 * Takes care of
 * <ul>
 *     <li>Inverting <code>NOT EXISTS (n.prop)</code> into <code>n.prop IS NULL</code></li>
 * </ul>
 *
 * @author Michael J. Simons
 */
@RegisterForReflection(allDeclaredConstructors = true)
final class Neo4j5ComparisonVisitor extends VisitorWithResult {

	private final DefaultVisitor delegate;

	Neo4j5ComparisonVisitor(DefaultVisitor delegate) {
		this.delegate = delegate;
	}

	@Override
	public EnterResult enterWithResult(Visitable segment) {

		Comparison comparison = (Comparison) segment;
		AtomicReference<Operator> capture = new AtomicReference<>();
		AtomicInteger level = new AtomicInteger(0);
		AtomicReference<Visitable> nPropExists = new AtomicReference<>();
		comparison.accept(new VisitorWithResult() {
			@Override
			public EnterResult enterWithResult(Visitable visitable) {
				boolean isOneLevelBelow = level.getAndIncrement() == 1;
				if (isOneLevelBelow) {
					if (visitable instanceof Operator operator) {
						capture.compareAndSet(null, operator);
					} else if (Neo4j5FunctionInvocationVisitor.isNPropExists(visitable)) {
						nPropExists.compareAndSet(null, visitable);
					}
				}
				return EnterResult.CONTINUE;
			}

			@Override
			public void leave(Visitable segment) {
				level.decrementAndGet();
			}
		});

		if (capture.get() == Operator.NOT && nPropExists.get() != null) {
			SingleArgExtractor<Property> singleArgExtractor = new SingleArgExtractor<>(Property.class);
			nPropExists.get().accept(singleArgExtractor);
			singleArgExtractor.singleArg.accept(delegate);
			delegate.builder.append(" IS NULL");
			return EnterResult.SKIP_CHILDREN;
		}

		return EnterResult.CONTINUE;
	}
}
