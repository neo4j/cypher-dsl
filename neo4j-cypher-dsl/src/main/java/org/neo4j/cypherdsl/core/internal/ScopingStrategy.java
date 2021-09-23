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
package org.neo4j.cypherdsl.core.internal;

import static org.apiguardian.api.API.Status.INTERNAL;

import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Collections;
import java.util.Deque;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.Expression;
import org.neo4j.cypherdsl.core.Foreach;
import org.neo4j.cypherdsl.core.IdentifiableElement;
import org.neo4j.cypherdsl.core.Named;
import org.neo4j.cypherdsl.core.Order;
import org.neo4j.cypherdsl.core.PatternComprehension;
import org.neo4j.cypherdsl.core.Property;
import org.neo4j.cypherdsl.core.Return;
import org.neo4j.cypherdsl.core.Statement;
import org.neo4j.cypherdsl.core.Subquery;
import org.neo4j.cypherdsl.core.SymbolicName;
import org.neo4j.cypherdsl.core.With;
import org.neo4j.cypherdsl.core.ast.TypedSubtree;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;

/**
 * A strategy to keep track of {@link Named named variables} inside a scope.
 *
 * @author Michael J. Simons
 * @soundtrack Knorkator - The Schlechtst Of Knorkator
 * @since 2021.3.2
 */
@API(status = INTERNAL, since = "2021.3.2")
public final class ScopingStrategy {

	/**
	 * Keeps track of named objects that have been already visited.
	 */
	private final Deque<Set<IdentifiableElement>> dequeOfVisitedNamed = new ArrayDeque<>(new HashSet<>());

	private Set<IdentifiableElement> afterStatement = Collections.emptySet();

	private Visitable previous;

	private boolean inOrder = false;

	private boolean inProperty = false;

	public ScopingStrategy() {
		this.dequeOfVisitedNamed.push(new HashSet<>());
	}

	public void doEnter(Visitable visitable) {

		// We don't want the identifiable in an order clause to be retained
		if (visitable instanceof Order) {
			this.inOrder = true;
		}

		if (visitable instanceof Property) {
			this.inProperty = true;
		}

		if (hasLocalScope(visitable)) {
			dequeOfVisitedNamed.push(
				new HashSet<>(dequeOfVisitedNamed.isEmpty() ? Collections.emptySet() : dequeOfVisitedNamed.peek()));
		}
	}

	public void doLeave(Visitable visitable) {

		if (!hasScope()) {
			return;
		}

		if (visitable instanceof IdentifiableElement && !inOrder && (!inProperty || visitable instanceof Property)) {

			dequeOfVisitedNamed.peek().add((IdentifiableElement) visitable);
		}

		if (visitable instanceof Statement) {
			Set<IdentifiableElement> lastScope = this.dequeOfVisitedNamed.peek();
			// We keep properties only around when they have been actually returned
			if (!(previous instanceof Return || previous instanceof YieldItems)) {
				this.afterStatement = lastScope.stream().filter(i -> !(i instanceof Property))
					.collect(Collectors.toSet());
			} else {
				this.afterStatement = new HashSet<>(lastScope);
			}
			lastScope.clear();
		} else if (hasLocalScope(visitable)) {
			this.dequeOfVisitedNamed.pop();
		} else {
			clearPreviouslyVisitedNamed(visitable);
		}

		if (visitable instanceof Order) {
			this.inOrder = false;
		}

		if (visitable instanceof Property) {
			this.inProperty = false;
		}

		previous = visitable;
	}

	public boolean hasVisitedBefore(Named namedItem) {

		if (!hasScope()) {
			return false;
		}

		Set<IdentifiableElement> scope = dequeOfVisitedNamed.peek();
		return hasVisitedInScope(scope, namedItem);
	}

	private boolean hasScope() {
		return !this.dequeOfVisitedNamed.isEmpty();
	}

	private boolean hasVisitedInScope(Collection<IdentifiableElement> visited, Named needle) {

		return visited.contains(needle) || needle.getSymbolicName().isPresent() && visited.stream()
			.filter(Named.class::isInstance)
			.map(Named.class::cast)
			.map(Named::getSymbolicName)
			.anyMatch(s -> s.equals(needle.getSymbolicName()));
	}

	private static boolean hasLocalScope(Visitable visitable) {
		return visitable instanceof PatternComprehension ||
			visitable instanceof Subquery ||
			visitable instanceof Foreach;
	}

	private void clearPreviouslyVisitedNamed(Visitable visitable) {

		if (visitable instanceof With) {
			clearPreviouslyVisitedAfterWith((With) visitable);
		} else if (visitable instanceof Return || visitable instanceof YieldItems) {
			clearPreviouslyVisitedAfterReturnish(visitable);
		}
	}

	private void clearPreviouslyVisitedAfterWith(With with) {

		// We need to clear the named cache after defining a with.
		// Everything not taken into the next step has to go.
		Set<IdentifiableElement> retain = new HashSet<>();
		Set<IdentifiableElement> visitedNamed = dequeOfVisitedNamed.peek();
		with.accept(segment -> {
			if (segment instanceof SymbolicName) {
				visitedNamed.stream()
					.filter(element -> {
						if (element instanceof Named) {
							return ((Named) element).getRequiredSymbolicName().equals(segment);
						}
						return false;
					})
					.forEach(retain::add);
			}
		});

		if (visitedNamed != null) {
			visitedNamed.retainAll(retain);
		}
	}

	private void clearPreviouslyVisitedAfterReturnish(Visitable returnish) {

		// Everything not returned as to go.
		Set<IdentifiableElement> retain = new HashSet<>();
		Set<IdentifiableElement> visitedNamed = dequeOfVisitedNamed.peek();
		returnish.accept(new Visitor() {

			boolean in = false;

			int level = 0;

			@Override
			public void enter(Visitable segment) {
				if (!in && segment instanceof TypedSubtree) {
					in = true;
					return;
				}

				if (in) {
					++level;
				}

				// Only collect things exactly one level into the list of returned items
				if (level == 1 && segment instanceof IdentifiableElement) {
					retain.add((IdentifiableElement) segment);
				}
			}

			@Override
			public void leave(Visitable segment) {

				if (in) {
					--level;
					if (segment instanceof TypedSubtree) {
						in = false;
					}
				}
			}
		});

		if (visitedNamed != null) {
			visitedNamed.retainAll(retain);
		}
	}

	public Collection<Expression> getIdentifiables() {

		if (!hasScope()) {
			return Collections.emptySet();
		}

		Predicate<IdentifiableElement> allNamedElementsHaveResolvedNames = e ->
			!(e instanceof Named) || ((Named) e).getSymbolicName().isPresent();

		Set<IdentifiableElement> items = Optional.ofNullable(this.dequeOfVisitedNamed.peek())
			.filter(scope -> !scope.isEmpty())
			.orElse(afterStatement);
		return items
			.stream()
			.filter(allNamedElementsHaveResolvedNames)
			.map(IdentifiableElement::asExpression)
			.collect(Collectors.collectingAndThen(Collectors.toSet(), Collections::unmodifiableSet));
	}
}
