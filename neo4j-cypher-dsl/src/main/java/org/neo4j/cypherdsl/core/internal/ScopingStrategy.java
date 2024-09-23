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
package org.neo4j.cypherdsl.core.internal;

import static org.apiguardian.api.API.Status.INTERNAL;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Deque;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.apiguardian.api.API;
import org.jetbrains.annotations.NotNull;
import org.neo4j.cypherdsl.core.Aliased;
import org.neo4j.cypherdsl.core.AliasedExpression;
import org.neo4j.cypherdsl.core.Asterisk;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.Expression;
import org.neo4j.cypherdsl.core.Foreach;
import org.neo4j.cypherdsl.core.FunctionInvocation;
import org.neo4j.cypherdsl.core.IdentifiableElement;
import org.neo4j.cypherdsl.core.MapProjection;
import org.neo4j.cypherdsl.core.Named;
import org.neo4j.cypherdsl.core.Order;
import org.neo4j.cypherdsl.core.PatternComprehension;
import org.neo4j.cypherdsl.core.PatternElement;
import org.neo4j.cypherdsl.core.ProcedureCall;
import org.neo4j.cypherdsl.core.Property;
import org.neo4j.cypherdsl.core.Return;
import org.neo4j.cypherdsl.core.Statement;
import org.neo4j.cypherdsl.core.Subquery;
import org.neo4j.cypherdsl.core.SubqueryExpression;
import org.neo4j.cypherdsl.core.SymbolicName;
import org.neo4j.cypherdsl.core.UnionPart;
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
	 * @return an empty scoping strategy, for internal use only.
	 */
	public static ScopingStrategy create() {
		return new ScopingStrategy();
	}

	/**
	 * @param onScopeEntered Event handlers to be called after entering local or implicit scope
	 * @param onScopeLeft    Event handlers to b called after leaving local or implicit scope
	 * @return an empty scoping strategy, for internal use only.
	 */
	public static ScopingStrategy create(List<BiConsumer<Visitable, Collection<IdentifiableElement>>> onScopeEntered, List<BiConsumer<Visitable, Collection<IdentifiableElement>>> onScopeLeft) {
		var strategy = create();
		strategy.onScopeEntered.addAll(onScopeEntered);
		strategy.onScopeLeft.addAll(onScopeLeft);
		return strategy;
	}

	/**
	 * Keeps track of named objects that have been already visited.
	 */
	private final Deque<Set<IdentifiableElement>> dequeOfVisitedNamed = new ArrayDeque<>();

	/**
	 * Some expressions have implicit scopes, we might not clear that after returning from inner statements or returns.
	 */
	private final Deque<Set<IdentifiableElement>> implicitScope = new ArrayDeque<>();

	private Set<IdentifiableElement> afterStatement = Collections.emptySet();

	private Visitable previous;

	private boolean inOrder = false;

	private boolean inProperty = false;

	private boolean inSubquery = false;

	private boolean inListFunctionPredicate = false;

	private final Deque<Set<String>> currentImports = new ArrayDeque<>();
	private final Deque<Set<String>> definedInSubquery = new ArrayDeque<>();

	private final List<BiConsumer<Visitable, Collection<IdentifiableElement>>> onScopeEntered = new ArrayList<>();
	private final List<BiConsumer<Visitable, Collection<IdentifiableElement>>> onScopeLeft = new ArrayList<>();

	/**
	 * A flag if we can skip aliasing. This is currently the case in exactly one scenario: A aliased expression passed
	 * to a map project. In that case, the alias is already defined by the key to use in the projected map, and we
	 * cannot define him in `AS xxx` fragment.
	 */
	private final Deque<Boolean> skipAliasing = new ArrayDeque<>();

	private ScopingStrategy() {
		this.dequeOfVisitedNamed.push(new LinkedHashSet<>());
	}

	/**
	 * Called when visiting a {@link Visitable}
	 *
	 * @param visitable Element to be checked for new scope
	 */
	public void doEnter(Visitable visitable) {

		// We don't want the identifiable in an order clause to be retained
		if (visitable instanceof Order) {
			this.inOrder = true;
		}

		if (visitable instanceof Property) {
			this.inProperty = true;
		}

		if (visitable instanceof Subquery subquery) {
			var with = subquery.importingWith();

			this.inSubquery = true;
			this.definedInSubquery.push(new LinkedHashSet<>());

			Set<String> imports = new LinkedHashSet<>();
			this.currentImports.push(imports);
			if (with != null) {
				with.getItems().stream()
					.filter(IdentifiableElement.class::isInstance)
					.map(IdentifiableElement.class::cast)
					.map(ScopingStrategy::extractIdentifier)
					.filter(Objects::nonNull)
					.forEach(imports::add);
			}
		}

		if (isListFunctionPredicate(visitable)) {
			this.inListFunctionPredicate = true;
		}

		if (visitable instanceof MapProjection) {
			this.skipAliasing.push(true);
		} else if (visitable instanceof SubqueryExpression) {
			this.skipAliasing.push(false);
		}

		boolean notify = false;
		Set<IdentifiableElement> scopeSeed = dequeOfVisitedNamed.isEmpty() ? Collections.emptySet() : dequeOfVisitedNamed.peek();
		if (hasLocalScope(visitable)) {
			notify = true;
			dequeOfVisitedNamed.push(new LinkedHashSet<>(scopeSeed));
		}

		if (hasImplicitScope(visitable)) {
			notify = true;
			implicitScope.push(new LinkedHashSet<>(scopeSeed));
		}

		if (notify) {
			scopeSeed.addAll(afterStatement);
			var importsAndScope = new LinkedHashSet<IdentifiableElement>();
			var current = this.currentImports.peek();
			if (current != null) {
				current.stream().map(Cypher::name).forEach(importsAndScope::add);
			}
			importsAndScope.addAll(scopeSeed);
			this.onScopeEntered.forEach(c -> c.accept(visitable, importsAndScope));
		}
	}

	public boolean isSkipAliasing() {
		return Optional.ofNullable(this.skipAliasing.peek()).orElse(false);
	}

	/**
	 * @param namedItem An item that might have been visited in the current scope
	 * @return {@literal true} if the named item has been visited in the current scope before
	 */
	public boolean hasVisitedBefore(Named namedItem) {

		if (!hasScope()) {
			return false;
		}

		Set<IdentifiableElement> scope = dequeOfVisitedNamed.peek();
		return hasVisitedInScope(scope, namedItem);
	}

	private boolean isListFunctionPredicate(Visitable visitable) {

		return visitable instanceof FunctionInvocation fi && Set.of("all", "any", "none", "single")
			.contains(fi.getFunctionName().toLowerCase(
				Locale.ROOT));
	}

	/**
	 * Called when leaving a {@link Visitable}
	 *
	 * @param visitable Element to be checked for a scope to be closed
	 */
	public void doLeave(Visitable visitable) {

		if (!hasScope()) {
			return;
		}

		if (visitable instanceof IdentifiableElement identifiableElement && !inOrder && (!inProperty || visitable instanceof Property)) {
			if (identifiableElement instanceof SymbolicName && inListFunctionPredicate) {
				this.inListFunctionPredicate = false;
			} else {
				dequeOfVisitedNamed.peek().add(identifiableElement);
				if (inSubquery) {
					var identifier = extractIdentifier(identifiableElement);
					if (identifier != null) {
						this.definedInSubquery.peek().add(identifier);
					}
				}
			}
		}

		if (isListFunctionPredicate(visitable)) {
			this.inListFunctionPredicate = false;
		}

		boolean notify = false;
		if (visitable instanceof Statement) {
			leaveStatement(visitable);
		} else if (hasLocalScope(visitable)) {
			notify = true;
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
		if (visitable instanceof Subquery) {
			this.inSubquery = false;
			this.currentImports.pop();
			this.definedInSubquery.pop();
		}

		if (visitable instanceof MapProjection || visitable instanceof SubqueryExpression) {
			this.skipAliasing.pop();
		}

		if (hasImplicitScope(visitable)) {
			notify = true;
			this.implicitScope.pop();
		}

		previous = visitable;

		if (notify) {
			Set<IdentifiableElement> retainedElements = new HashSet<>(afterStatement);
			this.onScopeLeft.forEach(c -> c.accept(visitable, retainedElements));
		}
	}

	/**
	 * Anything that might import variables from the outside, without using an explicit {@code WITH} clause.
	 *
	 * @param visitable the element to be checked whether it implicitly imports named elements.
	 * @return {@literal true} if named elements are imported
	 */
	private static boolean hasImplicitScope(Visitable visitable) {
		return visitable instanceof SubqueryExpression || visitable instanceof Statement.UnionQuery;
	}

	private void leaveStatement(Visitable visitable) {

		Set<IdentifiableElement> lastScope = this.dequeOfVisitedNamed.peek();
		// We keep properties only around when they have been actually returned
		if (previous instanceof UnionPart && afterStatement != null) {
			lastScope.stream().filter(i -> !(i instanceof Property))
				.forEach(afterStatement::add);
		} else if (!(previous instanceof Return || previous instanceof YieldItems)) {
			this.afterStatement = lastScope.stream().filter(i -> !(i instanceof Property))
				.collect(Collectors.toCollection(LinkedHashSet::new));
		} else {
			this.afterStatement = new LinkedHashSet<>(lastScope);
		}

		// A procedure call doesn't change scope.
		if (visitable instanceof ProcedureCall) {
			return;
		}

		lastScope.retainAll(Optional.ofNullable(this.implicitScope.peek()).orElseGet(Set::of));
	}

	private boolean hasScope() {
		return !this.dequeOfVisitedNamed.isEmpty();
	}

	private boolean hasVisitedInScope(Collection<IdentifiableElement> visited, Named needle) {

		return visited.contains(needle) || needle.getSymbolicName().isPresent() && visited.stream()
			.filter(byHasAName())
			.map(ScopingStrategy::extractIdentifier)
			.filter(Objects::nonNull)
			.anyMatch(identifiedBy(needle));
	}

	@NotNull
	private static Predicate<IdentifiableElement> byHasAName() {
		Predicate<IdentifiableElement> hasAName = Named.class::isInstance;
		hasAName = hasAName.or(AliasedExpression.class::isInstance);
		hasAName = hasAName.or(SymbolicName.class::isInstance);
		return hasAName;
	}

	/**
	 * Extracts an identifier from an {@link IdentifiableElement identifiable element}.
	 * @param i The identifiable element
	 * @return The identifier
	 */
	private static String extractIdentifier(IdentifiableElement i) {
		String value;
		if (i instanceof Named named) {
			value = named.getSymbolicName().map(SymbolicName::getValue).orElse(null);
		} else if (i instanceof Aliased aliased) {
			value = aliased.getAlias();
		} else if (i instanceof SymbolicName symbolicName) {
			value = symbolicName.getValue();
		} else {
			value = null;
		}
		return value;
	}

	@NotNull
	private Predicate<String> identifiedBy(Named needle) {
		return i -> {
			boolean result = i.equals(needle.getRequiredSymbolicName().getValue());
			if (result && inSubquery) {
				var imported = Optional.ofNullable(currentImports.peek()).orElseGet(Set::of).contains(i);
				return imported || this.definedInSubquery.peek().contains(i);
			}
			return result;
		};
	}

	private static boolean hasLocalScope(Visitable visitable) {
		return visitable instanceof PatternComprehension ||
			visitable instanceof Subquery ||
			visitable instanceof SubqueryExpression ||
			visitable instanceof Foreach;
	}

	private void clearPreviouslyVisitedNamed(Visitable visitable) {

		if (visitable instanceof With with) {
			clearPreviouslyVisitedAfterWith(with);
		} else if (visitable instanceof Return || visitable instanceof YieldItems) {
			clearPreviouslyVisitedAfterReturnish(visitable);
		}
	}

	private void clearPreviouslyVisitedAfterWith(With with) {

		// We need to clear the named cache after defining a with.
		// Everything not taken into the next step has to go.
		Set<IdentifiableElement> retain = new HashSet<>();
		Set<IdentifiableElement> visitedNamed = dequeOfVisitedNamed.peek();
		if (visitedNamed == null) {
			return;
		}
		with.accept(segment -> {
			if (segment instanceof SymbolicName symbolicName) {
				visitedNamed.stream()
					.filter(element -> {
						if (element instanceof Named named) {
							return named.getRequiredSymbolicName().equals(segment);
						} else if (element instanceof Aliased aliased) {
							return aliased.getAlias().equals((symbolicName).getValue());
						} else {
							return element.equals(segment);
						}
					})
					.forEach(retain::add);
			} else if (segment instanceof Asterisk) {
				retain.addAll(visitedNamed);
			}
		});

		retain.addAll(Optional.ofNullable(implicitScope.peek()).orElseGet(Set::of));
		visitedNamed.retainAll(retain);
	}

	private void clearPreviouslyVisitedAfterReturnish(Visitable returnish) {

		// Everything not returned has to go.
		Set<IdentifiableElement> retain = new HashSet<>();
		Set<IdentifiableElement> visitedNamed = dequeOfVisitedNamed.peek();
		returnish.accept(new Visitor() {

			int level = 0;

			Visitable entranceLevel1;

			@Override
			public void enter(Visitable segment) {
				if (entranceLevel1 == null && segment instanceof TypedSubtree) {
					entranceLevel1 = segment;
					return;
				}

				if (entranceLevel1 != null) {
					++level;
				}

				// Only collect things exactly one level into the list of returned items
				if (level == 1 && segment instanceof IdentifiableElement identifiableElement) {
					retain.add(identifiableElement);
				}
			}

			@Override
			public void leave(Visitable segment) {

				if (entranceLevel1 != null) {
					level = Math.max(0, level - 1);
					if (segment == entranceLevel1) {
						entranceLevel1 = null;
					}
				}
			}
		});

		retain.addAll(Optional.ofNullable(implicitScope.peek()).orElseGet(Set::of));
		if (visitedNamed != null) {
			visitedNamed.retainAll(retain);
		}
	}

	/**
	 * @return An unmodifiable collections with identifiables in the current scope
	 */
	public Collection<Expression> getIdentifiables() {

		if (!hasScope()) {
			return Collections.emptySet();
		}

		Predicate<IdentifiableElement> allNamedElementsHaveResolvedNames = e ->
			!(e instanceof Named named) || named.getSymbolicName().filter(s -> s.getValue() != null).isPresent();

		Set<IdentifiableElement> items = Optional.ofNullable(this.dequeOfVisitedNamed.peek())
			.filter(scope -> !scope.isEmpty())
			.orElse(afterStatement);

		return items
			.stream()
			.filter(allNamedElementsHaveResolvedNames)
			.map(IdentifiableElement::asExpression)
			.collect(Collectors.collectingAndThen(Collectors.toCollection(LinkedHashSet::new), Collections::unmodifiableSet));
	}

	public PatternElement lookup(Named node) {

		if (!hasScope() || node.getSymbolicName().isEmpty()) {
			return null;
		}

		var scope = dequeOfVisitedNamed.peek();
		var identifiedBy = identifiedBy(node);
		return scope.stream()
			.filter(byHasAName())
			.filter(PatternElement.class::isInstance)
			.filter(i -> {
				var identifier = extractIdentifier(i);
				return identifier != null && identifiedBy.test(identifier);
			})
			.map(PatternElement.class::cast)
			.findFirst()
			.orElse(null);
	}

	/**
	 * @return The set of current imports
	 */
	public Set<SymbolicName> getCurrentImports() {
		return Optional.ofNullable(this.currentImports.peek()).stream()
			.flatMap(v -> v.stream()
				.map(Cypher::name))
			.collect(Collectors.toSet());
	}
}
