/*
 * Copyright (c) 2019-2023 "Neo4j,"
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

import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import org.neo4j.cypherdsl.build.annotations.RegisterForReflection;
import org.neo4j.cypherdsl.core.ParameterCollectingVisitor.ParameterInformation;
import org.neo4j.cypherdsl.core.StatementCatalog.Condition;
import org.neo4j.cypherdsl.core.StatementCatalog.Condition.Clause;
import org.neo4j.cypherdsl.core.StatementCatalog.Token;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.internal.ReflectiveVisitor;
import org.neo4j.cypherdsl.core.internal.ScopingStrategy;

/**
 * This visitor  creates a  {@link StatementCatalog  statement catalog}.  It is  not thread  safe and  must not  be used
 * multiple times. Please create a new instance on each invocation.
 *
 * @author Michael J. Simons
 * @soundtrack Avenger - Prayers Of Steel
 * @since 2023.1.0
 */
@RegisterForReflection
@SuppressWarnings({"unused", "squid:S1172"})
class StatementCatalogBuildingVisitor extends ReflectiveVisitor {

	/**
	 * Constant class name for skipping compounds, not inclined to make this type public.
	 */
	private static final String TYPE_OF_COMPOUND_CONDITION = "org.neo4j.cypherdsl.core.CompoundCondition";

	/**
	 * Current clause the visitor is in.
	 */
	private final AtomicReference<Clause> currentClause = new AtomicReference<>(Clause.UNKNOWN);

	/**
	 * The current pattern element visited.
	 */
	private final Deque<PatternElement> currentPatternElement = new ArrayDeque<>();

	private final Set<Token> tokens = new HashSet<>();

	private final Set<StatementCatalog.Property> properties = new HashSet<>();

	private final Map<StatementCatalog.Property, Set<Condition>> conditions = new HashMap<>();

	/**
	 * Scoped lookup tables from symbolic name to pattern elements (nodes or relationships).
	 */
	private final Deque<Map<SymbolicName, PatternElement>> patternLookup = new ArrayDeque<>();

	/**
	 * A stack of conditions (keeping track of the latest entered).
	 */
	private final Deque<org.neo4j.cypherdsl.core.Condition> currentConditions = new ArrayDeque<>();

	/**
	 * Required for resolving parameter names, must be from the same statement that is analyzed.
	 */
	private final StatementContext statementContext;

	private final boolean renderConstantsAsParameters;

	/**
	 * Delegating the hard work to the shared scope strategy in most cases.
	 */
	private final ScopingStrategy scopingStrategy;

	StatementCatalogBuildingVisitor(StatementContext statementContext, boolean renderConstantsAsParameters) {

		this.statementContext = statementContext;
		this.renderConstantsAsParameters = renderConstantsAsParameters;
		this.scopingStrategy = ScopingStrategy.create(
			List.of((cause, imports) -> patternLookup.push(createNewScope(imports))),
			List.of((cause, exports) -> importIntoCurrentScope(exports))
		);
		this.patternLookup.push(new HashMap<>());
	}

	private Map<SymbolicName, PatternElement> createNewScope(Collection<IdentifiableElement> imports) {
		Map<SymbolicName, PatternElement> currentScope = patternLookup.isEmpty() ? Collections.emptyMap() : patternLookup.peek();
		Map<SymbolicName, PatternElement> newScope = new HashMap<>();
		copyIdentifiableElements(imports, currentScope, newScope);
		return newScope;
	}

	private void importIntoCurrentScope(Collection<IdentifiableElement> exports) {
		Map<SymbolicName, PatternElement> previousScope = patternLookup.pop();
		Map<SymbolicName, PatternElement> currentScope = patternLookup.isEmpty() ? new HashMap<>() : patternLookup.peek();
		copyIdentifiableElements(exports, previousScope, currentScope);
	}

	private static void copyIdentifiableElements(Collection<IdentifiableElement> elements, Map<SymbolicName, PatternElement> source, Map<SymbolicName, PatternElement> target) {
		for (IdentifiableElement e : elements) {
			if (e instanceof SymbolicName s && source.containsKey(s)) {
				target.put(s, source.get(s));
			} else if (e instanceof Named n && e instanceof PatternElement p) {
				target.put(n.getRequiredSymbolicName(), p);
			}
		}
	}

	StatementCatalog getResult() {
		return new DefaultStatementCatalog(this.tokens, this.properties, this.conditions, scopingStrategy.getIdentifiables());
	}

	@Override
	protected boolean preEnter(Visitable visitable) {
		scopingStrategy.doEnter(visitable);
		return true;
	}

	@Override
	protected void postLeave(Visitable visitable) {
		scopingStrategy.doLeave(visitable);
	}

	void enter(Match match) {
		currentClause.compareAndSet(Clause.UNKNOWN, Clause.MATCH);
	}

	void leave(Match match) {
		currentClause.compareAndSet(Clause.MATCH, Clause.UNKNOWN);
	}

	void enter(Create create) {
		currentClause.compareAndSet(Clause.UNKNOWN, Clause.CREATE);
	}

	void leave(Create create) {
		currentClause.compareAndSet(Clause.CREATE, Clause.UNKNOWN);
	}

	void enter(Merge merge) {
		currentClause.compareAndSet(Clause.UNKNOWN, Clause.MERGE);
	}

	void leave(Merge merge) {
		currentClause.compareAndSet(Clause.MERGE, Clause.UNKNOWN);
	}

	void enter(Delete delete) {
		currentClause.compareAndSet(Clause.UNKNOWN, Clause.DELETE);
	}

	void leave(Delete delete) {
		currentClause.compareAndSet(Clause.DELETE, Clause.UNKNOWN);
	}

	void enter(With with) {
		currentClause.compareAndSet(Clause.UNKNOWN, Clause.WITH);
	}

	void leave(With with) {
		currentClause.compareAndSet(Clause.WITH, Clause.UNKNOWN);
	}

	void enter(Node node) {

		node.getSymbolicName().ifPresent(s -> store(s, node));
		currentPatternElement.push(node);
	}

	void enter(KeyValueMapEntry mapEntry) {

		var owner = currentPatternElement.peek();
		if (owner == null) {
			return;
		}

		StatementCatalog.Property property;
		if (owner instanceof Node node) {
			property = new StatementCatalog.Property(node.getLabels().stream().map(Token::label).collect(Collectors.toSet()), mapEntry.getKey());
		} else if (owner instanceof Relationship relationship) {
			property = new StatementCatalog.Property(relationship.getDetails().getTypes().stream().map(Token::type).collect(Collectors.toSet()), mapEntry.getKey());
		} else {
			property = null;
		}

		if (property == null) {
			return;
		}
		this.properties.add(property);

		Expression left;
		if (((PropertyContainer) owner).getSymbolicName().isPresent()) {
			left = ((PropertyContainer) owner).property(mapEntry.getKey());
		} else {
			left = PropertyLookup.forName(mapEntry.getKey());
		}
		var parameterInformation = extractParameters(mapEntry.getValue());
		this.conditions.computeIfAbsent(property, ignored -> new HashSet<>())
			.add(new Condition(currentClause.get(), left, Operator.EQUALITY, mapEntry.getValue(), parameterInformation.names, parameterInformation.values));
	}

	void leave(Node node) {
		currentPatternElement.removeFirstOccurrence(node);
	}

	void enter(Relationship relationship) {

		relationship.getSymbolicName().ifPresent(s -> store(s, relationship));
		currentPatternElement.push(relationship);
	}

	void leave(Relationship relationship) {
		currentPatternElement.removeFirstOccurrence(relationship);
	}

	void enter(org.neo4j.cypherdsl.core.Property property) {

		if (property.getNames().size() != 1) {
			return;
		}
		var lookup = property.getNames().get(0);
		if (lookup.isDynamicLookup()) {
			return;
		}

		if (!(property.getContainerReference() instanceof SymbolicName s)) {
			return;
		}

		var propertyName = new AtomicReference<String>();
		lookup.accept(segment -> {
			if (segment instanceof SymbolicName name) {
				propertyName.compareAndSet(null, name.getValue());
			}
		});

		StatementCatalog.Property newProperty;
		var patternElement = lookup(s);
		if (patternElement instanceof Node node) {
			newProperty = new StatementCatalog.Property(
				node.getLabels().stream().map(NodeLabel::getValue).map(Token::label).collect(Collectors.toSet()),
				propertyName.get()
			);
		} else if (patternElement instanceof Relationship relationship) {
			newProperty = new StatementCatalog.Property(
				relationship.getDetails().getTypes().stream().map(Token::type).collect(Collectors.toSet()),
				propertyName.get()
			);
		} else {
			return;
		}

		properties.add(newProperty);
		if (inCurrentCondition(property)) {
			conditions.computeIfAbsent(newProperty, ignored -> new HashSet<>())
				.add(extractPropertyCondition(newProperty, currentConditions.peek()));
		}
	}

	private boolean inCurrentCondition(org.neo4j.cypherdsl.core.Property property) {
		var currentCondition = this.currentConditions.peek();

		if (currentCondition == null) {
			return false;
		}

		var result = new AtomicBoolean();
		currentCondition.accept(segment -> {
			if (segment == property) {
				result.compareAndSet(false, true);
			}
		});
		return result.get();
	}

	private Condition extractPropertyCondition(StatementCatalog.Property property, org.neo4j.cypherdsl.core.Condition condition) {

		var left = new AtomicReference<Expression>();
		var op = new AtomicReference<Operator>();
		var right = new AtomicReference<Expression>();
		condition.accept(new Visitor() {
			int cnt;

			@Override
			public void enter(Visitable segment) {
				if (++cnt != 2) {
					return;
				}
				if (segment instanceof Operator operator) {
					op.compareAndSet(null, operator);
				} else if (segment instanceof Expression expression && !left.compareAndSet(null, expression)) {
					right.compareAndSet(null, expression);
				}
			}

			@Override
			public void leave(Visitable segment) {
				--cnt;
			}
		});
		var parameterInformation = extractParameters(left.get(), right.get());
		return new Condition(currentClause.get(), left.get(), op.get(), right.get(), parameterInformation.names, parameterInformation.values);
	}

	void enter(NodeLabel label) {
		this.tokens.add(new Token(Token.Type.NODE_LABEL, label.getValue()));
	}

	void enter(Relationship.Details details) {
		details.getTypes().stream().map(Token::type).forEach(tokens::add);
	}

	PatternElement lookup(SymbolicName s) {
		if (patternLookup.isEmpty()) {
			throw new IllegalStateException("Invalid scope");
		}
		return patternLookup.peek().get(s);
	}

	void enter(org.neo4j.cypherdsl.core.Condition condition) {
		if (TYPE_OF_COMPOUND_CONDITION.equals(condition.getClass().getName())) {
			return;
		}
		this.currentConditions.push(condition);
	}

	void leave(org.neo4j.cypherdsl.core.Condition condition) {
		if (TYPE_OF_COMPOUND_CONDITION.equals(condition.getClass().getName())) {
			return;
		}
		this.currentConditions.pop();
	}

	void store(SymbolicName s, PatternElement patternElement) {
		if (patternLookup.isEmpty()) {
			throw new IllegalStateException("Invalid scope");
		}
		var currentScope = patternLookup.peek();
		// Don't overwrite in same scope or when imported
		if (currentScope.containsKey(s) && scopingStrategy.getCurrentImports().contains(s)) {
			return;
		}
		currentScope.put(s, patternElement);
	}

	private ParameterInformation extractParameters(Expression... expressions) {

		var parameterCollectingVisitor = new ParameterCollectingVisitor(this.statementContext, this.renderConstantsAsParameters);
		for (Expression expression : expressions) {
			if (expression == null) {
				continue;
			}
			expression.accept(parameterCollectingVisitor);
		}
		return parameterCollectingVisitor.getResult();
	}

	static final class DefaultStatementCatalog implements StatementCatalog {

		private final Set<Token> tokens;

		private final Set<Property> properties;

		private final Map<Property, Collection<Condition>> conditions;

		private final Set<Expression> identifiableExpressions;

		DefaultStatementCatalog(Set<Token> tokens, Set<Property> properties, Map<Property, Set<Condition>> conditions, Collection<Expression> identifiableExpressions) {
			this.tokens = Set.copyOf(tokens);
			this.properties = Set.copyOf(properties);
			this.conditions = conditions.entrySet().stream()
				.collect(Collectors.collectingAndThen(Collectors.toMap(Map.Entry::getKey, e -> Set.copyOf(e.getValue())), Map::copyOf));
			this.identifiableExpressions = Set.copyOf(identifiableExpressions);
		}

		@Override
		public Set<Token> getAllTokens() {
			return tokens;
		}

		@Override
		public Set<Property> getProperties() {
			return properties;
		}

		@Override
		public Map<Property, Collection<Condition>> getAllConditions() {
			return conditions;
		}

		@Override
		public Set<Expression> getIdentifiableExpressions() {
			return identifiableExpressions;
		}
	}
}
