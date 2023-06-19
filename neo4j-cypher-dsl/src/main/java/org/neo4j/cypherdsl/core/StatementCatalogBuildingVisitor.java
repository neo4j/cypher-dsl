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
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.neo4j.cypherdsl.build.annotations.RegisterForReflection;
import org.neo4j.cypherdsl.core.ParameterCollectingVisitor.ParameterInformation;
import org.neo4j.cypherdsl.core.StatementCatalog.Clause;
import org.neo4j.cypherdsl.core.StatementCatalog.PropertyFilter;
import org.neo4j.cypherdsl.core.StatementCatalog.Token;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.internal.Namespace;
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

	private final Set<Token> tokens = new LinkedHashSet<>();

	private final Set<StatementCatalog.Property> properties = new LinkedHashSet<>();

	private final Set<StatementCatalog.LabelFilter> labelFilters = new LinkedHashSet<>();

	private final Map<StatementCatalog.Property, Set<PropertyFilter>> propertyFilters = new LinkedHashMap<>();

	/**
	 * Scoped lookup tables from symbolic name to pattern elements (nodes or relationships).
	 */
	private final Deque<Map<SymbolicName, PatternElement>> patternLookup = new ArrayDeque<>();

	/**
	 * A stack of conditions (keeping track of the latest entered).
	 */
	private final Deque<org.neo4j.cypherdsl.core.Condition> currentConditions = new ArrayDeque<>();

	private final AtomicReference<Set<Token>> currentHasLabelCondition = new AtomicReference<>();

	/**
	 * Required for resolving parameter names, must be from the same statement that is analyzed.
	 */
	private final StatementContext statementContext;

	private final boolean renderConstantsAsParameters;

	/**
	 * Delegating the hard work to the shared scope strategy in most cases.
	 */
	private final ScopingStrategy scopingStrategy;

	private final ParameterCollectingVisitor allParameters;

	private final Map<Node, Set<Token>> currentUndirectedRelations = new HashMap<>();
	private final Map<Node, Set<Token>> currentIncomingRelations = new HashMap<>();
	private final Map<Node, Set<Token>> currentOutgoingRelations = new HashMap<>();

	private final Map<Token, Relationships> relationships = new HashMap<>();

	private final Set<Literal<?>> literals = new LinkedHashSet<>();

	StatementCatalogBuildingVisitor(StatementContext statementContext, boolean renderConstantsAsParameters) {

		this.statementContext = statementContext;
		this.renderConstantsAsParameters = renderConstantsAsParameters;
		this.scopingStrategy = ScopingStrategy.create(
			List.of((cause, imports) -> patternLookup.push(createNewScope(imports))),
			List.of((cause, exports) -> importIntoCurrentScope(exports))
		);
		this.patternLookup.push(new HashMap<>());

		this.allParameters = new ParameterCollectingVisitor(statementContext, renderConstantsAsParameters);
	}

	private Map<SymbolicName, PatternElement> createNewScope(Collection<IdentifiableElement> imports) {
		addRelationsInCurrentScope();

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

	/**
	 * A holder for the relationship types connected to node labels
	 * @param outgoing Outgoing types
	 * @param incoming Incoming types
	 * @param undirected Undirected connections
	 */
	record Relationships(Set<Token> outgoing, Set<Token> incoming, Set<Token> undirected) {

		static Relationships empty() {
			return new Relationships(Set.of(), Set.of(), Set.of());
		}

		Relationships() {
			this(new HashSet<>(), new HashSet<>(), new HashSet<>());
		}

		Relationships copy() {
			return new Relationships(Set.copyOf(this.outgoing), Set.copyOf(this.incoming), Set.copyOf(this.undirected));
		}
	}

	StatementCatalog getResult() {

		addRelationsInCurrentScope();
		var parameterInformation = allParameters.getResult();
		return new DefaultStatementCatalog(this.tokens, this.labelFilters, this.properties, this.propertyFilters, scopingStrategy.getIdentifiables(), parameterInformation, relationships, literals);
	}

	void addRelationsInCurrentScope() {

		finish(currentOutgoingRelations, Relationships::outgoing);
		finish(currentIncomingRelations, Relationships::incoming);
		finish(currentUndirectedRelations, Relationships::undirected);
	}

	/**
	 * Finishes up the relationship's storage (retrieval of actual labels from the nodes)
	 *
	 * @param nodesToRelations The map to process
	 * @param targetProvider   The target where to store the tokens
	 */
	private void finish(Map<Node, Set<Token>> nodesToRelations, Function<Relationships, Set<Token>> targetProvider) {
		nodesToRelations.forEach((k, v) -> {
			var labels = getAllLabels((Node) k.getSymbolicName().map(this::lookup).orElse(k));
			labels.forEach(t -> {
				var rels = relationships.computeIfAbsent(t, unused -> new Relationships());
				targetProvider.apply(rels).addAll(v);
			});
		});
		nodesToRelations.clear();
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
			property = new StatementCatalog.Property(getAllLabels(node), mapEntry.getKey());
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
		this.propertyFilters.computeIfAbsent(property, ignored -> new HashSet<>())
			.add(new PropertyFilter(currentClause.get(), left, Operator.EQUALITY, mapEntry.getValue(), parameterInformation.names, parameterInformation.values));
	}

	void leave(Node node) {
		currentPatternElement.removeFirstOccurrence(node);
	}

	void enter(Relationship relationship) {

		relationship.getSymbolicName().ifPresent(s -> store(s, relationship));
		currentPatternElement.push(relationship);
		var types = relationship.getDetails().getTypes().stream().map(Token::type).toList();
		tokens.addAll(types);

		storeRelations(relationship.getLeft(), relationship.getRight(), types, relationship.getDetails().getDirection());
	}

	/**
	 * Stores the source and target of this relationships in incoming/outgoing and undirected lists for processing
	 * after leaving the scope / statement (when all labels are known).
	 *
	 * @param left      Left hand side of the relation
	 * @param right     Right hand side of the relation
	 * @param types     Types of the relation
	 * @param direction Direction of the relation
	 */
	private void storeRelations(Node left, Node right, List<Token> types, Relationship.Direction direction) {

		final Function<Node, Set<Token>> targetSupplier = unused -> new HashSet<>();
		switch (direction) {
			case UNI -> {
				currentUndirectedRelations.computeIfAbsent(left, targetSupplier).addAll(types);
				currentUndirectedRelations.computeIfAbsent(right, targetSupplier).addAll(types);
			}
			case LTR -> {
				currentOutgoingRelations.computeIfAbsent(left, targetSupplier).addAll(types);
				currentIncomingRelations.computeIfAbsent(right, targetSupplier).addAll(types);
			}
			case RTL -> {
				currentIncomingRelations.computeIfAbsent(left, targetSupplier).addAll(types);
				currentOutgoingRelations.computeIfAbsent(right, targetSupplier).addAll(types);
			}
		}
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
				getAllLabels(node),
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
			propertyFilters.computeIfAbsent(newProperty, ignored -> new HashSet<>())
				.add(extractPropertyCondition(newProperty, currentConditions.peek()));
		}
	}

	private static Set<Token> getAllLabels(Node node) {
		Set<Token> result = new TreeSet<>();
		if (node.getLabels().isEmpty()) {
			node.accept(segment -> {
				if (segment instanceof LabelExpression l) {
					collectLabels(l, null, result);
				}
			});
		} else {
			node.getLabels().stream()
				.map(NodeLabel::getValue)
				.map(Token::label)
				.forEach(result::add);
		}
		return result;
	}

	private static void collectLabels(LabelExpression l, LabelExpression.Type parent, Set<Token> labels) {
		if (l == null) {
			return;
		}
		var current = l.type();
		collectLabels(l.lhs(), current, labels);
		if (current == LabelExpression.Type.LEAF) {
			l.value().stream().map(Token::label).forEach(labels::add);
		}
		collectLabels(l.rhs(), current, labels);
	}

	void enter(Parameter<?> parameter) {
		this.allParameters.enter(parameter);
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

	private PropertyFilter extractPropertyCondition(StatementCatalog.Property property, org.neo4j.cypherdsl.core.Condition condition) {

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
		return new PropertyFilter(currentClause.get(), left.get(), op.get(), right.get(), parameterInformation.names, parameterInformation.values);
	}

	void enter(NodeLabel label) {
		this.tokens.add(new Token(Token.Type.NODE_LABEL, label.getValue()));
		var currentCondition = currentConditions.peek();
		if (currentCondition instanceof HasLabelCondition hasLabelCondition) {
			this.currentHasLabelCondition.get().add(Token.label(label));
		}
	}

	void enter(LabelExpression labelExpression) {
		collectLabels(labelExpression, null, tokens);
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
		if (condition instanceof HasLabelCondition) {
			this.currentHasLabelCondition.compareAndSet(null, new TreeSet<>());
		}
	}

	void enter(Literal<?> literal) {
		if (literal instanceof Asterisk || literal instanceof PeriodLiteral || literal instanceof RawLiteral.RawElement || literal == Merge.BLANK || literal == ListOperator.DOTS || literal instanceof Namespace) {
			return;
		}
		this.literals.add(literal);
	}

	void leave(org.neo4j.cypherdsl.core.Condition condition) {
		if (TYPE_OF_COMPOUND_CONDITION.equals(condition.getClass().getName())) {
			return;
		}
		this.currentConditions.pop();

		var setOfRequiredTokens = currentHasLabelCondition.getAndSet(null);
		if (condition instanceof HasLabelCondition hasLabelCondition && setOfRequiredTokens != null) {
			AtomicReference<String> symbolicName = new AtomicReference<>();
			hasLabelCondition.accept(segment -> {
				if (segment instanceof SymbolicName s) {
					symbolicName.compareAndSet(null, s.getValue());
				}
			});
			this.labelFilters.add(new StatementCatalog.LabelFilter(symbolicName.get(), setOfRequiredTokens));
		}
	}

	void store(SymbolicName s, PatternElement patternElement) {
		if (patternLookup.isEmpty()) {
			throw new IllegalStateException("Invalid scope");
		}
		var currentScope = patternLookup.peek();
		// Don't overwrite in same scope or when imported,
		// size == 1 catering for with clauses on top level
		if (currentScope.containsKey(s) && (scopingStrategy.getCurrentImports().contains(s) || patternLookup.size() == 1)) {
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

		private final Collection<LabelFilter> labelFilters;

		private final Map<Property, Collection<PropertyFilter>> propertyFilters;

		private final Set<Expression> identifiableExpressions;

		private final ParameterInformation parameterInformation;

		private final Map<Token, Relationships> relationships;

		private final Set<Literal<?>> literals;

		@SuppressWarnings("squid:S107") // Totally fine with that number of args.
		DefaultStatementCatalog(
			Set<Token> tokens,
			Set<LabelFilter> labelFilters,
			Set<Property> properties,
			Map<Property, Set<PropertyFilter>> propertyFilters,
			Collection<Expression> identifiableExpressions,
			ParameterInformation parameterInformation,
			Map<Token, Relationships> relationships,
			Set<Literal<?>> literals
		) {
			this.tokens = Collections.unmodifiableSet(tokens);
			this.labelFilters = Collections.unmodifiableSet(labelFilters);

			this.properties = Collections.unmodifiableSet(properties);
			this.propertyFilters = propertyFilters.entrySet().stream()
				.collect(Collectors.toUnmodifiableMap(Map.Entry::getKey, e -> Collections.unmodifiableSet(e.getValue())));

			this.identifiableExpressions = identifiableExpressions instanceof Set<Expression> s ? Collections.unmodifiableSet(s) : Set.copyOf(identifiableExpressions);
			this.parameterInformation = parameterInformation;

			this.relationships = relationships.entrySet().stream()
				.collect(Collectors.toUnmodifiableMap(Map.Entry::getKey, e -> e.getValue().copy()));

			this.literals = Collections.unmodifiableSet(literals);
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
		public Collection<LabelFilter> getAllLabelFilters() {
			return this.labelFilters;
		}

		@Override
		public Map<Property, Collection<PropertyFilter>> getAllPropertyFilters() {
			return this.propertyFilters;
		}

		@Override
		public Set<Expression> getIdentifiableExpressions() {
			return identifiableExpressions;
		}

		@Override
		public Map<String, Object> getParameters() {
			return parameterInformation.values;
		}

		@Override
		public Collection<String> getParameterNames() {
			return parameterInformation.names;
		}

		@Override
		public Map<String, String> getRenamedParameters() {
			return parameterInformation.renames;
		}

		@Override
		public Collection<Token> getOutgoingRelations(Token label) {

			return extractRelations(label, Relationships::outgoing);
		}

		private Collection<Token> extractRelations(Token label, Function<Relationships, Set<Token>> tokenProvider) {
			if (label.type() != Token.Type.NODE_LABEL) {
				throw new IllegalArgumentException(label + " must be a node label, not a relationship type");
			}

			return tokenProvider.apply(relationships.getOrDefault(label, Relationships.empty()));
		}

		@Override
		public Collection<Token> getTargetNodes(Token type) {

			if (type.type() != Token.Type.RELATIONSHIP_TYPE) {
				throw new IllegalArgumentException(type + " must be a relationship type, not a node label");
			}

			return relationships
				.entrySet()
				.stream()
				.filter(e -> e.getValue().incoming().contains(type))
				.map(Map.Entry::getKey)
				.collect(Collectors.toSet());
		}

		@Override
		public Collection<Token> getIncomingRelations(Token label) {

			return extractRelations(label, Relationships::incoming);
		}

		@Override
		public Collection<Token> getSourceNodes(Token type) {

			if (type.type() != Token.Type.RELATIONSHIP_TYPE) {
				throw new IllegalArgumentException(type + " must be a relationship type, not a node label");
			}

			return relationships
				.entrySet()
				.stream()
				.filter(e -> e.getValue().outgoing().contains(type))
				.map(Map.Entry::getKey)
				.collect(Collectors.toSet());
		}

		@Override
		public Collection<Token> getUndirectedRelations(Token label) {

			return extractRelations(label, Relationships::undirected);
		}

		@Override
		public Set<Literal<?>> getLiterals() {
			return literals;
		}
	}
}
