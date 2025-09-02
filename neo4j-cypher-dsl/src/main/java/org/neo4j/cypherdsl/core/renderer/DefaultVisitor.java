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

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import org.neo4j.cypherdsl.build.annotations.RegisterForReflection;
import org.neo4j.cypherdsl.core.AliasedExpression;
import org.neo4j.cypherdsl.core.Case;
import org.neo4j.cypherdsl.core.CollectExpression;
import org.neo4j.cypherdsl.core.Condition;
import org.neo4j.cypherdsl.core.CountExpression;
import org.neo4j.cypherdsl.core.Create;
import org.neo4j.cypherdsl.core.Delete;
import org.neo4j.cypherdsl.core.ExistentialSubquery;
import org.neo4j.cypherdsl.core.Foreach;
import org.neo4j.cypherdsl.core.FunctionInvocation;
import org.neo4j.cypherdsl.core.Hint;
import org.neo4j.cypherdsl.core.InTransactions;
import org.neo4j.cypherdsl.core.KeyValueMapEntry;
import org.neo4j.cypherdsl.core.Labels;
import org.neo4j.cypherdsl.core.Limit;
import org.neo4j.cypherdsl.core.ListComprehension;
import org.neo4j.cypherdsl.core.ListExpression;
import org.neo4j.cypherdsl.core.Literal;
import org.neo4j.cypherdsl.core.MapExpression;
import org.neo4j.cypherdsl.core.Match;
import org.neo4j.cypherdsl.core.Merge;
import org.neo4j.cypherdsl.core.MergeAction;
import org.neo4j.cypherdsl.core.Named;
import org.neo4j.cypherdsl.core.NestedExpression;
import org.neo4j.cypherdsl.core.Node;
import org.neo4j.cypherdsl.core.NodeLabel;
import org.neo4j.cypherdsl.core.Operation;
import org.neo4j.cypherdsl.core.Operator;
import org.neo4j.cypherdsl.core.Order;
import org.neo4j.cypherdsl.core.Parameter;
import org.neo4j.cypherdsl.core.PatternComprehension;
import org.neo4j.cypherdsl.core.PatternExpression;
import org.neo4j.cypherdsl.core.PatternSelector;
import org.neo4j.cypherdsl.core.ProcedureCall;
import org.neo4j.cypherdsl.core.Properties;
import org.neo4j.cypherdsl.core.PropertyLookup;
import org.neo4j.cypherdsl.core.QuantifiedPathPattern;
import org.neo4j.cypherdsl.core.Relationship;
import org.neo4j.cypherdsl.core.Remove;
import org.neo4j.cypherdsl.core.Return;
import org.neo4j.cypherdsl.core.Set;
import org.neo4j.cypherdsl.core.Skip;
import org.neo4j.cypherdsl.core.SortItem;
import org.neo4j.cypherdsl.core.StatementContext;
import org.neo4j.cypherdsl.core.Subquery;
import org.neo4j.cypherdsl.core.SubqueryExpression;
import org.neo4j.cypherdsl.core.SymbolicName;
import org.neo4j.cypherdsl.core.UnionPart;
import org.neo4j.cypherdsl.core.Unwind;
import org.neo4j.cypherdsl.core.Use;
import org.neo4j.cypherdsl.core.Where;
import org.neo4j.cypherdsl.core.With;
import org.neo4j.cypherdsl.core.ast.ProvidesAffixes;
import org.neo4j.cypherdsl.core.ast.TypedSubtree;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.internal.CaseElse;
import org.neo4j.cypherdsl.core.internal.CaseWhenThen;
import org.neo4j.cypherdsl.core.internal.ConstantParameterHolder;
import org.neo4j.cypherdsl.core.internal.Distinct;
import org.neo4j.cypherdsl.core.internal.LoadCSV;
import org.neo4j.cypherdsl.core.internal.NameResolvingStrategy;
import org.neo4j.cypherdsl.core.internal.Namespace;
import org.neo4j.cypherdsl.core.internal.ProcedureName;
import org.neo4j.cypherdsl.core.internal.ReflectiveVisitor;
import org.neo4j.cypherdsl.core.internal.RelationshipLength;
import org.neo4j.cypherdsl.core.internal.RelationshipPatternCondition;
import org.neo4j.cypherdsl.core.internal.RelationshipTypes;
import org.neo4j.cypherdsl.core.internal.SchemaNamesBridge;
import org.neo4j.cypherdsl.core.internal.ScopingStrategy;
import org.neo4j.cypherdsl.core.internal.UsingPeriodicCommit;
import org.neo4j.cypherdsl.core.internal.YieldItems;

/**
 * This is a simple (some would call it naive) implementation of a visitor to the Cypher
 * AST created by the Cypher builder based on the {@link ReflectiveVisitor reflective
 * visitor}.
 * <p>
 * It takes care of separating elements of subtrees containing the element type with a
 * separator and provides pairs of {@code enter} / {@code leave} for the structuring
 * elements of the Cypher AST as needed.
 * <p>
 * This rendering visitor is not meant to be used outside framework code, and we don't
 * give any guarantees on the format being output apart from that it works within the
 * constraints of SDN-RX respectively SDN 6 and later.
 *
 * @author Michael J. Simons
 * @author Gerrit Meier
 * @since 1.0
 */
@SuppressWarnings({ "unused", "squid:S1172" })
@RegisterForReflection
class DefaultVisitor extends ReflectiveVisitor implements RenderingVisitor {

	private static final EnumSet<Operator> SKIP_SPACES = EnumSet.of(Operator.EXPONENTIATION, Operator.UNARY_MINUS,
			Operator.UNARY_PLUS);

	/**
	 * Target of all rendering.
	 */
	protected final StringBuilder builder = new StringBuilder();

	/**
	 * A set of aliased expressions that already have been seen and for which an alias
	 * must be used on each following appearance.
	 */
	protected final java.util.Set<AliasedExpression> visitableToAliased = new HashSet<>();

	/**
	 * This keeps track on which level of the tree a separator is needed.
	 */
	private final Map<Integer, SeparatorAndSupplier> separatorOnLevel = new ConcurrentHashMap<>();

	/**
	 * Keeps track of scoped, named variables.
	 */
	private final ScopingStrategy scopingStrategy;

	/**
	 * Keeps track if currently in an aliased expression so that the content can be
	 * skipped when already visited.
	 */
	private final Deque<AliasedExpression> currentAliasedElements = new ArrayDeque<>();

	/**
	 * A cache of delegates, avoiding unnecessary object creation.
	 */
	private final Map<Class<? extends Visitor>, Visitor> delegateCache = new ConcurrentHashMap<>();

	private final NameResolvingStrategy nameResolvingStrategy;

	private final boolean enforceSchema;

	private final Map<String, List<Configuration.RelationshipDefinition>> relationshipDefinitions;

	private final Deque<Boolean> inPatternExpression = new ArrayDeque<>();

	/**
	 * Rendering parameters is not a config property due to some needs in Spring Data
	 * Neo4j: This needs to be configured per statement, not per config there.
	 */
	private final boolean renderConstantsAsParameters;

	private final boolean alwaysEscapeNames;

	private final Dialect dialect;

	boolean inReturn;

	boolean inSubquery;

	private final Deque<Boolean> inLabelExpression = new ArrayDeque<>();

	/**
	 * The current level in the tree of cypher elements.
	 */
	private int currentLevel = 0;

	/**
	 * Will be set to true when entering an already visited node.
	 */
	private boolean skipNodeContent = false;

	/**
	 * Will be set to true when entering an already visited relationship.
	 */
	private boolean skipRelationshipContent = false;

	/**
	 * Will be true when inside a {@link RelationshipPatternCondition}.
	 */
	private boolean inRelationshipCondition = false;

	private boolean inEntity;

	private boolean inPropertyLookup;

	private Relationship.Direction directionOverride;

	DefaultVisitor(StatementContext statementContext) {
		this(statementContext, false);
	}

	DefaultVisitor(StatementContext statementContext, boolean renderConstantsAsParameters) {
		this(statementContext, renderConstantsAsParameters, Configuration.newConfig().alwaysEscapeNames(true).build());
	}

	DefaultVisitor(StatementContext statementContext, boolean renderConstantsAsParameters,
			Configuration configuration) {
		this.nameResolvingStrategy = configuration.isUseGeneratedNames()
				? NameResolvingStrategy.useGeneratedNames(statementContext, configuration.getGeneratedNames())
				: NameResolvingStrategy.useGivenNames(statementContext);

		this.scopingStrategy = ScopingStrategy.create(List.of(this.nameResolvingStrategy::enterScope),
				List.of(this.nameResolvingStrategy::leaveScope));

		this.renderConstantsAsParameters = renderConstantsAsParameters;
		this.alwaysEscapeNames = configuration.isAlwaysEscapeNames();
		this.dialect = configuration.getDialect();
		this.enforceSchema = configuration.isEnforceSchema();
		this.relationshipDefinitions = configuration.getRelationshipDefinitions();
	}

	boolean hasIdentifiables() {
		return !this.scopingStrategy.getIdentifiables().isEmpty();
	}

	private void enableSeparator(int level, boolean on, Supplier<String> supplier) {
		if (on) {
			this.separatorOnLevel.put(level,
					new SeparatorAndSupplier(new AtomicReference<>(""), (supplier != null) ? supplier : () -> ""));
		}
		else {
			this.separatorOnLevel.remove(level);
		}
	}

	private Optional<SeparatorAndSupplier> separatorOnCurrentLevel() {

		return Optional.ofNullable(this.separatorOnLevel.get(this.currentLevel));
	}

	@Override
	protected boolean preEnter(Visitable visitable) {

		Visitable lastAliased = this.currentAliasedElements.peek();
		if (this.skipNodeContent || this.visitableToAliased.contains(lastAliased)) {
			return false;
		}

		if (visitable instanceof AliasedExpression aliasedExpression) {
			this.currentAliasedElements.push(aliasedExpression);
		}

		int nextLevel = ++this.currentLevel + 1;
		if (visitable instanceof TypedSubtree<?> ts) {
			enableSeparator(nextLevel, true, ts::separator);
		}

		separatorOnCurrentLevel().ifPresent(ref -> this.builder.append(ref.seperator().getAndSet("")));

		if (visitable instanceof ProvidesAffixes providesAffixes) {
			providesAffixes.getPrefix().ifPresent(this::doWithPrefix);
		}

		boolean doEnter = !this.skipNodeContent;
		if (doEnter) {
			this.scopingStrategy.doEnter(visitable);
		}
		return doEnter;
	}

	@Override
	protected final PreEnterResult getPreEnterResult(Visitable visitable) {
		boolean doEnter = preEnter(visitable);
		if (!doEnter) {
			return PreEnterResult.skip();
		}

		Class<? extends Visitor> handlerType = this.dialect.getHandler(visitable);
		if (handlerType != null) {
			Visitor handler = this.delegateCache.computeIfAbsent(handlerType, this::newHandler);
			return PreEnterResult.delegateTo(handler);
		}
		return PreEnterResult.doEnter();
	}

	private Visitor newHandler(Class<? extends Visitor> handlerType) {
		try {
			Constructor<? extends Visitor> ctor = handlerType.getDeclaredConstructor(DefaultVisitor.class);
			return ctor.newInstance(this);
		}
		catch (NoSuchMethodException | InstantiationException | IllegalAccessException | InvocationTargetException ex) {
			throw new IllegalArgumentException(this.dialect.name()
					+ " has defined an illegal handler not providing a constructor accepting a delegate.");
		}
	}

	@Override
	protected void postLeave(Visitable visitable) {

		this.scopingStrategy.doLeave(visitable);

		separatorOnCurrentLevel().ifPresent(ref -> ref.seperator().set(ref.supplier().get()));

		if (visitable instanceof ProvidesAffixes providesAffixes) {
			providesAffixes.getSuffix().ifPresent(this::doWithSuffix);
		}

		if (visitable instanceof TypedSubtree) {
			enableSeparator(this.currentLevel + 1, false, null);
		}

		if (this.currentAliasedElements.peek() == visitable) {
			this.currentAliasedElements.pop();
		}

		if (visitable instanceof AliasedExpression aliasedExpression) {
			this.visitableToAliased.add(aliasedExpression);
		}

		--this.currentLevel;
	}

	protected void doWithPrefix(String prefix) {
		this.builder.append(prefix);
	}

	protected void doWithSuffix(String suffix) {
		this.builder.append(suffix);
	}

	void enter(Match match) {
		if (match.isOptional()) {
			this.builder.append("OPTIONAL ");
		}
		this.builder.append("MATCH ");
	}

	void leave(Match match) {

		this.builder.append(" ");
	}

	void enter(Where where) {
		this.builder.append(" WHERE ");
	}

	void enter(Create create) {
		this.builder.append("CREATE ");
	}

	void leave(Create create) {
		this.builder.append(" ");
	}

	void enter(Merge merge) {
		this.builder.append("MERGE ");
	}

	void leave(Merge merge) {
		if (!merge.hasEvents()) { // The last SET will include this
			this.builder.append(" ");
		}
	}

	void enter(MergeAction onCreateOrMatchEvent) {
		switch (onCreateOrMatchEvent.getType()) {
			case ON_CREATE -> this.builder.append("ON CREATE");
			case ON_MATCH -> this.builder.append("ON MATCH");
		}
		this.builder.append(" ");
	}

	void enter(Condition condition) {
		this.inRelationshipCondition = condition instanceof RelationshipPatternCondition;
	}

	void leave(Condition condition) {
		this.inRelationshipCondition = false;
	}

	void enter(Distinct distinct) {
		this.builder.append("DISTINCT ");
	}

	void enter(Return returning) {

		this.inReturn = true;
		if (!returning.isRaw()) {
			this.builder.append("RETURN ");
		}
	}

	void leave(Return returning) {
		this.inReturn = false;
	}

	void enter(With with) {
		this.builder.append("WITH ");
	}

	void leave(With with) {
		this.builder.append(" ");
	}

	void enter(Delete delete) {

		if (delete.isDetach()) {
			this.builder.append("DETACH ");
		}

		this.builder.append("DELETE ");
	}

	void leave(Delete match) {
		this.builder.append(" ");
	}

	boolean inLastReturn() {
		return this.inReturn && !this.inSubquery;
	}

	void enter(AliasedExpression aliased) {

		if (this.visitableToAliased.contains(aliased)) {
			this.builder.append(escapeIfNecessary(this.nameResolvingStrategy.resolve(aliased, false, inLastReturn())));
		}
	}

	void leave(AliasedExpression aliased) {

		if (!(this.visitableToAliased.contains(aliased) || this.scopingStrategy.isSkipAliasing())) {
			this.builder.append(" AS ")
				.append(escapeIfNecessary(this.nameResolvingStrategy.resolve(aliased, true, inLastReturn())));
		}
	}

	void enter(NestedExpression nested) {
		this.builder.append("(");
	}

	void leave(NestedExpression nested) {
		this.builder.append(")");
	}

	void enter(Order order) {
		this.builder.append(" ORDER BY ");
	}

	void enter(Skip skip) {
		this.builder.append(" SKIP ");
	}

	void enter(Limit limit) {
		this.builder.append(" LIMIT ");
	}

	void enter(SortItem.Direction direction) {
		this.builder.append(" ").append(direction.getSymbol());
	}

	void enter(PropertyLookup propertyLookup) {

		this.inPropertyLookup = true;
		if (propertyLookup.isDynamicLookup()) {
			this.builder.append("[");
		}
		else {
			this.builder.append(".");
		}
	}

	void leave(PropertyLookup propertyLookup) {

		this.inPropertyLookup = false;
		if (propertyLookup.isDynamicLookup()) {
			this.builder.append("]");
		}
	}

	void enter(FunctionInvocation functionInvocation) {
		String functionName = functionInvocation.getFunctionName();
		if ("elementId".equals(functionName)) {
			functionName = "toString(id";
		}
		this.builder.append(functionName).append("(");
	}

	void leave(FunctionInvocation functionInvocation) {
		String functionName = functionInvocation.getFunctionName();
		if ("elementId".equals(functionName)) {
			this.builder.append(")");
		}
		this.builder.append(")");
	}

	void enter(Operation operation) {

		if (operation.needsGrouping()) {
			this.builder.append("(");
		}
	}

	void enter(Operator operator) {

		Operator.Type type = operator.getType();
		if (type == Operator.Type.LABEL) {
			return;
		}

		boolean skipSpaces = SKIP_SPACES.contains(operator);
		if (type != Operator.Type.PREFIX && !skipSpaces) {
			this.builder.append(" ");
		}
		this.builder.append(operator.getRepresentation());
		if (type != Operator.Type.POSTFIX && !skipSpaces) {
			this.builder.append(" ");
		}
	}

	void leave(Operation operation) {

		if (operation.needsGrouping()) {
			this.builder.append(")");
		}
	}

	void enter(Literal<?> expression) {
		this.builder.append(expression.asString());
	}

	void enter(Node node) {

		this.builder.append("(");

		// This is only relevant for nodes in relationships.
		// Otherwise, all the labels would be rendered again.
		this.skipNodeContent = this.scopingStrategy.hasVisitedBefore(node);

		if (this.skipNodeContent) {
			this.builder.append(this.nameResolvingStrategy
				.resolve(node.getSymbolicName().orElseGet(node::getRequiredSymbolicName), true, false));
		}

		this.inEntity = true;
	}

	void leave(Node node) {

		this.builder.append(")");

		this.skipNodeContent = false;
		this.inEntity = false;
	}

	void enter(NodeLabel nodeLabel) {

		escapeName(nodeLabel.getValue()).ifPresent(label -> {
			var inLabelExpression = this.inLabelExpression.peek();
			if (inLabelExpression == null || !inLabelExpression) {
				this.builder.append(Symbols.NODE_LABEL_START);
			}
			this.builder.append(label);
		});
	}

	void enter(Labels labels) {
		this.inLabelExpression.push(true);
		this.builder.append(":");
		renderLabelExpression(labels, null);
	}

	@SuppressWarnings("squid:S3776")
	void renderLabelExpression(Labels l, Labels.Type parent) {
		if (l == null) {
			return;
		}
		if (l.isNegated()) {
			this.builder.append("!");
		}
		var current = l.getType();
		boolean close = false;
		if (current != Labels.Type.LEAF) {
			close = (parent != null || l.isNegated()) && l.getType() != parent;
			if (close && !l.isNegated() && (current == Labels.Type.CONJUNCTION || parent == Labels.Type.DISJUNCTION)) {
				close = false;
			}
		}
		if (close) {
			this.builder.append("(");
		}
		renderLabelExpression(l.getLhs(), current);
		if (current == Labels.Type.LEAF) {
			l.getValue().forEach(v -> v.accept(this));
		}
		else {
			this.builder.append(current.getValue());
		}
		renderLabelExpression(l.getRhs(), current);
		if (close) {
			this.builder.append(")");
		}
	}

	void enter(Labels.Value value) {

		var modifier = switch (value.modifier()) {
			case ALL -> "$(";
			case ANY -> "$any(";
			case STATIC -> "";
		};

		this.builder.append(modifier);
	}

	void leave(Labels.Value value) {

		if (value.modifier() != Labels.Modifier.STATIC) {
			this.builder.append(")");
		}
	}

	void leave(Labels labels) {
		this.inLabelExpression.pop();
	}

	void enter(Properties properties) {

		this.builder.append(" ");
	}

	void enter(SymbolicName symbolicName) {
		if (!this.inRelationshipCondition || this.nameResolvingStrategy.isResolved(symbolicName)) {
			if (Boolean.TRUE.equals(this.inPatternExpression.peek())
					&& !this.scopingStrategy.hasVisitedBefore(new Named() {
						@Override
						public Optional<SymbolicName> getSymbolicName() {
							return Optional.of(symbolicName);
						}
					})) {
				return;
			}

			this.builder.append(this.nameResolvingStrategy.resolve(symbolicName, this.inEntity, this.inPropertyLookup));
		}
	}

	void enter(PatternExpression p) {
		this.inPatternExpression.push(true);
	}

	void leave(PatternExpression p) {
		this.inPatternExpression.pop();
	}

	void enter(Relationship relationship) {
		this.skipRelationshipContent = this.scopingStrategy.hasVisitedBefore(relationship);
		if (this.enforceSchema && relationship.getDetails().getDirection() != Relationship.Direction.UNI) {
			this.directionOverride = computeDirectionOverride(relationship);
		}
	}

	/**
	 * Helper to retrieve a nodes label.
	 * @param node the node
	 * @return a set of labels
	 */
	private java.util.Set<String> getLabels(Node node) {
		var nl = node.getLabels();
		if (nl.isEmpty()) {
			var patternElement = this.scopingStrategy.lookup(node);
			if (patternElement instanceof Node boundNode) {
				nl = boundNode.getLabels();
			}
		}
		return nl.stream().map(NodeLabel::getValue).collect(Collectors.toSet());
	}

	/**
	 * Computes an overwrite for enforcing a schema.
	 * @param relationship the relationship to potentially override
	 * @return a new direction
	 */
	Relationship.Direction computeDirectionOverride(Relationship relationship) {
		var sourceLabels = getLabels(relationship.getLeft());
		var targetLabels = getLabels(relationship.getRight());
		var details = relationship.getDetails();

		// Bail out on equal labels
		if (sourceLabels.equals(targetLabels)) {
			return details.getDirection();
		}

		for (String type : details.getTypes()) {
			outer: if (this.relationshipDefinitions.containsKey(type)) {
				var knownRelationships = this.relationshipDefinitions.get(type).stream().toList();
				for (var knownRelationship : knownRelationships) {
					if (knownRelationship.selfReferential() && (sourceLabels.isEmpty() || targetLabels.isEmpty())) {
						break outer;
					}
					if (sourceLabels.contains(knownRelationship.targetLabel())
							&& (targetLabels.isEmpty() || targetLabels.contains(knownRelationship.sourceLabel()))
							|| targetLabels.contains(knownRelationship.sourceLabel()) && (sourceLabels.isEmpty()
									|| sourceLabels.contains(knownRelationship.sourceLabel()))) {
						return Relationship.Direction.RTL;
					}
					else if (sourceLabels.contains(knownRelationship.sourceLabel())
							&& (targetLabels.isEmpty() || targetLabels.contains(knownRelationship.targetLabel()))
							|| targetLabels.contains(knownRelationship.targetLabel()) && (sourceLabels.isEmpty()
									|| sourceLabels.contains(knownRelationship.sourceLabel()))) {
						return Relationship.Direction.LTR;
					}
				}
			}
			if (!sourceLabels.isEmpty() && !targetLabels.isEmpty()) {
				throw new SchemaEnforcementFailedException();
			}
		}
		if (details.getTypes().isEmpty()) {
			var knownRelationships = this.relationshipDefinitions.values().stream().flatMap(List::stream).toList();
			for (var knownRelationship : knownRelationships) {
				if (sourceLabels.contains(knownRelationship.targetLabel())
						&& targetLabels.contains(knownRelationship.sourceLabel())) {
					return Relationship.Direction.RTL;
				}
				else if (sourceLabels.contains(knownRelationship.sourceLabel())
						&& targetLabels.contains(knownRelationship.targetLabel())) {
					return Relationship.Direction.LTR;
				}
			}
		}

		return details.getDirection();
	}

	void enter(Relationship.Details details) {

		Relationship.Direction direction = Optional.ofNullable(this.directionOverride).orElseGet(details::getDirection);
		this.builder.append(direction.getSymbolLeft());
		if (details.hasContent()) {
			this.builder.append("[");
		}

		this.inEntity = true;
	}

	void enter(RelationshipTypes types) {

		if (this.skipRelationshipContent) {
			return;
		}

		this.builder.append(types.getValues()
			.stream()
			.map(this::escapeName)
			.map(Optional::orElseThrow)
			.collect(Collectors.joining(Symbols.REL_TYP_SEPARATOR, Symbols.REL_TYPE_START, "")));
	}

	void enter(RelationshipLength length) {

		if (this.skipRelationshipContent) {
			return;
		}

		Integer minimum = length.getMinimum();
		Integer maximum = length.getMaximum();

		if (length.isUnbounded()) {
			this.builder.append("*");
			return;
		}

		if (minimum == null && maximum == null) {
			return;
		}

		this.builder.append("*");
		if (minimum != null) {
			this.builder.append(minimum);
		}
		this.builder.append("..");
		if (maximum != null) {
			this.builder.append(maximum);
		}
	}

	void leave(Relationship.Details details) {

		Relationship.Direction direction = Optional.ofNullable(this.directionOverride).orElseGet(details::getDirection);
		if (details.hasContent()) {
			this.builder.append("]");
		}
		this.builder.append(direction.getSymbolRight());

		this.inEntity = false;
	}

	void leave(Relationship relationship) {

		this.skipRelationshipContent = false;
		this.directionOverride = null;
	}

	void enter(Parameter<?> parameter) {

		Object value = parameter.getValue();
		if (value instanceof ConstantParameterHolder constantParameterHolder && !this.renderConstantsAsParameters) {
			this.builder.append(constantParameterHolder.asString());
		}
		else {
			this.builder.append("$").append(this.nameResolvingStrategy.resolve(parameter));
		}
	}

	void enter(MapExpression map) {

		this.builder.append("{");
	}

	void enter(KeyValueMapEntry map) {

		this.builder.append(escapeIfNecessary(map.getKey())).append(": ");
	}

	void leave(MapExpression map) {

		this.builder.append("}");
	}

	void enter(ListExpression list) {

		this.builder.append("[");
	}

	void leave(ListExpression list) {

		this.builder.append("]");
	}

	void enter(Unwind unwind) {

		this.builder.append("UNWIND ");
	}

	void leave(Unwind unwind) {

		this.builder.append(" ");
	}

	void enter(UnionPart unionPart) {

		this.builder.append(" UNION ");
		if (unionPart.isAll()) {
			this.builder.append("ALL ");
		}
	}

	void enter(Set set) {

		this.builder.append("SET ");
	}

	void leave(Set set) {
		this.builder.append(" ");
	}

	void enter(Remove remove) {

		this.builder.append("REMOVE ");
	}

	void leave(Remove remove) {
		this.builder.append(" ");
	}

	void enter(PatternComprehension patternComprehension) {
		this.builder.append("[");
	}

	void leave(PatternComprehension patternComprehension) {
		this.builder.append("]");
	}

	void enter(ListComprehension listComprehension) {
		this.builder.append("[");
	}

	void leave(ListComprehension listComprehension) {
		this.builder.append("]");
	}

	void enter(Case genericCase) {
		this.builder.append("CASE");
	}

	void enter(Case.SimpleCase simpleCase) {
		this.builder.append("CASE ");
	}

	void enter(CaseWhenThen caseWhenExpression) {
		this.builder.append(" WHEN ");
	}

	void leave(CaseWhenThen caseWhenExpression) {
		this.builder.append(" THEN ");
	}

	void enter(CaseElse caseElseExpression) {
		this.builder.append(" ELSE ");
	}

	void leave(Case caseExpression) {
		this.builder.append(" END");
	}

	void enter(ProcedureCall procedureCall) {

		this.builder.append("CALL ");
	}

	void leave(Namespace namespace) {

		this.builder.append(".");
	}

	void leave(ProcedureName procedureName) {

		this.builder.append(procedureName.getValue());
	}

	void enter(YieldItems yieldItems) {

		this.builder.append(" YIELD ");
	}

	void leave(ProcedureCall procedureCall) {

		this.builder.append(" ");
	}

	void enter(Enum<?> anEnum) {

		this.builder.append(anEnum.name().replace("_", " ")).append(" ");
	}

	void enter(Subquery subquery) {

		this.inSubquery = true;
		this.builder.append("CALL {");
	}

	void leave(Subquery subquery) {

		this.inSubquery = false;
		int l = this.builder.length() - 1;
		if (this.builder.charAt(l) == ' ' && !subquery.doesReturnOrYield()) {
			this.builder.replace(l, this.builder.length(), "} ");
		}
		else {
			this.builder.append("} ");
		}
	}

	void leave(InTransactions inTransactions) {

		int l = this.builder.length() - 1;
		if (this.builder.charAt(l) != ' ') {
			this.builder.append(" ");
		}
		this.builder.append("IN TRANSACTIONS ");
		if (inTransactions.getRows() != null) {
			this.builder.append("OF ").append(inTransactions.getRows()).append(" ROWS ");
		}
	}

	void enter(Foreach foreach) {

		this.builder.append("FOREACH (");
	}

	void leave(Foreach foreach) {

		this.builder.setCharAt(this.builder.length() - 1, ')'); // replace trailing space
																// with ')'
		this.builder.append(" ");
	}

	void enter(SubqueryExpression subquery) {

		if (subquery instanceof CountExpression) {
			this.builder.append("COUNT");
		}
		else if (subquery instanceof ExistentialSubquery) {
			this.builder.append("EXISTS");
		}
		else if (subquery instanceof CollectExpression) {
			this.builder.append("COLLECT");
		}
		this.builder.append(" { ");
	}

	void leave(SubqueryExpression subquery) {

		// Trimming the inner match without having to do this in the match (looking up if
		// inside subquery).
		if (this.builder.charAt(this.builder.length() - 1) == ' ') {
			this.builder.replace(this.builder.length() - 1, this.builder.length(), " }");
		}
		else {
			this.builder.append(" }");
		}
	}

	void enter(Hint hint) {

		this.builder.append(" USING ");
	}

	void enter(LoadCSV loadCSV) {

		this.builder.append("LOAD CSV");
		if (loadCSV.isWithHeaders()) {
			this.builder.append(" WITH HEADERS");
		}
		this.builder.append(" FROM '").append(loadCSV.getUri().toString()).append("' AS ").append(loadCSV.getAlias());

		if (loadCSV.getFieldTerminator() != null) {
			this.builder.append(" FIELDTERMINATOR '").append(loadCSV.getFieldTerminator()).append("'");
		}
		this.builder.append(" ");
	}

	void enter(UsingPeriodicCommit usingPeriodicCommit) {

		this.builder.append("USING PERIODIC COMMIT ");
		if (usingPeriodicCommit.rate() != null) {
			this.builder.append(usingPeriodicCommit.rate()).append(" ");
		}
	}

	void enter(Use use) {
		this.builder.append("USE ");
		if (use.dynamic()) {
			this.builder.append("graph.byName(");
		}
	}

	void leave(Use use) {
		if (use.dynamic()) {
			this.builder.append(")");
		}
		this.builder.append(" ");
	}

	void enter(PatternSelector shortest) {

		if (shortest instanceof PatternSelector.ShortestK shortestK) {
			this.builder.append("SHORTEST ").append(shortestK.getK()).append(" ");
		}
		else if (shortest instanceof PatternSelector.ShortestKGroups shortestK) {
			this.builder.append("SHORTEST ").append(shortestK.getK()).append(" GROUPS ");
		}
		else if (shortest instanceof PatternSelector.AllShortest) {
			this.builder.append("ALL SHORTEST ");
		}
		else if (shortest instanceof PatternSelector.Any) {
			this.builder.append("ANY ");
		}
	}

	void enter(QuantifiedPathPattern.TargetPattern qpp) {
		this.builder.append("(");
	}

	void leave(QuantifiedPathPattern.TargetPattern qpp) {
		this.builder.append(")");
	}

	void enter(QuantifiedPathPattern.Quantifier quantifier) {

		this.builder.append(quantifier.toString());
	}

	@Override
	public String getRenderedContent() {
		return this.builder.toString();
	}

	/**
	 * Escapes a symbolic name. Such a symbolic name is either used for a nodes label, the
	 * type of a relationship or a variable.
	 * @param unescapedName the name to escape.
	 * @return an empty optional when the unescaped name is {@literal null}, otherwise the
	 * correctly escaped name, safe to be used in statements.
	 */
	protected final Optional<String> escapeName(String unescapedName) {

		return SchemaNamesBridge.sanitize(unescapedName, this.alwaysEscapeNames);
	}

	protected final String escapeIfNecessary(String potentiallyNonIdentifier) {

		return SchemaNamesBridge.sanitize(potentiallyNonIdentifier, false).orElse(null);
	}

	record SeparatorAndSupplier(AtomicReference<String> seperator, Supplier<String> supplier) {
	}

}
