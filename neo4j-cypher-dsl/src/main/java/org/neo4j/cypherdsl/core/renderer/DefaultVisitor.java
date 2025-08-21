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

import org.jetbrains.annotations.NotNull;
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
import org.neo4j.cypherdsl.core.LabelExpression;
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
import org.neo4j.cypherdsl.core.PatternExpression;
import org.neo4j.cypherdsl.core.QuantifiedPathPattern;
import org.neo4j.cypherdsl.core.PatternComprehension;
import org.neo4j.cypherdsl.core.ProcedureCall;
import org.neo4j.cypherdsl.core.Properties;
import org.neo4j.cypherdsl.core.PropertyLookup;
import org.neo4j.cypherdsl.core.Relationship;
import org.neo4j.cypherdsl.core.Remove;
import org.neo4j.cypherdsl.core.Return;
import org.neo4j.cypherdsl.core.Set;
import org.neo4j.cypherdsl.core.PatternSelector;
import org.neo4j.cypherdsl.core.Skip;
import org.neo4j.cypherdsl.core.SortItem;
import org.neo4j.cypherdsl.core.Subquery;
import org.neo4j.cypherdsl.core.SubqueryExpression;
import org.neo4j.cypherdsl.core.SymbolicName;
import org.neo4j.cypherdsl.core.UnionPart;
import org.neo4j.cypherdsl.core.Unwind;
import org.neo4j.cypherdsl.core.Use;
import org.neo4j.cypherdsl.core.Where;
import org.neo4j.cypherdsl.core.With;
import org.neo4j.cypherdsl.core.internal.NameResolvingStrategy;
import org.neo4j.cypherdsl.core.internal.SchemaNamesBridge;
import org.neo4j.cypherdsl.core.ast.ProvidesAffixes;
import org.neo4j.cypherdsl.core.ast.TypedSubtree;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.internal.CaseElse;
import org.neo4j.cypherdsl.core.internal.CaseWhenThen;
import org.neo4j.cypherdsl.core.internal.ConstantParameterHolder;
import org.neo4j.cypherdsl.core.internal.Distinct;
import org.neo4j.cypherdsl.core.internal.LoadCSV;
import org.neo4j.cypherdsl.core.internal.Namespace;
import org.neo4j.cypherdsl.core.internal.ProcedureName;
import org.neo4j.cypherdsl.core.internal.ReflectiveVisitor;
import org.neo4j.cypherdsl.core.internal.RelationshipLength;
import org.neo4j.cypherdsl.core.internal.RelationshipPatternCondition;
import org.neo4j.cypherdsl.core.internal.RelationshipTypes;
import org.neo4j.cypherdsl.core.internal.ScopingStrategy;
import org.neo4j.cypherdsl.core.StatementContext;
import org.neo4j.cypherdsl.core.internal.UsingPeriodicCommit;
import org.neo4j.cypherdsl.core.internal.YieldItems;

/**
 * This is  a simple (some would  call it naive) implementation  of a visitor to  the Cypher AST created  by the Cypher
 * builder based on the {@link ReflectiveVisitor reflective visitor}.
 * <p>
 * It takes care of separating elements of subtrees containing  the element type with a separator and provides pairs of
 * {@code enter} / {@code leave} for the structuring elements of the Cypher AST as needed.
 * <p>
 * This rendering  visitor is not  meant to be  used outside framework code,  and we don't  give any guarantees  on the
 * format being output apart from that it works within the constraints of SDN-RX respectively SDN 6 and later.
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

	record SeparatorAndSupplier(AtomicReference<String> seperator, Supplier<String> supplier) {
	}

	/**
	 * This keeps track on which level of the tree a separator is needed.
	 */
	private final Map<Integer, SeparatorAndSupplier> separatorOnLevel = new ConcurrentHashMap<>();

	/**
	 * Keeps track of scoped, named variables.
	 */
	private final ScopingStrategy scopingStrategy;

	/**
	 * A set of aliased expressions that already have been seen and for which an alias must be used on each following
	 * appearance.
	 */
	protected final java.util.Set<AliasedExpression> visitableToAliased = new HashSet<>();

	/**
	 * Keeps track if currently in an aliased expression so that the content can be skipped when already visited.
	 */
	private final Deque<AliasedExpression> currentAliasedElements = new ArrayDeque<>();

	/**
	 * A cache of delegates, avoiding unnecessary object creation.
	 */
	private final Map<Class<? extends Visitor>, Visitor> delegateCache = new ConcurrentHashMap<>();

	private final NameResolvingStrategy nameResolvingStrategy;

	private final boolean enforceSchema;

	private final Map<String, List<Configuration.RelationshipDefinition>> relationshipDefinitions;

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

	private final Deque<Boolean> inPatternExpression = new ArrayDeque<>();

	/**
	 * Rendering parameters is not a config property due to some needs in Spring Data Neo4j: This needs to be configured
	 * per statement, not per config  there.
	 */
	private final boolean renderConstantsAsParameters;

	private final boolean alwaysEscapeNames;

	private final Dialect dialect;

	private boolean inEntity;
	private boolean inPropertyLookup;

	private Relationship.Direction directionOverride;

	DefaultVisitor(StatementContext statementContext) {
		this(statementContext, false);
	}

	DefaultVisitor(StatementContext statementContext, boolean renderConstantsAsParameters) {
		this(statementContext, renderConstantsAsParameters, Configuration.newConfig().alwaysEscapeNames(true).build());
	}

	boolean hasIdentifiables() {
		return !this.scopingStrategy.getIdentifiables().isEmpty();
	}

	DefaultVisitor(StatementContext statementContext, boolean renderConstantsAsParameters,
		Configuration configuration) {
		this.nameResolvingStrategy = configuration.isUseGeneratedNames() ?
			NameResolvingStrategy.useGeneratedNames(statementContext, configuration.getGeneratedNames()) :
			NameResolvingStrategy.useGivenNames(statementContext);

		this.scopingStrategy = ScopingStrategy.create(
			List.of(nameResolvingStrategy::enterScope),
			List.of(nameResolvingStrategy::leaveScope)
		);

		this.renderConstantsAsParameters = renderConstantsAsParameters;
		this.alwaysEscapeNames = configuration.isAlwaysEscapeNames();
		this.dialect = configuration.getDialect();
		this.enforceSchema = configuration.isEnforceSchema();
		this.relationshipDefinitions = configuration.getRelationshipDefinitions();
	}

	private void enableSeparator(int level, boolean on, Supplier<String> supplier) {
		if (on) {
			separatorOnLevel.put(level,
				new SeparatorAndSupplier(new AtomicReference<>(""), supplier == null ? () -> "" : supplier));
		} else {
			separatorOnLevel.remove(level);
		}
	}

	private Optional<SeparatorAndSupplier> separatorOnCurrentLevel() {

		return Optional.ofNullable(separatorOnLevel.get(currentLevel));
	}

	@Override
	protected boolean preEnter(Visitable visitable) {

		Visitable lastAliased = currentAliasedElements.peek();
		if (skipNodeContent || visitableToAliased.contains(lastAliased)) {
			return false;
		}

		if (visitable instanceof AliasedExpression aliasedExpression) {
			currentAliasedElements.push(aliasedExpression);
		}

		int nextLevel = ++currentLevel + 1;
		if (visitable instanceof TypedSubtree<?> ts) {
			enableSeparator(nextLevel, true, ts::separator);
		}

		separatorOnCurrentLevel().ifPresent(ref -> builder.append(ref.seperator().getAndSet("")));

		if (visitable instanceof ProvidesAffixes providesAffixes) {
			providesAffixes.getPrefix().ifPresent(this::doWithPrefix);
		}

		boolean doEnter = !skipNodeContent;
		if (doEnter) {
			scopingStrategy.doEnter(visitable);
		}
		return doEnter;
	}

	@Override
	protected final PreEnterResult getPreEnterResult(Visitable visitable) {
		boolean doEnter = preEnter(visitable);
		if (!doEnter) {
			return PreEnterResult.skip();
		}

		Class<? extends Visitor> handlerType = dialect.getHandler(visitable);
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
		} catch (NoSuchMethodException | InstantiationException | IllegalAccessException | InvocationTargetException e) {
			throw new IllegalArgumentException(
				dialect.name() + " has defined an illegal handler not providing a constructor accepting a delegate.");
		}
	}

	@Override
	protected void postLeave(Visitable visitable) {

		scopingStrategy.doLeave(visitable);

		separatorOnCurrentLevel().ifPresent(ref -> ref.seperator().set(ref.supplier().get()));

		if (visitable instanceof ProvidesAffixes providesAffixes) {
			providesAffixes.getSuffix().ifPresent(this::doWithSuffix);
		}

		if (visitable instanceof TypedSubtree) {
			enableSeparator(currentLevel + 1, false, null);
		}

		if (currentAliasedElements.peek() == visitable) {
			currentAliasedElements.pop();
		}

		if (visitable instanceof AliasedExpression aliasedExpression) {
			visitableToAliased.add(aliasedExpression);
		}

		--currentLevel;
	}

	protected void doWithPrefix(String prefix) {
		this.builder.append(prefix);
	}

	protected void doWithSuffix(String suffix) {
		this.builder.append(suffix);
	}

	void enter(Match match) {
		if (match.isOptional()) {
			builder.append("OPTIONAL ");
		}
		builder.append("MATCH ");
	}

	void leave(Match match) {

		builder.append(" ");
	}

	void enter(Where where) {
		builder.append(" WHERE ");
	}

	void enter(Create create) {
		builder.append("CREATE ");
	}

	void leave(Create create) {
		builder.append(" ");
	}

	void enter(Merge merge) {
		builder.append("MERGE ");
	}

	void leave(Merge merge) {
		if (!merge.hasEvents()) { // The last SET will include this
			builder.append(" ");
		}
	}

	void enter(MergeAction onCreateOrMatchEvent) {
		switch (onCreateOrMatchEvent.getType()) {
			case ON_CREATE -> builder.append("ON CREATE");
			case ON_MATCH -> builder.append("ON MATCH");
		}
		builder.append(" ");
	}

	void enter(Condition condition) {
		inRelationshipCondition = condition instanceof RelationshipPatternCondition;
	}

	void leave(Condition condition) {
		inRelationshipCondition = false;
	}

	void enter(Distinct distinct) {
		builder.append("DISTINCT ");
	}

	boolean inReturn;

	void enter(Return returning) {

		inReturn = true;
		if (!returning.isRaw()) {
			builder.append("RETURN ");
		}
	}

	void leave(Return returning) {
		inReturn = false;
	}

	void enter(With with) {
		builder.append("WITH ");
	}

	void leave(With with) {
		builder.append(" ");
	}

	void enter(Delete delete) {

		if (delete.isDetach()) {
			builder.append("DETACH ");
		}

		builder.append("DELETE ");
	}

	void leave(Delete match) {
		builder.append(" ");
	}

	boolean inLastReturn() {
		return inReturn && !inSubquery;
	}

	void enter(AliasedExpression aliased) {

		if (this.visitableToAliased.contains(aliased)) {
			builder.append(escapeIfNecessary(nameResolvingStrategy.resolve(aliased, false, inLastReturn())));
		}
	}

	void leave(AliasedExpression aliased) {

		if (!(this.visitableToAliased.contains(aliased) || scopingStrategy.isSkipAliasing())) {
			builder.append(" AS ").append(escapeIfNecessary(nameResolvingStrategy.resolve(aliased, true, inLastReturn())));
		}
	}

	void enter(NestedExpression nested) {
		builder.append("(");
	}

	void leave(NestedExpression nested) {
		builder.append(")");
	}

	void enter(Order order) {
		builder.append(" ORDER BY ");
	}

	void enter(Skip skip) {
		builder.append(" SKIP ");
	}

	void enter(Limit limit) {
		builder.append(" LIMIT ");
	}

	void enter(SortItem.Direction direction) {
		builder
			.append(" ")
			.append(direction.getSymbol());
	}

	void enter(PropertyLookup propertyLookup) {

		inPropertyLookup = true;
		if (propertyLookup.isDynamicLookup()) {
			builder.append("[");
		} else {
			builder.append(".");
		}
	}

	void leave(PropertyLookup propertyLookup) {

		inPropertyLookup = false;
		if (propertyLookup.isDynamicLookup()) {
			builder.append("]");
		}
	}

	void enter(FunctionInvocation functionInvocation) {
		String functionName = functionInvocation.getFunctionName();
		if ("elementId".equals(functionName)) {
			functionName = "toString(id";
		}
		builder
			.append(functionName)
			.append("(");
	}

	void leave(FunctionInvocation functionInvocation) {
		String functionName = functionInvocation.getFunctionName();
		if ("elementId".equals(functionName)) {
			builder.append(")");
		}
		builder.append(")");
	}

	void enter(Operation operation) {

		if (operation.needsGrouping()) {
			builder.append("(");
		}
	}

	void enter(Operator operator) {

		Operator.Type type = operator.getType();
		if (type == Operator.Type.LABEL) {
			return;
		}

		boolean skipSpaces = SKIP_SPACES.contains(operator);
		if (type != Operator.Type.PREFIX && !skipSpaces) {
			builder.append(" ");
		}
		builder.append(operator.getRepresentation());
		if (type != Operator.Type.POSTFIX && !skipSpaces) {
			builder.append(" ");
		}
	}

	void leave(Operation operation) {

		if (operation.needsGrouping()) {
			builder.append(")");
		}
	}

	void enter(Literal<?> expression) {
		builder.append(expression.asString());
	}

	void enter(Node node) {

		builder.append("(");

		// This is only relevant for nodes in relationships.
		// Otherwise, all the labels would be rendered again.
		skipNodeContent = scopingStrategy.hasVisitedBefore(node);

		if (skipNodeContent) {
			builder.append(nameResolvingStrategy.resolve(
				node.getSymbolicName().orElseGet(node::getRequiredSymbolicName), true, false));
		}

		inEntity = true;
	}

	void leave(Node node) {

		builder.append(")");

		skipNodeContent = false;
		inEntity = false;
	}

	void enter(NodeLabel nodeLabel) {

		escapeName(nodeLabel.getValue()).ifPresent(label -> builder.append(Symbols.NODE_LABEL_START).append(label));
	}

	void enter(LabelExpression labelExpression) {
		builder.append(":");
		renderLabelExpression(labelExpression, null);
	}

	@SuppressWarnings("squid:S3776")
	void renderLabelExpression(LabelExpression l, LabelExpression.Type parent) {
		if (l == null) {
			return;
		}
		if (l.negated()) {
			builder.append("!");
		}
		var current = l.type();
		boolean close = false;
		if (current != LabelExpression.Type.LEAF) {
			close = (parent != null || l.negated()) && l.type() != parent;
			if (close && !l.negated() && (current == LabelExpression.Type.CONJUNCTION || parent == LabelExpression.Type.DISJUNCTION)) {
				close = false;
			}
		}
		if (close) {
			builder.append("(");
		}
		renderLabelExpression(l.lhs(), current);
		if (current == LabelExpression.Type.LEAF) {
			l.value().forEach(v ->
				escapeName(v).ifPresent(builder::append)
			);
		}  else {
			builder.append(current.getValue());
		}
		renderLabelExpression(l.rhs(), current);
		if (close) {
			builder.append(")");
		}
	}

	void enter(Properties properties) {

		builder.append(" ");
	}

	void enter(SymbolicName symbolicName) {
		if (!inRelationshipCondition || nameResolvingStrategy.isResolved(symbolicName)) {
			if (Boolean.TRUE.equals(inPatternExpression.peek()) && !scopingStrategy.hasVisitedBefore(new Named() {
				@Override
				public @NotNull Optional<SymbolicName> getSymbolicName() {
					return Optional.of(symbolicName);
				}
			})) {
				return;
			}

			builder.append(nameResolvingStrategy.resolve(symbolicName, inEntity, inPropertyLookup));
		}
	}

	void enter(PatternExpression p) {
		this.inPatternExpression.push(true);
	}

	void leave(PatternExpression p) {
		this.inPatternExpression.pop();
	}

	void enter(Relationship relationship) {
		skipRelationshipContent = scopingStrategy.hasVisitedBefore(relationship);
		if (enforceSchema && relationship.getDetails().getDirection() != Relationship.Direction.UNI) {
			directionOverride = computeDirectionOverride(relationship);
		}
	}

	/**
	 * Helper to retrieve a nodes label
	 *
	 * @param node the node
	 * @return A set of labels
	 */
	private java.util.Set<String> getLabels(Node node) {
		var nl = node.getLabels();
		if (nl.isEmpty()) {
			var patternElement = scopingStrategy.lookup(node);
			if (patternElement instanceof Node boundNode) {
				nl = boundNode.getLabels();
			}
		}
		return nl.stream().map(NodeLabel::getValue).collect(Collectors.toSet());
	}

	/**
	 * Computes an overwrite for enforcing a schema
	 * @param relationship the relationship to potentially override
	 * @return A new direction
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
			outer:
			if (relationshipDefinitions.containsKey(type)) {
				var knownRelationships = relationshipDefinitions.get(type).stream().toList();
				for (var knownRelationship : knownRelationships) {
					if (knownRelationship.selfReferential() && (sourceLabels.isEmpty() || targetLabels.isEmpty())) {
						break outer;
					}
					if (
						sourceLabels.contains(knownRelationship.targetLabel()) && (targetLabels.isEmpty() || targetLabels.contains(knownRelationship.sourceLabel())) ||
						targetLabels.contains(knownRelationship.sourceLabel()) && (sourceLabels.isEmpty() || sourceLabels.contains(knownRelationship.sourceLabel()))
					) {
						return Relationship.Direction.RTL;
					} else if (
						sourceLabels.contains(knownRelationship.sourceLabel()) && (targetLabels.isEmpty() || targetLabels.contains(knownRelationship.targetLabel())) ||
						targetLabels.contains(knownRelationship.targetLabel()) && (sourceLabels.isEmpty() || sourceLabels.contains(knownRelationship.sourceLabel()))
					) {
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
				if (sourceLabels.contains(knownRelationship.targetLabel()) && targetLabels.contains(knownRelationship.sourceLabel())) {
					return Relationship.Direction.RTL;
				} else if (sourceLabels.contains(knownRelationship.sourceLabel()) && targetLabels.contains(knownRelationship.targetLabel())) {
					return Relationship.Direction.LTR;
				}
			}
		}

		return details.getDirection();
	}

	void enter(Relationship.Details details) {

		Relationship.Direction direction = Optional.ofNullable(directionOverride).orElseGet(details::getDirection);
		builder.append(direction.getSymbolLeft());
		if (details.hasContent()) {
			builder.append("[");
		}

		inEntity = true;
	}

	void enter(RelationshipTypes types) {

		if (skipRelationshipContent) {
			return;
		}

		builder
			.append(types.getValues().stream()
				.map(this::escapeName)
				.map(Optional::orElseThrow)
				.collect(Collectors.joining(Symbols.REL_TYP_SEPARATOR, Symbols.REL_TYPE_START, "")));
	}

	void enter(RelationshipLength length) {

		if (skipRelationshipContent) {
			return;
		}

		Integer minimum = length.getMinimum();
		Integer maximum = length.getMaximum();

		if (length.isUnbounded()) {
			builder.append("*");
			return;
		}

		if (minimum == null && maximum == null) {
			return;
		}

		builder.append("*");
		if (minimum != null) {
			builder.append(minimum);
		}
		builder.append("..");
		if (maximum != null) {
			builder.append(maximum);
		}
	}

	void leave(Relationship.Details details) {

		Relationship.Direction direction = Optional.ofNullable(directionOverride).orElseGet(details::getDirection);
		if (details.hasContent()) {
			builder.append("]");
		}
		builder.append(direction.getSymbolRight());

		inEntity = false;
	}

	void leave(Relationship relationship) {

		skipRelationshipContent = false;
		directionOverride = null;
	}

	void enter(Parameter<?> parameter) {

		Object value = parameter.getValue();
		if (value instanceof ConstantParameterHolder constantParameterHolder && !renderConstantsAsParameters) {
			builder.append(constantParameterHolder.asString());
		} else {
			builder.append("$").append(nameResolvingStrategy.resolve(parameter));
		}
	}

	void enter(MapExpression map) {

		builder.append("{");
	}

	void enter(KeyValueMapEntry map) {

		builder.append(escapeIfNecessary(map.getKey())).append(": ");
	}

	void leave(MapExpression map) {

		builder.append("}");
	}

	void enter(ListExpression list) {

		builder.append("[");
	}

	void leave(ListExpression list) {

		builder.append("]");
	}

	void enter(Unwind unwind) {

		builder.append("UNWIND ");
	}

	void leave(Unwind unwind) {

		builder
			.append(" ");
	}

	void enter(UnionPart unionPart) {

		builder.append(" UNION ");
		if (unionPart.isAll()) {
			builder.append("ALL ");
		}
	}

	void enter(Set set) {

		builder.append("SET ");
	}

	void leave(Set set) {
		builder.append(" ");
	}

	void enter(Remove remove) {

		builder.append("REMOVE ");
	}

	void leave(Remove remove) {
		builder.append(" ");
	}

	void enter(PatternComprehension patternComprehension) {
		builder.append("[");
	}

	void leave(PatternComprehension patternComprehension) {
		builder.append("]");
	}

	void enter(ListComprehension listComprehension) {
		builder.append("[");
	}

	void leave(ListComprehension listComprehension) {
		builder.append("]");
	}

	void enter(Case genericCase) {
		builder.append("CASE");
	}

	void enter(Case.SimpleCase simpleCase) {
		builder.append("CASE ");
	}

	void enter(CaseWhenThen caseWhenExpression) {
		builder.append(" WHEN ");
	}

	void leave(CaseWhenThen caseWhenExpression) {
		builder.append(" THEN ");
	}

	void enter(CaseElse caseElseExpression) {
		builder.append(" ELSE ");
	}

	void leave(Case caseExpression) {
		builder.append(" END");
	}

	void enter(ProcedureCall procedureCall) {

		builder.append("CALL ");
	}

	void leave(Namespace namespace) {

		builder.append(".");
	}

	void leave(ProcedureName procedureName) {

		builder.append(procedureName.getValue());
	}

	void enter(YieldItems yieldItems) {

		builder.append(" YIELD ");
	}

	void leave(ProcedureCall procedureCall) {

		builder.append(" ");
	}

	void enter(Enum<?> anEnum) {

		builder.append(anEnum.name().replace("_", " ")).append(" ");
	}

	boolean inSubquery;
	void enter(Subquery subquery) {

		this.inSubquery = true;
		builder.append("CALL {");
	}

	void leave(Subquery subquery) {

		this.inSubquery = false;
		int l = builder.length() - 1;
		if (builder.charAt(l) == ' ' && !subquery.doesReturnOrYield()) {
			builder.replace(l, builder.length(), "} ");
		} else {
			builder.append("} ");
		}
	}

	void leave(InTransactions inTransactions) {

		int l = builder.length() - 1;
		if (builder.charAt(l) != ' ') {
			builder.append(" ");
		}
		builder.append("IN TRANSACTIONS ");
		if (inTransactions.getRows() != null) {
			builder.append("OF ").append(inTransactions.getRows()).append(" ROWS ");
		}
	}

	void enter(Foreach foreach) {

		builder.append("FOREACH (");
	}

	void leave(Foreach foreach) {

		builder.setCharAt(builder.length() - 1, ')'); // replace trailing space with ')'
		builder.append(" ");
	}

	void enter(SubqueryExpression subquery) {

		if (subquery instanceof CountExpression) {
			builder.append("COUNT");
		} else if (subquery instanceof ExistentialSubquery) {
			builder.append("EXISTS");
		} else if (subquery instanceof CollectExpression) {
			builder.append("COLLECT");
		}
		builder.append(" { ");
	}

	void leave(SubqueryExpression subquery) {

		// Trimming the inner match without having to do this in the match (looking up if inside subquery).
		if (builder.charAt(builder.length() - 1) == ' ') {
			builder.replace(builder.length() - 1, builder.length(), " }");
		} else {
			builder.append(" }");
		}
	}

	void enter(Hint hint) {

		builder.append(" USING ");
	}

	void enter(LoadCSV loadCSV) {

		builder.append("LOAD CSV");
		if (loadCSV.isWithHeaders()) {
			builder.append(" WITH HEADERS");
		}
		builder.append(" FROM '")
			.append(loadCSV.getUri().toString())
			.append("' AS ")
			.append(loadCSV.getAlias());

		if (loadCSV.getFieldTerminator() != null) {
			builder.append(" FIELDTERMINATOR '")
				.append(loadCSV.getFieldTerminator())
				.append("'");
		}
		builder.append(" ");
	}

	void enter(UsingPeriodicCommit usingPeriodicCommit) {

		builder.append("USING PERIODIC COMMIT ");
		if (usingPeriodicCommit.rate() != null) {
			builder.append(usingPeriodicCommit.rate()).append(" ");
		}
	}

	void enter(Use use) {
		builder.append("USE ");
		if (use.dynamic()) {
			builder.append("graph.byName(");
		}
	}

	void leave(Use use) {
		if (use.dynamic()) {
			builder.append(")");
		}
		builder.append(" ");
	}

	void enter(PatternSelector shortest) {

		if (shortest instanceof PatternSelector.ShortestK shortestK) {
			builder.append("SHORTEST ").append(shortestK.getK());
		} else if (shortest instanceof PatternSelector.ShortestKGroups shortestK) {
			builder.append("SHORTEST ").append(shortestK.getK()).append(" GROUPS");
		} else if (shortest instanceof PatternSelector.AllShortest) {
			builder.append("ALL SHORTEST");
		} else if (shortest instanceof PatternSelector.Any) {
			builder.append("ANY");
		}
	}

	void leave(PatternSelector shortest) {
		builder.append(" ");
	}

	void enter(QuantifiedPathPattern.TargetPattern qpp) {
		builder.append("(");
	}

	void leave(QuantifiedPathPattern.TargetPattern qpp) {
		builder.append(")");
	}

	void enter(QuantifiedPathPattern.Quantifier quantifier) {

		builder.append(quantifier.toString());
	}

	@Override
	public String getRenderedContent() {
		return this.builder.toString();
	}

	/**
	 * Escapes a symbolic name. Such a symbolic name is either used for a nodes label, the type of a relationship or a
	 * variable.
	 *
	 * @param unescapedName The name to escape.
	 * @return An empty optional when the unescaped name is {@literal null}, otherwise the correctly escaped name, safe to be used in statements.
	 */
	protected final Optional<String> escapeName(String unescapedName) {

		return SchemaNamesBridge.sanitize(unescapedName, alwaysEscapeNames);
	}

	protected final String escapeIfNecessary(String potentiallyNonIdentifier) {

		return SchemaNamesBridge.sanitize(potentiallyNonIdentifier, false).orElse(null);
	}
}
