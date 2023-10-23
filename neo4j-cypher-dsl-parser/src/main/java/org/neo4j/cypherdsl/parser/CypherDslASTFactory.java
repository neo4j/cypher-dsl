/*
 * Copyright (c) "Neo4j"
 * Neo4j Sweden AB [http://neo4j.com]
 *
 * This file is part of Neo4j.
 *
 * Neo4j is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.neo4j.cypherdsl.parser;

import static org.apiguardian.api.API.Status.INTERNAL;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.function.UnaryOperator;
import java.util.stream.Stream;

import org.apiguardian.api.API;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.neo4j.cypher.internal.ast.factory.ASTFactory;
import org.neo4j.cypher.internal.ast.factory.ASTFactory.NULL;
import org.neo4j.cypher.internal.ast.factory.AccessType;
import org.neo4j.cypher.internal.ast.factory.ActionType;
import org.neo4j.cypher.internal.ast.factory.CallInTxsOnErrorBehaviourType;
import org.neo4j.cypher.internal.ast.factory.ConstraintType;
import org.neo4j.cypher.internal.ast.factory.ConstraintVersion;
import org.neo4j.cypher.internal.ast.factory.CreateIndexTypes;
import org.neo4j.cypher.internal.ast.factory.HintIndexType;
import org.neo4j.cypher.internal.ast.factory.ParameterType;
import org.neo4j.cypher.internal.ast.factory.ParserCypherTypeName;
import org.neo4j.cypher.internal.ast.factory.ScopeType;
import org.neo4j.cypher.internal.ast.factory.ShowCommandFilterTypes;
import org.neo4j.cypher.internal.ast.factory.SimpleEither;
import org.neo4j.cypherdsl.core.Case;
import org.neo4j.cypherdsl.core.Clause;
import org.neo4j.cypherdsl.core.Clauses;
import org.neo4j.cypherdsl.core.Condition;
import org.neo4j.cypherdsl.core.Conditions;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.ExposesPatternLengthAccessors;
import org.neo4j.cypherdsl.core.ExposesProperties;
import org.neo4j.cypherdsl.core.ExposesRelationships;
import org.neo4j.cypherdsl.core.Expression;
import org.neo4j.cypherdsl.core.Expressions;
import org.neo4j.cypherdsl.core.FunctionInvocation;
import org.neo4j.cypherdsl.core.Functions;
import org.neo4j.cypherdsl.core.Hint;
import org.neo4j.cypherdsl.core.KeyValueMapEntry;
import org.neo4j.cypherdsl.core.LabelExpression;
import org.neo4j.cypherdsl.core.MapExpression;
import org.neo4j.cypherdsl.core.MapProjection;
import org.neo4j.cypherdsl.core.Match;
import org.neo4j.cypherdsl.core.MergeAction;
import org.neo4j.cypherdsl.core.NamedPath;
import org.neo4j.cypherdsl.core.Node;
import org.neo4j.cypherdsl.core.Operation;
import org.neo4j.cypherdsl.core.Operations;
import org.neo4j.cypherdsl.core.Parameter;
import org.neo4j.cypherdsl.core.ParenthesizedPathPattern;
import org.neo4j.cypherdsl.core.PatternComprehension;
import org.neo4j.cypherdsl.core.PatternElement;
import org.neo4j.cypherdsl.core.Predicates;
import org.neo4j.cypherdsl.core.Property;
import org.neo4j.cypherdsl.core.PropertyLookup;
import org.neo4j.cypherdsl.core.Relationship;
import org.neo4j.cypherdsl.core.RelationshipChain;
import org.neo4j.cypherdsl.core.RelationshipPattern;
import org.neo4j.cypherdsl.core.Return;
import org.neo4j.cypherdsl.core.Set;
import org.neo4j.cypherdsl.core.SortItem;
import org.neo4j.cypherdsl.core.Statement;
import org.neo4j.cypherdsl.core.StatementBuilder;
import org.neo4j.cypherdsl.core.StringLiteral;
import org.neo4j.cypherdsl.core.SymbolicName;
import org.neo4j.cypherdsl.core.Where;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;

/**
 * An implementation of Neo4j's {@link ASTFactory} that creates Cypher-DSL AST elements that can be used for creating
 * conditions, patterns to match etc.
 *
 * @author Michael J. Simons
 * @since 2021.3.0
 */
@API(status = INTERNAL, since = "2021.3.0")
final class CypherDslASTFactory implements ASTFactory<
		Statement,
		Statement,
		Clause,
		Return,
		Expression,
		List<Expression>,
		SortItem,
		PatternElement,
		NodeAtom,
		PathAtom,
		PathLength,
		Clause,
		Expression,
		Expression,
		Expression,
		Hint,
		Expression,
		LabelExpression,
		Parameter<?>,
		Expression,
		Property,
		Expression,
		Clause,
		Statement,
		Statement,
		Statement,
		Clause,
		Where,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		InputPosition,
		EntityType,
		NULL,
		PatternAtom,
		DatabaseName,
		NULL,
		NULL,
		PatternElement> {

	private static CypherDslASTFactory instanceFromDefaultOptions;

	static CypherDslASTFactory getInstance(Options options) {

		CypherDslASTFactory instance;

		if (options != null && !options.areDefault()) {
			instance = new CypherDslASTFactory(options);
		} else {
			instance = instanceFromDefaultOptions;
			if (instance == null) {
				synchronized (CypherDslASTFactory.class) {
					instance = instanceFromDefaultOptions;
					if (instance == null) {
						instanceFromDefaultOptions = new CypherDslASTFactory(Optional.ofNullable(options).orElseGet(Options::defaultOptions));
						instance = instanceFromDefaultOptions;
					}
				}
			}
		}

		return instance;
	}

	private final Options options;

	private CypherDslASTFactory(Options options) {
		this.options = options;
	}

	private String[] computeFinalLabelList(LabelParsedEventType event, List<StringPos<InputPosition>> inputLabels) {

		return inputLabels == null ? new String[0] : this.options.getLabelFilter()
			.apply(event, inputLabels.stream().map(v -> v.string).toList())
			.toArray(new String[0]);
	}

	private Optional<String[]> computeFinalLabelList(LabelParsedEventType event, LabelExpression inputLabels) {

		if (inputLabels == null) {
			return Optional.of(new String[0]);
		}

		if (inputLabels.type() == LabelExpression.Type.COLON_CONJUNCTION || (inputLabels.type() == LabelExpression.Type.LEAF && inputLabels.value() != null)) {
			return Optional.of(this.options.getLabelFilter()
				.apply(event, inputLabels.value())
				.toArray(new String[0]));
		}
		return Optional.empty();
	}

	private String[] computeFinalTypeList(TypeParsedEventType event, LabelExpression inputTypes) {

		if (inputTypes == null) {
			return new String[0];
		}

		if ((inputTypes.negated() && inputTypes.value().size() > 1) || inputTypes.type() == LabelExpression.Type.CONJUNCTION) {
			throw new UnsupportedOperationException("Expressions for relationship types are not supported in Cypher-DSL");
		}

		List<String> types = new ArrayList<>();
			traverseTypeExpression(types, inputTypes);

		return this.options.getTypeFilter()
			.apply(event, types)
			.toArray(new String[0]);
	}

	void traverseTypeExpression(List<String> types, LabelExpression expression) {

		if (expression.type() == LabelExpression.Type.LEAF || expression.type() == LabelExpression.Type.COLON_DISJUNCTION) {
			types.addAll(expression.value());
		} else {
			traverseTypeExpression(types, expression.lhs());
			traverseTypeExpression(types, expression.rhs());
		}
	}

	static void isInstanceOf(Class<?> type, Object obj, String message) {
		if (type == null) {
			throw new IllegalArgumentException("Type to check against must not be null");
		}
		if (!type.isInstance(obj)) {
			throw new IllegalArgumentException(message);
		}
	}

	private static void notNull(Object object, String message) {
		if (object == null) {
			throw new IllegalArgumentException(message);
		}
	}

	private <T extends Expression> T applyCallbacksFor(ExpressionCreatedEventType type, T newExpression) {
		return applyCallbacksFor(type, List.of(newExpression)).get(0);
	}

	@SuppressWarnings("unchecked")
	private <T extends Expression> List<T> applyCallbacksFor(ExpressionCreatedEventType type, List<T> expressions) {

		var callbacks = this.options.getOnNewExpressionCallbacks().getOrDefault(type, List.of());
		if (callbacks.isEmpty()) {
			return expressions;
		}

		var chainedCallbacks = callbacks.stream().reduce(Function.identity(), Function::andThen);
		return expressions.stream().map(e -> (T) chainedCallbacks.apply(e)).toList();
	}

	@SuppressWarnings("unchecked")
	private <T extends Visitable> T applyCallbacksFor(InvocationCreatedEventType type, T newExpression) {

		var callbacks = this.options.getOnNewInvocationCallbacks().getOrDefault(type, List.of());
		if (callbacks.isEmpty()) {
			return newExpression;
		}

		Visitable result = newExpression;
		for (UnaryOperator<Visitable> callback : callbacks) {
			result = callback.apply(result);
		}
		return (T) result;
	}

	private static SymbolicName assertSymbolicName(@Nullable Expression v) {

		if (v == null) {
			return null;
		}

		isInstanceOf(SymbolicName.class, v,  "An invalid type has been generated where a SymbolicName was required. Generated type was " + v.getClass().getName());
		return (SymbolicName) v;
	}

	@Override
	public Statement newSingleQuery(InputPosition p, List<Clause> clauses) {
		return newSingleQuery(clauses);
	}

	@Override
	public Statement newSingleQuery(List<Clause> clauses) {
		return Statement.of(clauses);
	}

	@Override
	public Statement newUnion(InputPosition p, Statement lhs, Statement rhs, boolean all) {
		if (all) {
			return Cypher.unionAll(lhs, rhs);
		} else {
			return Cypher.union(lhs, rhs);
		}
	}

	@Override
	public Clause useClause(InputPosition p, Expression e) {
		throw new UnsupportedOperationException();
	}

	public List<Expression> newReturnItems(InputPosition p, boolean returnAll, List<Expression> returnItems) {
		var finalReturnItems = returnItems;

		if (returnAll) {
			finalReturnItems = Stream.concat(Stream.of(Cypher.asterisk()), finalReturnItems.stream()).toList();
		}

		if (finalReturnItems.isEmpty()) {
			if (!returnAll) {
				throw new IllegalArgumentException("Cannot return nothing.");
			}
			finalReturnItems = Collections.singletonList(Cypher.asterisk());
		}
		return finalReturnItems;
	}

	@Override
	public Return newReturnClause(InputPosition p, boolean distinct, List<Expression> returnItems, List<SortItem> sortItems,
		InputPosition orderPos, Expression skip, InputPosition skipPosition, Expression limit,
		InputPosition limitPosition) {

		return options.getReturnClauseFactory().apply(new ReturnDefinition(distinct, returnItems, sortItems, skip, limit));
	}

	@Override
	public Expression newReturnItem(InputPosition p, Expression e, Expression v) {

		var s = assertSymbolicName(v);
		return applyCallbacksFor(ExpressionCreatedEventType.ON_RETURN_ITEM, e.as(s));
	}

	@Override
	public Expression newReturnItem(InputPosition p, Expression e, int eStartOffset, int eEndOffset) {
		return applyCallbacksFor(ExpressionCreatedEventType.ON_RETURN_ITEM, e);
	}

	@Override
	public SortItem orderDesc(InputPosition p, Expression e) {
		return e.descending();
	}

	@Override
	public SortItem orderAsc(InputPosition p, Expression e) {
		return e.ascending();
	}

	@Override
	public Clause withClause(InputPosition p, Return returnClause, Where where) {
		return Clauses.with(returnClause, where);
	}

	@Override
	public Clause matchClause(InputPosition p, boolean optional, NULL matchMode, List<PatternElement> patternElements, InputPosition patternPos, List<Hint> hints, Where whereIn) {

		var patternElementCallbacks = this.options.getOnNewPatternElementCallbacks().getOrDefault(PatternElementCreatedEventType.ON_MATCH, List.of());
		Condition condition = Conditions.noCondition();
		List<PatternElement> openForTransformation = new ArrayList<>();
		for (PatternElement patternElement : patternElements) {
			if (patternElement instanceof NodeAtom nodeAtom) {
				openForTransformation.add(nodeAtom.value());
				if (nodeAtom.predicate() != null) {
					condition = condition.and(nodeAtom.predicate().asCondition());
				}
			} else {
				openForTransformation.add(patternElement);
			}
		}
		var transformedPatternElements = transformIfPossible(patternElementCallbacks, openForTransformation);
		Where where = computeFinalWhere(whereIn, condition);

		return options.getMatchClauseFactory().apply(new MatchDefinition(optional, transformedPatternElements, where, hints));
	}

	private static Where computeFinalWhere(Where whereIn, Condition condition) {

		if (condition == Conditions.noCondition()) {
			return whereIn;
		}
		if (whereIn == null) {
			return Where.from(condition);
		}

		AtomicReference<Condition> capturedCondition = new AtomicReference<>();
		whereIn.accept(segment -> {
			if (segment instanceof Condition inner) {
				capturedCondition.compareAndSet(null, inner);
			}
		});
		return Where.from(capturedCondition.get().and(condition));
	}

	private List<PatternElement> transformIfPossible(List<UnaryOperator<PatternElement>> callbacks,
		List<PatternElement> patternElements) {
		if (callbacks.isEmpty()) {
			return patternElements;
		}

		@SuppressWarnings("squid:S4276") // The function is needed due to the assigment below
		var transformer = Function.<PatternElement>identity();
		for (UnaryOperator<PatternElement> callback : callbacks) {
			transformer = transformer.andThen(callback);
		}
		return patternElements.stream().map(transformer)
			.filter(Objects::nonNull)
			.toList();
	}

	@Override
	public Hint usingIndexHint(InputPosition p, Expression v, String labelOrRelType, List<String> properties,
		boolean seekOnly, HintIndexType indexType) {

		// We build nodes here. As of now, the node isn't used anyway, but only the label
		// will be used down further the AST.
		// It is easier than introduce a new common abstraction of label and relationship type (probably
		// in line with the decision made for the parser)
		var node = Cypher.node(labelOrRelType).named(assertSymbolicName(v));
		return Hint.useIndexFor(seekOnly, properties.stream().map(node::property).toArray(Property[]::new));
	}

	@Override
	public Hint usingJoin(InputPosition p, List<Expression> joinVariables) {

		return Hint.useJoinOn(joinVariables.stream().map(CypherDslASTFactory::assertSymbolicName).toArray(SymbolicName[]::new));
	}

	@Override
	public Hint usingScan(InputPosition p, Expression v, String label) {

		var s = assertSymbolicName(v);
		// Same note applies as with usingIndexHint in regard to relationships
		return Hint.useScanFor(Cypher.node(label).named(s));
	}

	@Override
	public Clause createClause(InputPosition p, List<PatternElement> patternElements) {

		var callbacks = this.options.getOnNewPatternElementCallbacks().getOrDefault(PatternElementCreatedEventType.ON_CREATE, List.of());
		return Clauses.create(transformIfPossible(callbacks, patternElements.stream().map(v -> v instanceof NodeAtom n ? n.value() : v).toList()));
	}

	@Override
	public Clause setClause(InputPosition p, List<Expression> setItems) {
		return Clauses.set(setItems);
	}

	@Override
	public Operation setProperty(Property property, Expression value) {
		return applyCallbacksFor(ExpressionCreatedEventType.ON_SET_PROPERTY, property.to(value));
	}

	@Override
	public Operation setVariable(Expression v, Expression value) {

		return applyCallbacksFor(ExpressionCreatedEventType.ON_SET_VARIABLE, Operations.set(v, value));
	}

	@Override
	public Operation addAndSetVariable(Expression v, Expression value) {
		return applyCallbacksFor(ExpressionCreatedEventType.ON_ADD_AND_SET_VARIABLE, Operations.mutate(v, value));
	}

	@Override
	public Operation setLabels(Expression v, List<StringPos<InputPosition>> values) {

		var s = assertSymbolicName(v);
		var labels = computeFinalLabelList(LabelParsedEventType.ON_SET, values);
		return applyCallbacksFor(ExpressionCreatedEventType.ON_SET_LABELS, Operations.set(Cypher.anyNode(s), labels));
	}

	@Override
	public Clause removeClause(InputPosition p, List<Expression> removeItems) {
		return Clauses.remove(removeItems);
	}

	@Override
	public Expression removeProperty(Property property) {
		return applyCallbacksFor(ExpressionCreatedEventType.ON_REMOVE_PROPERTY, property);
	}

	@Override
	public Expression removeLabels(Expression v, List<StringPos<InputPosition>> values) {

		var s = assertSymbolicName(v);
		var labels = computeFinalLabelList(LabelParsedEventType.ON_REMOVE, values);
		return applyCallbacksFor(ExpressionCreatedEventType.ON_REMOVE_LABELS, Operations.remove(Cypher.anyNode(s), labels));
	}

	@Override
	public Clause deleteClause(InputPosition p, boolean detach, List<Expression> expressions) {
		return Clauses.delete(detach, applyCallbacksFor(ExpressionCreatedEventType.ON_DELETE_ITEM, expressions));
	}

	@Override
	public Clause unwindClause(InputPosition p, Expression e, Expression v) {
		return Clauses.unwind(e, assertSymbolicName(v));
	}

	@Override
	public Clause mergeClause(InputPosition p, PatternElement patternElementIn, List<Clause> setClauses,
		List<MergeActionType> actionTypes, List<InputPosition> positions) {


		var patternElement = patternElementIn instanceof NodeAtom n ? n.value() : patternElementIn;
		var mergeActions = new ArrayList<MergeAction>();
		if (setClauses != null && !setClauses.isEmpty() && actionTypes != null && !actionTypes.isEmpty()) {

			var iteratorClauses = setClauses.iterator();
			var iteratorTypes = actionTypes.iterator();
			while (iteratorClauses.hasNext() && iteratorTypes.hasNext()) {
				var type = iteratorTypes.next();
				switch (type) {
					case OnCreate ->
						mergeActions.add(MergeAction.of(MergeAction.Type.ON_CREATE, (Set) iteratorClauses.next()));
					case OnMatch ->
						mergeActions.add(MergeAction.of(MergeAction.Type.ON_MATCH, (Set) iteratorClauses.next()));
					default -> throw new IllegalArgumentException("Unsupported MergeActionType: " + type);
				}
			}
		}

		var callbacks = this.options.getOnNewPatternElementCallbacks().getOrDefault(PatternElementCreatedEventType.ON_MERGE, List.of());
		return Clauses.merge(transformIfPossible(callbacks, List.of(patternElement)), mergeActions);
	}

	@Override
	public Clause callClause(InputPosition p, InputPosition namespacePosition, InputPosition procedureNamePosition,
		InputPosition procedureResultPosition, List<String> namespace, String name, List<Expression> arguments,
		boolean yieldAll, List<Expression> resultItems, Where where) {
		var intermediateResult = Clauses.callClause(namespace, name, arguments,
			yieldAll && resultItems == null ? List.of(Cypher.asterisk()) : resultItems, where);
		return applyCallbacksFor(InvocationCreatedEventType.ON_CALL, intermediateResult);
	}

	@Override
	public Expression callResultItem(InputPosition p, String name, Expression alias) {
		var finalName = Cypher.name(name);
		if (alias != null) {
			return finalName.as(assertSymbolicName(alias));
		}
		return finalName;
	}

	@Override
	public PatternElement patternWithSelector(NULL aNull, PatternElement patternPart) {
		return null;
	}

	@Override
	public PatternElement namedPattern(Expression v, PatternElement patternElement) {
		return Cypher.path(assertSymbolicName(v)).definedBy(patternElement);
	}

	@Override
	public PatternElement shortestPathPattern(InputPosition p, PatternElement patternElement) {

		isInstanceOf(RelationshipPattern.class, patternElement,
			"Only relationship patterns are supported for the shortestPath function.");

		return new ExpressionAsPatternElementWrapper(
			FunctionInvocation.create(PatternElementFunctions.SHORTEST_PATH, patternElement));
	}

	@Override
	public PatternElement allShortestPathsPattern(InputPosition p, PatternElement patternElement) {

		isInstanceOf(RelationshipPattern.class, patternElement,
			"Only relationship patterns are supported for the allShortestPaths function.");

		return new ExpressionAsPatternElementWrapper(
			FunctionInvocation.create(PatternElementFunctions.ALL_SHORTEST_PATHS, patternElement));
	}

	@Override
	public PatternElement pathPattern(PatternElement patternElement) {
		return patternElement;
	}

	@Override
	public PatternElement patternElement(List<PatternAtom> atoms) {

		if (atoms.isEmpty()) {
			throw new IllegalArgumentException(
				"Cannot create a PatternElement from an empty list of patterns.");
		}

		if (atoms.size() == 1 && atoms.get(0) instanceof ParenthesizedPathPatternAtom atom) {
			return ParenthesizedPathPattern.of(atom.getPatternElement());
		}

		var nodes = atoms.stream()
			.filter(NodeAtom.class::isInstance).map(NodeAtom.class::cast).toList();
		var relationships = atoms.stream()
			.filter(PathAtom.class::isInstance).map(PathAtom.class::cast).toList();

		if (nodes.size() == 1 && relationships.isEmpty()) {
			return nodes.get(0);
		}

		if (nodes.size() != relationships.size() + 1) {
			throw new IllegalArgumentException(
				"Something weird has happened. Got " + nodes.size() + " nodes and " + relationships.size()
				+ " path details.");
		}

		ExposesRelationships<?> relationshipPattern = nodes.get(0).value();
		for (int i = 1; i < nodes.size(); i++) {
			Node node = nodes.get(i).value();
			PathAtom pathAtom = relationships.get(i - 1);
			PathLength length = pathAtom.getLength();

			relationshipPattern = switch (pathAtom.getDirection()) {
				case LTR -> relationshipPattern.relationshipTo(node, pathAtom.getTypes());
				case RTL -> relationshipPattern.relationshipFrom(node, pathAtom.getTypes());
				case UNI -> relationshipPattern.relationshipBetween(node, pathAtom.getTypes());
			};

			relationshipPattern = applyOptionalName(relationshipPattern, pathAtom);

			relationshipPattern = applyOptionalProperties(relationshipPattern, pathAtom);

			relationshipPattern = applyOptionalLength(relationshipPattern, length);
		}

		return (PatternElement) relationshipPattern;
	}

	@Override
	public NULL anyPathSelector(String count, InputPosition countPosition, InputPosition position) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL allPathSelector(InputPosition position) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL anyShortestPathSelector(String count, InputPosition countPosition, InputPosition position) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL allShortestPathSelector(InputPosition position) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL shortestGroupsSelector(String count, InputPosition countPosition, InputPosition position) {
		throw new UnsupportedOperationException();
	}

	private static ExposesRelationships<?> applyOptionalLength(ExposesRelationships<?> relationshipPattern, PathLength length) {
		if (length == null) {
			return relationshipPattern;
		}
		if (length.isUnbounded()) {
			return ((ExposesPatternLengthAccessors<?>) relationshipPattern).unbounded();
		}
		return ((ExposesPatternLengthAccessors<?>) relationshipPattern).length(length.getMinimum(), length.getMaximum());
	}

	private static ExposesRelationships<?> applyOptionalProperties(ExposesRelationships<?> relationshipPattern, PathAtom pathAtom) {
		if (pathAtom.getProperties() == null) {
			return relationshipPattern;
		}
		if (relationshipPattern instanceof ExposesProperties<?> exposesProperties) {
			return (ExposesRelationships<?>) exposesProperties.withProperties(pathAtom.getProperties());
		}
		return ((RelationshipChain) relationshipPattern).properties(pathAtom.getProperties());
	}

	private static ExposesRelationships<?> applyOptionalName(ExposesRelationships<?> relationshipPattern, PathAtom pathAtom) {
		if (pathAtom.getName() == null) {
			return relationshipPattern;
		}
		if (relationshipPattern instanceof Relationship relationship) {
			return relationship.named(pathAtom.getName());
		}
		return ((RelationshipChain) relationshipPattern).named(pathAtom.getName());
	}

	@Override
	public NodeAtom nodePattern(InputPosition p, Expression v, LabelExpression labels, Expression properties, Expression predicate) {

		Node node;
		if (labels == null) {
			node = Cypher.anyNode();
		} else {
			var finalLabels = computeFinalLabelList(LabelParsedEventType.ON_NODE_PATTERN, labels);
			node = finalLabels.map(l -> {
				var primaryLabel = l[0];
				var additionalLabels = Arrays.stream(l).skip(1).toList();
				return Cypher.node(primaryLabel, additionalLabels);
			}).orElseGet(() -> Cypher.node(labels));
		}

		if (v != null) {
			node = node.named(assertSymbolicName(v));
		}
		if (properties != null) {
			node = node.withProperties((MapExpression) properties);
		}
		return new NodeAtom(node, predicate);
	}

	@Override
	public PathAtom relationshipPattern(InputPosition p, boolean left, boolean right, Expression v, LabelExpression relTypes, PathLength pathLength, Expression properties, Expression predicate) {
		return PathAtom.of(assertSymbolicName(v), pathLength, left, right,
			computeFinalTypeList(TypeParsedEventType.ON_RELATIONSHIP_PATTERN, relTypes), (MapExpression) properties,
			relTypes != null && relTypes.negated());
	}

	@Override
	public PathLength pathLength(InputPosition p, InputPosition pMin, InputPosition pMax, String minLength,
		String maxLength) {
		return PathLength.of(minLength, maxLength);
	}

	@Override
	public NULL intervalPathQuantifier(InputPosition p, InputPosition posLowerBound, InputPosition posUpperBound, String lowerBound, String upperBound) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL fixedPathQuantifier(InputPosition p, InputPosition valuePos, String value) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL plusPathQuantifier(InputPosition p) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL starPathQuantifier(InputPosition p) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL repeatableElements(InputPosition p) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL differentRelationships(InputPosition p) {
		throw new UnsupportedOperationException();
	}

	@Override
	public PatternAtom parenthesizedPathPattern(InputPosition p, PatternElement internalPattern, Expression where, NULL aNull) {
		return ParenthesizedPathPatternAtom.of(internalPattern);
	}

	@Override
	public PatternAtom quantifiedRelationship(PathAtom rel, NULL aNull) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Clause loadCsvClause(InputPosition p, boolean headers, Expression source, Expression v,
		String fieldTerminator) {

		isInstanceOf(StringLiteral.class, source, "Only string literals are supported as source for the LOAD CSV clause.");
		return Clauses.loadCSV(headers, (StringLiteral) source, assertSymbolicName(v), fieldTerminator);
	}

	@Override
	public Clause foreachClause(InputPosition p, Expression v, Expression list, List<Clause> objects) {
		return Clauses.forEach(assertSymbolicName(v), list, objects);
	}

	@Override
	public Clause subqueryClause(InputPosition p, Statement subquery, NULL inTransactions) {
		return Clauses.callClause(subquery);
	}

	@Override
	public Clause yieldClause(InputPosition p, boolean returnAll, List<Expression> expressions,
		InputPosition returnItemsPosition, List<SortItem> orderBy, InputPosition orderPos, Expression skip,
		InputPosition skipPosition, Expression limit, InputPosition limitPosition, Where where) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Clause showIndexClause(InputPosition p, ShowCommandFilterTypes indexType, boolean brief, boolean verbose,
		Where where, boolean hasYield) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Clause showConstraintClause(InputPosition p, ShowCommandFilterTypes constraintType, boolean brief,
		boolean verbose, Where where, boolean hasYield) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Clause showProcedureClause(InputPosition p, boolean currentUser, String user, Where where,
		boolean hasYield) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Clause showFunctionClause(InputPosition p, ShowCommandFilterTypes functionType, boolean currentUser,
		String user, Where where, boolean hasYield) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement useGraph(Statement command, Clause useGraph) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement createRole(InputPosition p, boolean replace, SimpleEither<String, Parameter<?>> roleName,
		SimpleEither<String, Parameter<?>> fromRole, boolean ifNotExists) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement dropRole(InputPosition p, SimpleEither<String, Parameter<?>> roleName, boolean ifExists) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement renameRole(InputPosition p, SimpleEither<String, Parameter<?>> fromRoleName,
		SimpleEither<String, Parameter<?>> toRoleName, boolean ifExists) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement showRoles(InputPosition p, boolean withUsers, boolean showAll, Clause yieldExpr,
		Return returnWithoutGraph, Where where) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement grantRoles(InputPosition p, List<SimpleEither<String, Parameter<?>>> roles,
		List<SimpleEither<String, Parameter<?>>> users) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement revokeRoles(InputPosition p, List<SimpleEither<String, Parameter<?>>> roles,
		List<SimpleEither<String, Parameter<?>>> users) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement createUser(InputPosition p, boolean replace, boolean ifNotExists,
		SimpleEither<String, Parameter<?>> username,
		Expression password, boolean encrypted, boolean changeRequired, Boolean suspended,
		DatabaseName homeDatabase) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement dropUser(InputPosition p, boolean ifExists, SimpleEither<String, Parameter<?>> username) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement renameUser(InputPosition p, SimpleEither<String, Parameter<?>> fromUserName,
		SimpleEither<String, Parameter<?>> toUserName, boolean ifExists) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement setOwnPassword(InputPosition p, Expression currentPassword, Expression newPassword) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement alterUser(InputPosition p, boolean ifExists, SimpleEither<String, Parameter<?>> username,
		Expression password,
		boolean encrypted, Boolean changeRequired, Boolean suspended, DatabaseName homeDatabase,
		boolean removeHome) {
		throw new UnsupportedOperationException();
	}

	@Override public Expression passwordExpression(Parameter<?> password) {
		throw new UnsupportedOperationException();
	}

	@Override public Expression passwordExpression(InputPosition p, String password) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement showUsers(InputPosition p, Clause yieldExpr, Return returnWithoutGraph, Where where) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement showCurrentUser(InputPosition p, Clause yieldExpr, Return returnWithoutGraph, Where where) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement showSupportedPrivileges(InputPosition p, Clause yieldExpr, Return returnWithoutGraph, Where where) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement showAllPrivileges(InputPosition p, boolean asCommand, boolean asRevoke, Clause yieldExpr, Return returnWithoutGraph, Where where) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement showRolePrivileges(InputPosition p, List<SimpleEither<String, Parameter<?>>> roles, boolean asCommand, boolean asRevoke, Clause yieldExpr, Return returnWithoutGraph, Where where) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement showUserPrivileges(InputPosition p, List<SimpleEither<String, Parameter<?>>> users, boolean asCommand, boolean asRevoke, Clause yieldExpr, Return returnWithoutGraph, Where where) {
		throw new UnsupportedOperationException();
	}

	@SuppressWarnings("HiddenField") // The database options are quite different options than ours ;)
	@Override
	public Statement createDatabase(InputPosition p, boolean replace, DatabaseName databaseName,
		boolean ifNotExists, NULL aNull, SimpleEither<Map<String, Expression>, Parameter<?>> options,
		Integer topologyPrimaries, Integer topologySecondaries) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement createCompositeDatabase(InputPosition p, boolean replace, DatabaseName compositeDatabaseName,
		boolean ifNotExists, SimpleEither<Map<String, Expression>, Parameter<?>> databaseOptions, NULL aNull) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement dropDatabase(InputPosition p, DatabaseName databaseName, boolean ifExists, boolean composite,
		boolean dumpData, NULL wait) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement alterDatabase(InputPosition p, DatabaseName databaseName, boolean ifExists, AccessType accessType, Integer topologyPrimaries, Integer topologySecondaries, Map<String, Expression> newOptions, java.util.Set<String> optionsToRemove, NULL aNull) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement showDatabase(InputPosition p, NULL scope, Clause yieldExpr, Return returnWithoutGraph,
		Where where) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement startDatabase(InputPosition p, DatabaseName databaseName, NULL wait) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement stopDatabase(InputPosition p, DatabaseName databaseName, NULL wait) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL databaseScope(InputPosition p, DatabaseName databaseName, boolean isDefault, boolean isHome) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement dropAlias(InputPosition p, DatabaseName aliasName, boolean ifExists) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement showAliases(InputPosition p, DatabaseName aliasName, Clause yieldExpr, Return returnWithoutGraph, Where where) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL wait(boolean wait, long seconds) {
		throw new UnsupportedOperationException();
	}

	@Override
	public DatabaseName databaseName(InputPosition p, List<String> names) {
		if (names.isEmpty()) {
			throw new IllegalArgumentException("No database name");
		}
		if (names.size() == 1) {
			return new DatabaseName(Cypher.literalOf(names.get(0)));
		}
		return new DatabaseName(Cypher.literalOf(names));
	}

	@Override
	public DatabaseName databaseName(Parameter<?> param) {
		return new DatabaseName(param);
	}

	@Override
	public Statement createLocalDatabaseAlias(InputPosition p, boolean replace, DatabaseName aliasName,
		DatabaseName targetName, boolean ifNotExists, SimpleEither<Map<String, Expression>, Parameter<?>> properties
	) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement createRemoteDatabaseAlias(InputPosition p, boolean replace, DatabaseName aliasName,
		DatabaseName targetName, boolean ifNotExists, SimpleEither<String, Parameter<?>> url,
		SimpleEither<String, Parameter<?>> username, Expression password, SimpleEither<Map<String, Expression>, Parameter<?>> driverSettings,
		SimpleEither<Map<String, Expression>, Parameter<?>> properties
	) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement alterLocalDatabaseAlias(InputPosition p, DatabaseName aliasName, DatabaseName targetName,
		boolean ifExists, SimpleEither<Map<String, Expression>, Parameter<?>> properties
	) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement alterRemoteDatabaseAlias(InputPosition p, DatabaseName aliasName, DatabaseName targetName,
		boolean ifExists, SimpleEither<String, Parameter<?>> url, SimpleEither<String, Parameter<?>> username,
		Expression password, SimpleEither<Map<String, Expression>, Parameter<?>> driverSettings,
		SimpleEither<Map<String, Expression>, Parameter<?>> properties
	) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Expression newVariable(InputPosition p, String name) {
		return applyCallbacksFor(ExpressionCreatedEventType.ON_NEW_VARIABLE, Cypher.name(name));
	}

	@Override
	public Parameter<?> newParameter(InputPosition p, Expression v, ParameterType type) {
		return applyCallbacksFor(ExpressionCreatedEventType.ON_NEW_PARAMETER, parameterFromSymbolicName(v));
	}

	@Override
	public Parameter<?> newParameter(InputPosition p, String v, ParameterType type) {
		return applyCallbacksFor(ExpressionCreatedEventType.ON_NEW_PARAMETER, parameterFromSymbolicName(Cypher.name(v)));
	}

	@Override
	public Parameter<?> newSensitiveStringParameter(InputPosition p, Expression v) {
		throw new UnsupportedOperationException("The Cypher-DSL does not support sensitive parameters.");
	}

	@Override
	public Parameter<?> newSensitiveStringParameter(InputPosition p, String v) {
		throw new UnsupportedOperationException("The Cypher-DSL does not support sensitive parameters.");
	}

	@NotNull
	Parameter<?> parameterFromSymbolicName(Expression v) {
		var symbolicName = assertSymbolicName(v);
		if (symbolicName == null) {
			return Cypher.anonParameter(Cypher.literalNull());
		}

		var name = symbolicName.getValue();
		return options.getParameterValues().containsKey(name) ? Cypher.parameter(name, options.getParameterValues().get(name)) : Cypher.parameter(name);
	}

	@Override
	public Expression newDouble(InputPosition p, String image) {
		return applyCallbacksFor(ExpressionCreatedEventType.ON_NEW_LITERAL, Cypher.literalOf(Double.parseDouble(image)));
	}

	@Override
	public Expression newDecimalInteger(InputPosition p, String image, boolean negated) {
		return applyCallbacksFor(ExpressionCreatedEventType.ON_NEW_LITERAL, Cypher.literalOf(Long.parseUnsignedLong(image) * (negated ? -1 : 1)));
	}

	@Override public Expression newHexInteger(InputPosition p, String image, boolean negated) {
		return applyCallbacksFor(ExpressionCreatedEventType.ON_NEW_LITERAL, Cypher.literalOf(Long.parseUnsignedLong(image.replaceFirst("(?i)0x", ""), 16) * (negated ? -1 : 1)));
	}

	@Override public Expression newOctalInteger(InputPosition p, String image, boolean negated) {
		return applyCallbacksFor(ExpressionCreatedEventType.ON_NEW_LITERAL, Cypher.literalOf(Long.parseUnsignedLong(image.replaceFirst("(?i)0o", ""), 8) * (negated ? -1 : 1)));
	}

	@Override
	public Expression newString(InputPosition p, String image) {
		return applyCallbacksFor(ExpressionCreatedEventType.ON_NEW_LITERAL, Cypher.literalOf(image));
	}

	@Override
	public Expression newTrueLiteral(InputPosition p) {
		return applyCallbacksFor(ExpressionCreatedEventType.ON_NEW_LITERAL, Cypher.literalTrue());
	}

	@Override
	public Expression newFalseLiteral(InputPosition p) {
		return applyCallbacksFor(ExpressionCreatedEventType.ON_NEW_LITERAL, Cypher.literalFalse());
	}

	@Override
	public Expression newInfinityLiteral(InputPosition p) {
		return applyCallbacksFor(ExpressionCreatedEventType.ON_NEW_LITERAL, InfinityLiteral.INSTANCE);
	}

	@Override
	public Expression newNaNLiteral(InputPosition p) {
		return applyCallbacksFor(ExpressionCreatedEventType.ON_NEW_LITERAL, NaNLiteral.INSTANCE);
	}

	@Override
	public Expression newNullLiteral(InputPosition p) {
		return applyCallbacksFor(ExpressionCreatedEventType.ON_NEW_LITERAL, Cypher.literalNull());
	}

	@Override
	public Expression listLiteral(InputPosition p, List<Expression> values) {
		return Cypher.listOf(values.toArray(new Expression[0]));
	}

	@Override
	public MapExpression mapLiteral(InputPosition p, List<StringPos<InputPosition>> keys, List<Expression> values) {
		Object[] keysAndValues = new Object[keys.size() * 2];
		int i = 0;
		Iterator<Expression> valueIterator = values.iterator();
		for (StringPos<InputPosition> key : keys) {
			keysAndValues[i++] = key.string;
			keysAndValues[i++] = valueIterator.next();
		}
		return options.isCreateSortedMaps() ? Cypher.sortedMapOf(keysAndValues) : Cypher.mapOf(keysAndValues);
	}

	@Override
	public Property property(Expression subject, StringPos<InputPosition> propertyKeyName) {
		return subject.property(propertyKeyName.string);
	}

	@Override
	public Expression or(InputPosition p, Expression lhs, Expression rhs) {
		return lhs.asCondition().or(rhs.asCondition());
	}

	@Override
	public Expression xor(InputPosition p, Expression lhs, Expression rhs) {
		return lhs.asCondition().xor(rhs.asCondition());
	}

	@Override
	public Expression and(InputPosition p, Expression lhs, Expression rhs) {
		return lhs.asCondition().and(rhs.asCondition());
	}

	@Override
	public LabelExpression labelConjunction(InputPosition p, LabelExpression lhs, LabelExpression rhs, boolean containsIs) {

		return lhs.and(rhs);
	}

	@Override
	public LabelExpression labelDisjunction(InputPosition p, LabelExpression lhs, LabelExpression rhs, boolean containsIs) {

		return lhs.or(rhs);
	}

	@Override
	public LabelExpression labelNegation(InputPosition p, LabelExpression e, boolean containsIs) {

		return e.negate();
	}

	@Override
	public LabelExpression labelWildcard(InputPosition p, boolean containsIs) {
		throw new UnsupportedOperationException();
	}

	@Override
	public LabelExpression labelLeaf(InputPosition p, String e, EntityType entityType, boolean containsIs) {
		return new LabelExpression(e);
	}

	@Override
	public LabelExpression labelColonConjunction(InputPosition p, LabelExpression lhs, LabelExpression rhs, boolean containsIs) {

		return colonJunjction(lhs, rhs, LabelExpression.Type.COLON_CONJUNCTION);
	}

	@Override
	public LabelExpression labelColonDisjunction(InputPosition p, LabelExpression lhs, LabelExpression rhs, boolean containsIs) {

		return colonJunjction(lhs, rhs, LabelExpression.Type.COLON_DISJUNCTION);
	}

	@NotNull
	private static LabelExpression colonJunjction(LabelExpression lhs, LabelExpression rhs, LabelExpression.Type colonDisjunction) {
		List<String> value = new ArrayList<>();
		value.addAll(lhs.value());
		value.addAll(rhs.value());
		return new LabelExpression(colonDisjunction, false, value, null, null);
	}

	@Override
	public Expression labelExpressionPredicate(Expression subject, LabelExpression exp) {

		if (!(subject instanceof SymbolicName symbolicName)) {
			throw new IllegalArgumentException("Expected an symbolic name to create a label based expression predicate!");
		} else {
			List<String> values = new ArrayList<>();
			LabelExpression current = exp;
			while (current != null) {
				values.addAll(current.value());
				current = current.rhs();
			}
			return Conditions.hasLabelsOrType(symbolicName, values.toArray(String[]::new));
		}
	}

	@Override
	public Expression ands(List<Expression> exprs) {
		return exprs.stream().reduce(Conditions.noCondition(), (l, r) -> l.asCondition().and(r.asCondition()));
	}

	@Override
	public Expression not(InputPosition p, Expression e) {
		return e.asCondition().not();
	}

	@Override
	public Expression plus(InputPosition p, Expression lhs, Expression rhs) {
		return lhs.add(rhs);
	}

	@Override
	public Expression minus(InputPosition p, Expression lhs, Expression rhs) {
		return lhs.subtract(rhs);
	}

	@Override
	public Expression multiply(InputPosition p, Expression lhs, Expression rhs) {
		return lhs.multiply(rhs);
	}

	@Override
	public Expression divide(InputPosition p, Expression lhs, Expression rhs) {
		return lhs.divide(rhs);
	}

	@Override
	public Expression modulo(InputPosition p, Expression lhs, Expression rhs) {
		return lhs.remainder(rhs);
	}

	@Override
	public Expression pow(InputPosition p, Expression lhs, Expression rhs) {
		return lhs.pow(rhs);
	}

	@Override public Expression unaryPlus(Expression e) {
		return Operations.plus(e);
	}

	@Override
	public Expression unaryPlus(InputPosition inputPosition, Expression expression) {
		return Operations.plus(expression);
	}

	@Override
	public Expression unaryMinus(InputPosition inputPosition, Expression expression) {
		return Operations.minus(expression);
	}

	@Override
	public Expression eq(InputPosition p, Expression lhs, Expression rhs) {
		return lhs.eq(rhs);
	}

	@Override
	public Expression neq(InputPosition p, Expression lhs, Expression rhs) {
		return lhs.ne(rhs);
	}

	@Override
	public Expression neq2(InputPosition p, Expression lhs, Expression rhs) {
		return lhs.ne(rhs);
	}

	@Override
	public Expression lte(InputPosition p, Expression lhs, Expression rhs) {
		return lhs.lte(rhs);
	}

	@Override
	public Expression gte(InputPosition p, Expression lhs, Expression rhs) {
		return lhs.gte(rhs);
	}

	@Override
	public Expression lt(InputPosition p, Expression lhs, Expression rhs) {
		return lhs.lt(rhs);
	}

	@Override
	public Expression gt(InputPosition p, Expression lhs, Expression rhs) {
		return lhs.gt(rhs);
	}

	@Override
	public Expression regeq(InputPosition p, Expression lhs, Expression rhs) {
		return lhs.matches(rhs);
	}

	@Override
	public Expression startsWith(InputPosition p, Expression lhs, Expression rhs) {
		return lhs.startsWith(rhs);
	}

	@Override
	public Expression endsWith(InputPosition p, Expression lhs, Expression rhs) {
		return lhs.endsWith(rhs);
	}

	@Override
	public Expression contains(InputPosition p, Expression lhs, Expression rhs) {
		return lhs.contains(rhs);
	}

	@Override
	public Expression in(InputPosition p, Expression lhs, Expression rhs) {
		return lhs.in(rhs);
	}

	@Override
	public Expression isNull(InputPosition p, Expression e) {
		return e.isNull();
	}

	@Override
	public Expression isNotNull(InputPosition p, Expression e) {
		return e.isNotNull();
	}

	@Override
	public Expression isTyped(InputPosition p, Expression e, ParserCypherTypeName typeName) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Expression isNotTyped(InputPosition p, Expression e, ParserCypherTypeName typeName) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Expression listLookup(Expression list, Expression index) {
		return Cypher.valueAt(list, index);
	}

	@Override
	public Expression listSlice(InputPosition p, Expression list, Expression start, Expression end) {
		return Cypher.subList(list, start, end);
	}

	@Override
	public Expression newCountStar(InputPosition p) {
		return Functions.count(Cypher.asterisk());
	}

	@Override
	public Expression functionInvocation(InputPosition p, InputPosition functionNamePosition,  List<String> namespace, String name, boolean distinct,
		List<Expression> arguments) {

		String[] parts = new String[namespace.size() + 1];
		for (int i = 0; i < namespace.size(); i++) {
			parts[i] = namespace.get(i);
		}
		parts[parts.length - 1] = name;
		var expression = Cypher.call(parts).withArgs(arguments.toArray(Expression[]::new)).asFunction(distinct);
		return applyCallbacksFor(InvocationCreatedEventType.ON_INVOCATION, expression);
	}

	@Override
	public Expression listComprehension(InputPosition p, Expression v, Expression list, Expression where,
		Expression projection) {

		var in = Cypher.listWith(assertSymbolicName(v)).in(list);
		if (where != null) {
			var ongoingComprehension = in.where(where.asCondition());
			if (projection != null) {
				return ongoingComprehension.returning(projection);
			}
			return ongoingComprehension.returning();
		}
		return in.returning(projection);
	}

	@Override
	public Expression patternComprehension(InputPosition p, InputPosition relationshipPatternPosition, Expression v, PatternElement patternElement,
		Expression where, Expression projection) {

		PatternComprehension.OngoingDefinitionWithoutReturn ongoingDefinitionWithPattern;
		if (patternElement instanceof RelationshipPattern relationshipPattern) {
			if (v != null) {
				ongoingDefinitionWithPattern = Cypher.listBasedOn(Cypher.path(assertSymbolicName(v)).definedBy(relationshipPattern));
			} else {
				ongoingDefinitionWithPattern = Cypher.listBasedOn(relationshipPattern);
			}
		} else if (patternElement instanceof NamedPath namedPath) {
			ongoingDefinitionWithPattern = Cypher.listBasedOn(namedPath);
		} else {
			throw new IllegalArgumentException(
				"Cannot build a pattern comprehension around " + patternElement.getClass().getSimpleName());
		}
		if (where != null) {
			ongoingDefinitionWithPattern = ((PatternComprehension.OngoingDefinitionWithPattern) ongoingDefinitionWithPattern)
				.where(where.asCondition());
		}

		return ongoingDefinitionWithPattern.returning(projection);
	}

	@Override
	public Expression reduceExpression(InputPosition p, Expression acc, Expression accExpr, Expression v,
		Expression list, Expression innerExpr) {

		var variable = assertSymbolicName(v);
		if (variable == null) {
			throw new IllegalArgumentException("A variable to be reduced must be present.");
		}
		return Functions.reduce(variable)
			.in(list)
			.map(innerExpr)
			.accumulateOn(assertSymbolicName(acc))
			.withInitialValueOf(accExpr);
	}

	@Override
	public Expression allExpression(InputPosition p, Expression v, Expression list, Expression where) {

		notNull(where, "all(...) requires a WHERE predicate");
		return Predicates.all(assertSymbolicName(v)).in(list).where(where.asCondition());
	}

	@Override
	public Expression anyExpression(InputPosition p, Expression v, Expression list, Expression where) {

		notNull(where, "any(...) requires a WHERE predicate");
		return Predicates.any(assertSymbolicName(v)).in(list).where(where.asCondition());
	}

	@Override
	public Expression noneExpression(InputPosition p, Expression v, Expression list, Expression where) {

		notNull(where, "none(...) requires a WHERE predicate");
		return Predicates.none(assertSymbolicName(v)).in(list).where(where.asCondition());
	}

	@Override
	public Expression singleExpression(InputPosition p, Expression v, Expression list, Expression where) {

		notNull(where, "single(...) requires a WHERE predicate");
		return Predicates.single(assertSymbolicName(v)).in(list).where(where.asCondition());
	}

	@Override
	public Expression patternExpression(InputPosition p, PatternElement patternElement) {

		if (patternElement instanceof ExpressionAsPatternElementWrapper wrapper) {
			return wrapper.getExpression();
		}

		if (patternElement instanceof RelationshipPattern relationshipPattern) {
			return new PatternElementAsExpressionWrapper(relationshipPattern);
		}

		throw new UnsupportedOperationException();
	}

	@Override
	public Expression existsExpression(InputPosition p, NULL matchMode, List<PatternElement> patternElements, Statement q, Where where) {

		var elementsAndWhere = extractElementsAndWhere(patternElements, q, where);
		StatementBuilder.OngoingReadingWithoutWhere match = Cypher.match(elementsAndWhere.elements());
		if (elementsAndWhere.where() == null) {
			return match.asCondition();
		}
		var capturedCondition = new AtomicReference<Condition>();
		elementsAndWhere.where.accept(segment -> {
			if (segment instanceof Condition condition) {
				capturedCondition.compareAndSet(null, condition);
			}
		});
		return match.where(capturedCondition.get()).asCondition();
	}

	record ElementsAndWhere(List<PatternElement> elements, Where where) {
	}

	private ElementsAndWhere extractElementsAndWhere(List<PatternElement> patternElements, Statement q, Where where) {

		Where where0 = where;
		List<PatternElement> patternElements0 = patternElements;

		if (patternElements0 == null && q != null) {
			AtomicReference<Where> capturedWhere = new AtomicReference<>();
			AtomicBoolean inMatch = new AtomicBoolean();
			AtomicReference<PatternElement> inPattern = new AtomicReference<>();
			List<PatternElement> capturedElements = new ArrayList<>();
			q.accept(new Visitor() {
				@Override
				public void enter(Visitable segment) {
					inMatch.compareAndSet(false, segment instanceof Match);
					if (!inMatch.get() || capturedWhere.get() != null) {
						return;
					}

					if (segment instanceof Where innerWhere) {
						capturedWhere.compareAndSet(null, innerWhere);
					} else if (segment instanceof PatternElement patternElement && inPattern.compareAndSet(null, patternElement)) {
						capturedElements.add(patternElement);
					}
				}

				@Override
				public void leave(Visitable segment) {
					inMatch.compareAndSet(true, !(segment instanceof Match));
					if (segment instanceof PatternElement patternElement) {
						inPattern.compareAndSet(patternElement, null);
					}
				}
			});
			patternElements0 = capturedElements;
			where0 = capturedWhere.get();
		}

		return new ElementsAndWhere(patternElements0, where0);
	}

	@Override
	public Expression countExpression(InputPosition p, NULL matchMode, List<PatternElement> patternElements, Statement q, Where where) {

		var elementsAndWhere = extractElementsAndWhere(patternElements, q, where);
		Expression condition = null;
		if (elementsAndWhere.where() != null) {
			var capturedCondition = new AtomicReference<Condition>();
			elementsAndWhere.where.accept(segment -> {
				if (segment instanceof Condition nestedCondition) {
					capturedCondition.compareAndSet(null, nestedCondition);
				}
			});
			condition = capturedCondition.get();
		}


		var elements = elementsAndWhere.elements();
		var count = Expressions.count(elements.get(0), elements.subList(1, elements.size()).toArray(PatternElement[]::new));
		if (condition == null) {
			return count;
		}
		return count.where(condition.asCondition());
	}

	@Override
	public Expression collectExpression(InputPosition inputPosition, Statement statement) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Expression mapProjection(InputPosition p, Expression v, List<Expression> items) {
		return MapProjection.create(assertSymbolicName(v), items.toArray(new Object[0]));
	}

	@Override
	public Expression mapProjectionLiteralEntry(StringPos<InputPosition> property, Expression value) {
		return KeyValueMapEntry.create(property.string, value);
	}

	@Override
	public Expression mapProjectionProperty(StringPos<InputPosition> property) {
		return PropertyLookup.forName(property.string);
	}

	@Override
	public Expression mapProjectionVariable(Expression v) {
		return v;
	}

	@Override
	public Expression mapProjectionAll(InputPosition p) {
		return Cypher.asterisk();
	}

	@Override
	public Expression caseExpression(InputPosition p, Expression e, List<Expression> whens, List<Expression> thens,
		Expression elze) {

		if (whens != null && thens != null && whens.size() != thens.size()) {
			throw new IllegalArgumentException("Cannot combine lists of whens with a different sized list of thens.");
		}

		var aCase = Cypher.caseExpression(e);
		if (whens != null && thens != null) {
			var iteratorWhens = whens.iterator();
			var iteratorThens = thens.iterator();
			while (iteratorWhens.hasNext() && iteratorThens.hasNext()) {
				aCase = aCase.when(iteratorWhens.next()).then(iteratorThens.next());
			}
			if (elze != null) {
				return ((Case.CaseEnding) aCase).elseDefault(elze);
			}
			return aCase;
		}
		return aCase;
	}

	@Override
	public InputPosition inputPosition(int offset, int line, int column) {
		return new InputPosition(offset, line, column);
	}

	@Override
	public EntityType nodeType() {
		return EntityType.NODE;
	}

	@Override
	public EntityType relationshipType() {
		return EntityType.RELATIONSHIP;
	}

	@Override
	public EntityType nodeOrRelationshipType() {
		return EntityType.LOLWHAT;
	}

	@Override
	public Where whereClause(InputPosition p, Expression optionalWhere) {

		return Where.from(optionalWhere);
	}

	@Override
	public NULL subqueryInTransactionsParams(InputPosition p, NULL batchParams, NULL errorParams, NULL reportParams) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL subqueryInTransactionsBatchParameters(InputPosition p, Expression batchSize) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL subqueryInTransactionsErrorParameters(InputPosition p, CallInTxsOnErrorBehaviourType onErrorBehaviour) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL subqueryInTransactionsReportParameters(InputPosition p, Expression v) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Clause showTransactionsClause(InputPosition p, SimpleEither<List<String>, Expression> ids, Where where, Clause yieldClause) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Clause terminateTransactionsClause(InputPosition p, SimpleEither<List<String>, Expression> ids, Where where, Clause yieldClause) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Clause turnYieldToWith(Clause yieldClause) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Clause showSettingsClause(InputPosition inputPosition, SimpleEither<List<String>, Expression> simpleEither, Where where, boolean b) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement createConstraint(InputPosition p, ConstraintType constraintType, boolean replace, boolean ifNotExists, String constraintName, Expression expression, StringPos<InputPosition> label, List<Property> properties, ParserCypherTypeName propertyType, SimpleEither<Map<String, Expression>, Parameter<?>> constraintOptions, boolean containsOn, ConstraintVersion constraintVersion) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement dropConstraint(InputPosition p, String name, boolean ifExists) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement dropConstraint(InputPosition p, ConstraintType constraintType, Expression expression,
		StringPos<InputPosition> label, List<Property> properties) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement createIndexWithOldSyntax(InputPosition p, StringPos<InputPosition> label,
		List<StringPos<InputPosition>> properties) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement createLookupIndex(InputPosition p, boolean replace, boolean ifNotExists, boolean isNode,
		String indexName, Expression expression, StringPos<InputPosition> functionName, Expression functionParameter,
		SimpleEither<Map<String, Expression>, Parameter<?>> indexOptions) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement createIndex(InputPosition p, boolean replace, boolean ifNotExists, boolean isNode,
		String indexName, Expression expression, StringPos<InputPosition> label, List<Property> properties,
		SimpleEither<Map<String, Expression>, Parameter<?>> indexOptions, CreateIndexTypes indexType) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement createFulltextIndex(InputPosition p, boolean replace, boolean ifNotExists, boolean isNode,
		String indexName, Expression expression, List<StringPos<InputPosition>> labels, List<Property> properties,
		SimpleEither<Map<String, Expression>, Parameter<?>> indexOptions) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement dropIndex(InputPosition p, String name, boolean ifExists) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement dropIndex(InputPosition p, StringPos<InputPosition> label,
		List<StringPos<InputPosition>> propertyNames) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement grantPrivilege(InputPosition p, List<SimpleEither<String, Parameter<?>>> roles, NULL privilege) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement denyPrivilege(InputPosition p, List<SimpleEither<String, Parameter<?>>> roles, NULL privilege) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement revokePrivilege(InputPosition p, List<SimpleEither<String, Parameter<?>>> roles, NULL privilege,
		boolean revokeGrant, boolean revokeDeny) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL databasePrivilege(InputPosition p, NULL aNull, NULL aNull2, List<NULL> qualifier, boolean immutable) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL dbmsPrivilege(InputPosition p, NULL aNull, List<NULL> qualifier, boolean immutable) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL loadPrivilege(InputPosition inputPosition, SimpleEither<String, Parameter<?>> simpleEither,
		SimpleEither<String, Parameter<?>> simpleEither1, boolean b) {
		return null;
	}

	@Override
	public NULL graphPrivilege(InputPosition inputPosition, NULL aNull, NULL aNull2, NULL aNull3, List<NULL> qualifier, boolean immutable) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL privilegeAction(ActionType action) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL propertiesResource(InputPosition p, List<String> property) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL allPropertiesResource(InputPosition p) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL labelsResource(InputPosition p, List<String> label) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL allLabelsResource(InputPosition p) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL databaseResource(InputPosition p) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL noResource(InputPosition p) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL labelQualifier(InputPosition p, String label) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL relationshipQualifier(InputPosition p, String relationshipType) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL elementQualifier(InputPosition p, String name) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL allElementsQualifier(InputPosition p) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL patternQualifier(List<NULL> list, Expression expression, Expression expression2) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL allLabelsQualifier(InputPosition p) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL allRelationshipsQualifier(InputPosition p) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<NULL> allQualifier() {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<NULL> allDatabasesQualifier() {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<NULL> userQualifier(List<SimpleEither<String, Parameter<?>>> users) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<NULL> allUsersQualifier() {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<NULL> functionQualifier(InputPosition p, List<String> functions) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<NULL> procedureQualifier(InputPosition p, List<String> procedures) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<NULL> settingQualifier(InputPosition inputPosition, List<String> list) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL graphScope(InputPosition inputPosition, List<DatabaseName> graphNames, ScopeType scopeType) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL databaseScope(InputPosition inputPosition, List<DatabaseName> databaseNames, ScopeType scopeType) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement enableServer(InputPosition p, SimpleEither<String, Parameter<?>> serverName, SimpleEither<Map<String, Expression>, Parameter<?>> serverOptions) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement alterServer(InputPosition p, SimpleEither<String, Parameter<?>> serverName, SimpleEither<Map<String, Expression>, Parameter<?>> serverOptions) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement renameServer(InputPosition p, SimpleEither<String, Parameter<?>> serverName, SimpleEither<String, Parameter<?>> newName) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement dropServer(InputPosition p, SimpleEither<String, Parameter<?>> serverName) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement showServers(InputPosition p, Clause yieldExpr, Return returnWithoutGraph, Where where) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement deallocateServers(InputPosition p, boolean dryRun, List<SimpleEither<String, Parameter<?>>> serverNames) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement reallocateDatabases(InputPosition p, boolean dryRun) {
		throw new UnsupportedOperationException();
	}
}
