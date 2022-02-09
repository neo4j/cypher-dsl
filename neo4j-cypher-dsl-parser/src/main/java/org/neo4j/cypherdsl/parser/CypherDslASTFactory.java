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
import java.util.function.Function;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;

import org.apiguardian.api.API;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.neo4j.cypher.internal.ast.factory.ASTFactory;
import org.neo4j.cypher.internal.ast.factory.ASTFactory.NULL;
import org.neo4j.cypher.internal.ast.factory.AccessType;
import org.neo4j.cypher.internal.ast.factory.ActionType;
import org.neo4j.cypher.internal.ast.factory.ConstraintType;
import org.neo4j.cypher.internal.ast.factory.ConstraintVersion;
import org.neo4j.cypher.internal.ast.factory.CreateIndexTypes;
import org.neo4j.cypher.internal.ast.factory.HintIndexType;
import org.neo4j.cypher.internal.ast.factory.ParameterType;
import org.neo4j.cypher.internal.ast.factory.ScopeType;
import org.neo4j.cypher.internal.ast.factory.ShowCommandFilterTypes;
import org.neo4j.cypher.internal.ast.factory.SimpleEither;
import org.neo4j.cypherdsl.core.Case;
import org.neo4j.cypherdsl.core.Clause;
import org.neo4j.cypherdsl.core.Clauses;
import org.neo4j.cypherdsl.core.Conditions;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.ExposesPatternLengthAccessors;
import org.neo4j.cypherdsl.core.ExposesProperties;
import org.neo4j.cypherdsl.core.ExposesRelationships;
import org.neo4j.cypherdsl.core.Expression;
import org.neo4j.cypherdsl.core.FunctionInvocation;
import org.neo4j.cypherdsl.core.Functions;
import org.neo4j.cypherdsl.core.Hint;
import org.neo4j.cypherdsl.core.KeyValueMapEntry;
import org.neo4j.cypherdsl.core.Literal;
import org.neo4j.cypherdsl.core.MapExpression;
import org.neo4j.cypherdsl.core.MapProjection;
import org.neo4j.cypherdsl.core.MergeAction;
import org.neo4j.cypherdsl.core.NamedPath;
import org.neo4j.cypherdsl.core.Node;
import org.neo4j.cypherdsl.core.Operation;
import org.neo4j.cypherdsl.core.Operations;
import org.neo4j.cypherdsl.core.Parameter;
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
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * An implementation of Neo4j's {@link ASTFactory} that creates Cypher-DSL AST elements that can be used for creating
 * conditions, patterns to match etc.
 *
 * @author Michael J. Simons
 * @since 2021.3.0
 */
@API(status = INTERNAL, since = "2021.3.0")
final class CypherDslASTFactory implements
	ASTFactory<Statement, Statement, Clause, Return, Expression, List<Expression>, SortItem, PatternElement, Node, PathDetails, PathLength, Clause, Expression, Expression, Expression, Hint, Expression, Parameter<?>, Expression, Property, Expression, Clause, Statement, Statement, Statement, Clause, Where, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, InputPosition> {
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

		return this.options.getLabelFilter()
			.apply(event, inputLabels.stream().map(l -> l.string).collect(Collectors.toUnmodifiableList()))
			.toArray(new String[0]);
	}

	private String[] computeFinalTypeList(TypeParsedEventType event, List<StringPos<InputPosition>> inputTypes) {

		return this.options.getTypeFilter()
			.apply(event, inputTypes.stream().map(l -> l.string).collect(Collectors.toUnmodifiableList()))
			.toArray(new String[0]);
	}

	private <T extends Expression> T applyCallbackFor(ExpressionCreatedEventType type, T newExpression) {

		var callbacks = this.options.getOnNewExpressionCallbacks().getOrDefault(type, List.of());
		if (callbacks.isEmpty()) {
			return newExpression;
		}

		// We checked this when creating the callbacks
		@SuppressWarnings("unchecked")
		T modifiedExpression = (T) callbacks.stream().reduce(Function.identity(), Function::andThen).apply(newExpression);
		return modifiedExpression;
	}

	private static SymbolicName assertSymbolicName(@Nullable Expression v) {

		if (v == null) {
			return null;
		}

		Assertions.isInstanceOf(SymbolicName.class, v,  "An invalid type has been generated where a SymbolicName was required. Generated type was " + v.getClass().getName());
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
	public Statement periodicCommitQuery(InputPosition p, InputPosition periodicCommitPosition, String batchSize, Clause loadCsv, List<Clause> queryBody) {

		var allClauses = new ArrayList<Clause>();
		allClauses.add(loadCsv);
		allClauses.addAll(queryBody);
		return Statement.usingPeriodic(batchSize == null || batchSize.isBlank() ? null : Integer.parseInt(batchSize), allClauses);
	}

	@Override
	public Clause useClause(InputPosition p, Expression e) {
		throw new UnsupportedOperationException();
	}

	public List<Expression> newReturnItems(InputPosition p, boolean returnAll, List<Expression> returnItems) {
		var finalReturnItems = returnItems;
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
		return applyCallbackFor(ExpressionCreatedEventType.ON_RETURN_ITEM, e.as(s));
	}

	@Override
	public Expression newReturnItem(InputPosition p, Expression e, int eStartOffset, int eEndOffset) {
		return applyCallbackFor(ExpressionCreatedEventType.ON_RETURN_ITEM, e);
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
	public Clause matchClause(InputPosition p, boolean optional, List<PatternElement> patternElements, InputPosition patternPos, List<Hint> hints, Where where) {

		var callbacks = this.options.getOnNewPatternElementCallbacks().getOrDefault(PatternElementCreatedEventType.ON_MATCH, List.of());
		return Clauses.match(optional, transformIfPossible(callbacks, patternElements), where, hints);
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
			.collect(Collectors.toList());
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
		// Same note applies as with usingIndexHint in regards of relationships
		return Hint.useScanFor(Cypher.node(label).named(s));
	}

	@Override
	public Clause createClause(InputPosition p, List<PatternElement> patternElements) {

		var callbacks = this.options.getOnNewPatternElementCallbacks().getOrDefault(PatternElementCreatedEventType.ON_CREATE, List.of());
		return Clauses.create(transformIfPossible(callbacks, patternElements));
	}

	@Override
	public Clause setClause(InputPosition p, List<Expression> setItems) {
		return Clauses.set(setItems);
	}

	@Override
	public Operation setProperty(Property property, Expression value) {
		return applyCallbackFor(ExpressionCreatedEventType.ON_SET_PROPERTY, property.to(value));
	}

	@Override
	public Operation setVariable(Expression v, Expression value) {

		return applyCallbackFor(ExpressionCreatedEventType.ON_SET_VARIABLE, Operations.set(v, value));
	}

	@Override
	public Operation addAndSetVariable(Expression v, Expression value) {
		return applyCallbackFor(ExpressionCreatedEventType.ON_ADD_AND_SET_VARIABLE, Operations.mutate(v, value));
	}

	@Override
	public Operation setLabels(Expression v, List<StringPos<InputPosition>> values) {

		var s = assertSymbolicName(v);
		var labels = computeFinalLabelList(LabelParsedEventType.ON_SET, values);
		return applyCallbackFor(ExpressionCreatedEventType.ON_SET_LABELS, Operations.set(Cypher.anyNode(s), labels));
	}

	@Override
	public Clause removeClause(InputPosition p, List<Expression> removeItems) {
		return Clauses.remove(removeItems);
	}

	@Override
	public Expression removeProperty(Property property) {
		return applyCallbackFor(ExpressionCreatedEventType.ON_REMOVE_PROPERTY, property);
	}

	@Override
	public Expression removeLabels(Expression v, List<StringPos<InputPosition>> values) {

		var s = assertSymbolicName(v);
		var labels = computeFinalLabelList(LabelParsedEventType.ON_REMOVE, values);
		return applyCallbackFor(ExpressionCreatedEventType.ON_REMOVE_LABELS, Operations.remove(Cypher.anyNode(s), labels));
	}

	@Override
	public Clause deleteClause(InputPosition p, boolean detach, List<Expression> expressions) {
		return Clauses.delete(detach, expressions);
	}

	@Override
	public Clause unwindClause(InputPosition p, Expression e, Expression v) {
		return Clauses.unwind(e, assertSymbolicName(v));
	}

	@Override
	public Clause mergeClause(InputPosition p, PatternElement patternElement, List<Clause> setClauses,
		List<MergeActionType> actionTypes, List<InputPosition> positions) {

		var mergeActions = new ArrayList<MergeAction>();
		if (setClauses != null && !setClauses.isEmpty() && actionTypes != null && !actionTypes.isEmpty()) {

			var iteratorClauses = setClauses.iterator();
			var iteratorTypes = actionTypes.iterator();
			while (iteratorClauses.hasNext() && iteratorTypes.hasNext()) {
				var type = iteratorTypes.next();
				switch (type) {
					case OnCreate:
						mergeActions.add(MergeAction.of(MergeAction.Type.ON_CREATE, (Set) iteratorClauses.next()));
						break;
					case OnMatch:
						mergeActions.add(MergeAction.of(MergeAction.Type.ON_MATCH, (Set) iteratorClauses.next()));
						break;
					default:
						throw new IllegalArgumentException("Unsupported MergeActionType: " + type);
				}
			}
		}

		return Clauses.merge(List.of(patternElement), mergeActions);
	}

	@Override
	public Clause callClause(InputPosition p, InputPosition namespacePosition, InputPosition procedureNamePosition,
		InputPosition procedureResultPosition, List<String> namespace, String name, List<Expression> arguments,
		boolean yieldAll, List<Expression> resultItems, Where where) {
		return Clauses.callClause(namespace, name, arguments,
			yieldAll && resultItems == null ? List.of(Cypher.asterisk()) : resultItems, where);
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
	public PatternElement namedPattern(Expression v, PatternElement patternElement) {
		return Cypher.path(assertSymbolicName(v)).definedBy(patternElement);
	}

	@Override
	public PatternElement shortestPathPattern(InputPosition p, PatternElement patternElement) {

		Assertions.isInstanceOf(RelationshipPattern.class, patternElement,
			"Only relationship patterns are supported for the shortestPath function.");

		return new ExpressionAsPatternElementWrapper(
			FunctionInvocation.create(PatternElementFunctions.SHORTEST_PATH, patternElement));
	}

	@Override
	public PatternElement allShortestPathsPattern(InputPosition p, PatternElement patternElement) {

		Assertions.isInstanceOf(RelationshipPattern.class, patternElement,
			"Only relationship patterns are supported for the allShortestPaths function.");

		return new ExpressionAsPatternElementWrapper(
			FunctionInvocation.create(PatternElementFunctions.ALL_SHORTEST_PATHS, patternElement));
	}

	@Override
	public PatternElement everyPathPattern(List<Node> nodes, List<PathDetails> relationships) {

		if (nodes.size() == 1 && relationships.isEmpty()) {
			return nodes.get(0);
		}

		if (nodes.isEmpty() || relationships.isEmpty()) {
			throw new IllegalArgumentException(
				"Cannot create a PatternElement from an empty list of nodes or path details.");
		}

		if (nodes.size() != relationships.size() + 1) {
			throw new IllegalArgumentException(
				"Something weird has happened. Got " + nodes.size() + " nodes and " + relationships.size()
				+ " path details.");
		}

		ExposesRelationships<?> relationshipPattern = nodes.get(0);
		for (int i = 1; i < nodes.size(); i++) {
			PathDetails pathDetails = relationships.get(i - 1);
			PathLength length = pathDetails.getLength();

			switch (pathDetails.getDirection()) {
				case LTR:
					relationshipPattern = relationshipPattern.relationshipTo(nodes.get(i), pathDetails.getTypes());
					break;
				case RTL:
					relationshipPattern = relationshipPattern.relationshipFrom(nodes.get(i), pathDetails.getTypes());
					break;
				case UNI:
					relationshipPattern = relationshipPattern.relationshipBetween(nodes.get(i), pathDetails.getTypes());
					break;
				default:
					throw new IllegalArgumentException("Unknown direction type: " + pathDetails.getDirection());
			}

			if (pathDetails.getName() != null) {
				if (relationshipPattern instanceof Relationship) {
					relationshipPattern = ((Relationship) relationshipPattern).named(pathDetails.getName());
				} else {
					relationshipPattern = ((RelationshipChain) relationshipPattern).named(pathDetails.getName());
				}
			}

			if (pathDetails.getProperties() != null) {
				if (relationshipPattern instanceof ExposesProperties) {
					relationshipPattern = (ExposesRelationships<?>) ((ExposesProperties<?>) relationshipPattern)
						.withProperties(pathDetails.getProperties());
				} else {
					relationshipPattern = ((RelationshipChain) relationshipPattern)
						.properties(pathDetails.getProperties());
				}
			}

			if (length != null) {
				if (length.isUnbounded()) {
					relationshipPattern = ((ExposesPatternLengthAccessors<?>) relationshipPattern)
						.unbounded();
				} else {
					relationshipPattern = ((ExposesPatternLengthAccessors<?>) relationshipPattern)
						.length(length.getMinimum(), length.getMaximum());
				}
			}
		}

		return (PatternElement) relationshipPattern;
	}

	@Override
	public Node nodePattern(InputPosition p, Expression v, List<StringPos<InputPosition>> labels, Expression properties,
		Expression predicate) {

		var finalLabels = computeFinalLabelList(LabelParsedEventType.ON_NODE_PATTERN, labels);
		Node node;

		if (finalLabels.length == 0) {
			node = Cypher.anyNode();
		} else {
			var primaryLabel = finalLabels[0];
			var additionalLabels = Arrays.stream(finalLabels).skip(1).collect(Collectors.toList());
			node = Cypher.node(primaryLabel, additionalLabels);
		}

		if (v != null) {
			node = node.named(assertSymbolicName(v));
		}
		if (properties != null) {
			node = node.withProperties((MapExpression) properties);
		}
		return node;
	}

	@Override
	public PathDetails relationshipPattern(InputPosition p, boolean left, boolean right, Expression v,
		List<StringPos<InputPosition>> relTypes,
		PathLength pathLength, Expression properties, boolean legacyTypeSeparator) {

		return PathDetails.of(assertSymbolicName(v), pathLength, left, right, computeFinalTypeList(TypeParsedEventType.ON_RELATIONSHIP_PATTERN, relTypes), (MapExpression) properties);
	}

	@Override
	public PathLength pathLength(InputPosition p, InputPosition pMin, InputPosition pMax, String minLength,
		String maxLength) {
		return PathLength.of(minLength, maxLength);
	}

	@Override
	public Clause loadCsvClause(InputPosition p, boolean headers, Expression source, Expression v,
		String fieldTerminator) {

		Assertions.isInstanceOf(StringLiteral.class, source,
			"Only string literals are supported as source for the LOAD CSV clause.");
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
	public Statement hasCatalog(Statement command) {
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
		SimpleEither<String, Parameter<?>> homeDatabase) {
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
		boolean encrypted, Boolean changeRequired, Boolean suspended, SimpleEither<String, Parameter<?>> homeDatabase,
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

	@SuppressWarnings("HiddenField") // The database options are quite different options than ours ;)
	@Override
	public Statement createDatabase(InputPosition p, boolean replace, SimpleEither<String, Parameter<?>> databaseName,
		boolean ifNotExists, NULL aNull, SimpleEither<Map<String, Expression>, Parameter<?>> options) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement dropDatabase(InputPosition p, SimpleEither<String, Parameter<?>> databaseName, boolean ifExists,
		boolean dumpData, NULL wait) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement alterDatabase(InputPosition p, SimpleEither<String, Parameter<?>> databaseName, boolean ifExists,
		AccessType accessType) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement showDatabase(InputPosition p, NULL scope, Clause yieldExpr, Return returnWithoutGraph,
		Where where) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement startDatabase(InputPosition p, SimpleEither<String, Parameter<?>> databaseName, NULL wait) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement stopDatabase(InputPosition p, SimpleEither<String, Parameter<?>> databaseName, NULL wait) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL databaseScope(InputPosition p, SimpleEither<String, Parameter<?>> databaseName, boolean isDefault,
		boolean isHome) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement createDatabaseAlias(InputPosition p, boolean replace, SimpleEither<String, Parameter<?>> aliasName,
		SimpleEither<String, Parameter<?>> targetName, boolean ifNotExists) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement alterDatabaseAlias(InputPosition p, SimpleEither<String, Parameter<?>> aliasName,
		SimpleEither<String, Parameter<?>> targetName, boolean ifExists) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement dropAlias(InputPosition p, SimpleEither<String, Parameter<?>> aliasName, boolean ifExists) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL wait(boolean wait, long seconds) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Expression newVariable(InputPosition p, String name) {
		return applyCallbackFor(ExpressionCreatedEventType.ON_NEW_VARIABLE, Cypher.name(name));
	}

	@Override
	public Parameter<?> newParameter(InputPosition p, Expression v, ParameterType type) {
		return parameterFromSymbolicName(v);
	}

	@Override
	public Parameter<?> newParameter(InputPosition p, String v, ParameterType type) {
		return Cypher.parameter(v);
	}

	@Override
	public Parameter<?> newSensitiveStringParameter(InputPosition p, Expression v) {
		throw new UnsupportedOperationException("The Cypher-DSL does not support sensitive parameters.");
	}

	@Override
	public Parameter<?> newSensitiveStringParameter(InputPosition p, String v) {
		throw new UnsupportedOperationException("The Cypher-DSL does not support sensitive parameters.");
	}

	@Override
	public Expression oldParameter(InputPosition p, Expression v) {
		return parameterFromSymbolicName(v);
	}

	@NotNull
	static Parameter<?> parameterFromSymbolicName(Expression v) {
		var symbolicName = assertSymbolicName(v);
		return symbolicName == null ?
			Cypher.anonParameter(Cypher.literalNull()) :
			Cypher.parameter(symbolicName.getValue());
	}

	@Override
	public Expression newDouble(InputPosition p, String image) {
		return Cypher.literalOf(Double.parseDouble(image));
	}

	@Override
	public Expression newDecimalInteger(InputPosition p, String image, boolean negated) {
		return Cypher.literalOf(Long.parseUnsignedLong(image) * (negated ? -1 : 1));
	}

	@Override public Expression newHexInteger(InputPosition p, String image, boolean negated) {
		return Cypher.literalOf(Long.parseUnsignedLong(image.replaceFirst("(?i)0x", ""), 16) * (negated ? -1 : 1));
	}

	@Override public Expression newOctalInteger(InputPosition p, String image, boolean negated) {
		return Cypher.literalOf(Long.parseUnsignedLong(image, 8) * (negated ? -1 : 1));
	}

	@Override
	public Literal<String> newString(InputPosition p, String image) {
		return Cypher.literalOf(image);
	}

	@Override
	public Expression newTrueLiteral(InputPosition p) {
		return Cypher.literalTrue();
	}

	@Override
	public Expression newFalseLiteral(InputPosition p) {
		return Cypher.literalFalse();
	}

	@Override
	public Expression newNullLiteral(InputPosition p) {
		return Cypher.literalOf(null);
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
		return Cypher.mapOf(keysAndValues);
	}

	@Override
	public Expression hasLabelsOrTypes(Expression subject, List<StringPos<InputPosition>> labels) {

		Assertions.isInstanceOf(SymbolicName.class, subject, "Can only check for labels or types on symbolic names.");
		return Conditions
			.hasLabelsOrType((SymbolicName) subject, labels.stream().map(v -> v.string).toArray(String[]::new));
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
	public Expression ands(List<Expression> exprs) {
		throw new UnsupportedOperationException();
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
		return Cypher.call(parts).withArgs(arguments.toArray(Expression[]::new)).asFunction(distinct);
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
		if (patternElement instanceof RelationshipPattern) {
			var relationshipPattern = (RelationshipPattern) patternElement;
			if (v != null) {
				ongoingDefinitionWithPattern = Cypher.listBasedOn(Cypher.path(assertSymbolicName(v)).definedBy(relationshipPattern));
			} else {
				ongoingDefinitionWithPattern = Cypher.listBasedOn(relationshipPattern);
			}
		} else if (patternElement instanceof NamedPath) {
			ongoingDefinitionWithPattern = Cypher.listBasedOn((NamedPath) patternElement);
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
	public Expression filterExpression(InputPosition p, Expression v, Expression list, Expression where) {

		return extractExpression(p, v, list, where, v);
	}

	@Override
	public Expression extractExpression(InputPosition p, Expression v, Expression list, Expression where,
		Expression projection) {

		var listComprehension = Cypher.listWith(assertSymbolicName(v)).in(list);
		if (where != null) {
			return listComprehension.where(where.asCondition()).returning(projection);
		}
		return listComprehension.returning(projection);
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

		Assertions.notNull(where, "all(...) requires a WHERE predicate");
		return Predicates.all(assertSymbolicName(v)).in(list).where(where.asCondition());
	}

	@Override
	public Expression anyExpression(InputPosition p, Expression v, Expression list, Expression where) {

		Assertions.notNull(where, "any(...) requires a WHERE predicate");
		return Predicates.any(assertSymbolicName(v)).in(list).where(where.asCondition());
	}

	@Override
	public Expression noneExpression(InputPosition p, Expression v, Expression list, Expression where) {

		Assertions.notNull(where, "none(...) requires a WHERE predicate");
		return Predicates.none(assertSymbolicName(v)).in(list).where(where.asCondition());
	}

	@Override
	public Expression singleExpression(InputPosition p, Expression v, Expression list, Expression where) {

		Assertions.notNull(where, "single(...) requires a WHERE predicate");
		return Predicates.single(assertSymbolicName(v)).in(list).where(where.asCondition());
	}

	@Override
	public Expression patternExpression(InputPosition p, PatternElement patternElement) {

		if (patternElement instanceof ExpressionAsPatternElementWrapper) {
			return ((ExpressionAsPatternElementWrapper) patternElement).getExpression();
		}

		if (patternElement instanceof RelationshipPattern) {
			return new PatternElementAsExpressionWrapper((RelationshipPattern) patternElement);
		}

		throw new UnsupportedOperationException();
	}

	@Override
	public Expression existsSubQuery(InputPosition p, List<PatternElement> patternElements, Expression where) {

		StatementBuilder.OngoingReadingWithoutWhere match = Cypher.match(patternElements);
		if (where != null) {
			return match.where(where.asCondition()).asCondition();
		} else {
			return match.asCondition();
		}
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
	public Where whereClause(InputPosition p, Expression optionalWhere) {
		return Where.from(optionalWhere);
	}

	@Override
	public NULL subqueryInTransactionsParams(InputPosition p, Expression batchSize) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Clause showTransactionsClause(InputPosition p, SimpleEither<List<String>, Parameter<?>> ids, Where where,
		boolean hasYield) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Clause terminateTransactionsClause(InputPosition p, SimpleEither<List<String>, Parameter<?>> ids) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Statement createConstraint(InputPosition p, ConstraintType constraintType, boolean replace,
		boolean ifNotExists, String constraintName, Expression expression, StringPos<InputPosition> label,
		List<Property> properties, SimpleEither<Map<String, Expression>, Parameter<?>> constraintOptions, boolean containsOn,
		ConstraintVersion constraintVersion) {
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
	public NULL databasePrivilege(InputPosition p, NULL aNull, List<NULL> scope, List<NULL> qualifier) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL dbmsPrivilege(InputPosition p, NULL aNull, List<NULL> qualifier) {
		throw new UnsupportedOperationException();
	}

	@Override
	public NULL graphPrivilege(InputPosition p, NULL aNull, List<NULL> scope, NULL aNull2, List<NULL> qualifier) {
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
	public List<NULL> graphScopes(InputPosition p, List<SimpleEither<String, Parameter<?>>> graphNames,
		ScopeType scopeType) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<NULL> databaseScopes(InputPosition p, List<SimpleEither<String, Parameter<?>>> databaseNames,
		ScopeType scopeType) {
		throw new UnsupportedOperationException();
	}
}
