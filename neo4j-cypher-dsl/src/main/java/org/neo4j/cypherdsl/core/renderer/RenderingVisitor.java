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
package org.neo4j.cypherdsl.core.renderer;

import java.util.Deque;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.neo4j.cypherdsl.core.AliasedExpression;
import org.neo4j.cypherdsl.core.Arguments;
import org.neo4j.cypherdsl.core.Asterisk;
import org.neo4j.cypherdsl.core.Case;
import org.neo4j.cypherdsl.core.CompoundCondition;
import org.neo4j.cypherdsl.core.Create;
import org.neo4j.cypherdsl.core.Delete;
import org.neo4j.cypherdsl.core.Distinct;
import org.neo4j.cypherdsl.core.ExistentialSubquery;
import org.neo4j.cypherdsl.core.FunctionInvocation;
import org.neo4j.cypherdsl.core.KeyValueMapEntry;
import org.neo4j.cypherdsl.core.Limit;
import org.neo4j.cypherdsl.core.ListComprehension;
import org.neo4j.cypherdsl.core.ListExpression;
import org.neo4j.cypherdsl.core.ListOperator;
import org.neo4j.cypherdsl.core.Literal;
import org.neo4j.cypherdsl.core.MapExpression;
import org.neo4j.cypherdsl.core.Match;
import org.neo4j.cypherdsl.core.Merge;
import org.neo4j.cypherdsl.core.MergeAction;
import org.neo4j.cypherdsl.core.Named;
import org.neo4j.cypherdsl.core.Namespace;
import org.neo4j.cypherdsl.core.NestedExpression;
import org.neo4j.cypherdsl.core.Node;
import org.neo4j.cypherdsl.core.NodeLabel;
import org.neo4j.cypherdsl.core.Operation;
import org.neo4j.cypherdsl.core.Operator;
import org.neo4j.cypherdsl.core.Order;
import org.neo4j.cypherdsl.core.Parameter;
import org.neo4j.cypherdsl.core.PatternComprehension;
import org.neo4j.cypherdsl.core.ProcedureCall;
import org.neo4j.cypherdsl.core.ProcedureName;
import org.neo4j.cypherdsl.core.Properties;
import org.neo4j.cypherdsl.core.PropertyLookup;
import org.neo4j.cypherdsl.core.Relationship;
import org.neo4j.cypherdsl.core.RelationshipLength;
import org.neo4j.cypherdsl.core.RelationshipTypes;
import org.neo4j.cypherdsl.core.Remove;
import org.neo4j.cypherdsl.core.Return;
import org.neo4j.cypherdsl.core.Set;
import org.neo4j.cypherdsl.core.Skip;
import org.neo4j.cypherdsl.core.SortItem;
import org.neo4j.cypherdsl.core.Subquery;
import org.neo4j.cypherdsl.core.SymbolicName;
import org.neo4j.cypherdsl.core.UnionPart;
import org.neo4j.cypherdsl.core.Unwind;
import org.neo4j.cypherdsl.core.Where;
import org.neo4j.cypherdsl.core.With;
import org.neo4j.cypherdsl.core.YieldItems;
import org.neo4j.cypherdsl.core.support.ReflectiveVisitor;
import org.neo4j.cypherdsl.core.support.TypedSubtree;
import org.neo4j.cypherdsl.core.support.Visitable;
import org.neo4j.cypherdsl.core.utils.Strings;

/**
 * This is a simple (some would call it naive) implementation of a visitor to the Cypher AST created by the Cypher builder
 * based on the {@link ReflectiveVisitor reflective visitor}.
 * <p>
 * It takes care of separating elements of sub trees containing the element type with a separator and provides pairs of
 * {@code enter} / {@code leave} for the structuring elements of the Cypher AST as needed.
 * <p>
 * This rendering visitor is not meant to be used outside framework code and we don't give any guarantees on the format
 * being output apart from that it works within the constraints of SDN-RX.
 *
 * @author Michael J. Simons
 * @author Gerrit Meier
 * @since 1.0
 */
class RenderingVisitor extends ReflectiveVisitor {

	private static final Pattern LABEL_AND_TYPE_QUOTATION = Pattern.compile("`");

	/**
	 * Target of all rendering.
	 */
	private final StringBuilder builder = new StringBuilder();

	/**
	 * This keeps track on which level of the tree a separator is needed.
	 */
	private final java.util.Map<Integer, AtomicReference<String>> separatorOnLevel = new ConcurrentHashMap<>();

	/**
	 * Keeps track of named objects that have been already visited.
	 */
	private final java.util.Set<Named> visitedNamed = new HashSet<>();

	/**
	 * Keeps track if currently in an aliased expression so that the content can be skipped when already visited.
	 */
	private final Deque<AliasedExpression> currentAliasedElements = new LinkedList<>();

	/**
	 * Keeps track of unresolved symbolic names.
	 */
	private final Map<SymbolicName, String> resolvedSymbolicNames = new ConcurrentHashMap<>();

	/**
	 * The current level in the tree of cypher elements.
	 */
	private int currentLevel = 0;

	/**
	 * Will be set to true when entering an already visited node.
	 */
	private boolean skipNodeContent = false;

	private void enableSeparator(int level, boolean on) {
		if (on) {
			separatorOnLevel.put(level, new AtomicReference<>(""));
		} else {
			separatorOnLevel.remove(level);
		}
	}

	private Optional<AtomicReference<String>> separatorOnCurrentLevel() {

		return Optional.ofNullable(separatorOnLevel.get(currentLevel));
	}

	private String resolve(SymbolicName symbolicName) {

		return this.resolvedSymbolicNames.computeIfAbsent(symbolicName, k -> {
			String value = k.getValue();
			if (Strings.hasText(value)) {
				return escapeIfNecessary(symbolicName.getValue());
			}
			return String.format("%s%03d", Strings.randomIdentifier(8), resolvedSymbolicNames.size());
		});
	}

	@Override
	protected boolean preEnter(Visitable visitable) {

		Visitable lastAliased = currentAliasedElements.peek();
		if (skipNodeContent || visitableToAliased.containsValue(lastAliased)) {
			return false;
		}

		if (visitable instanceof AliasedExpression) {
			currentAliasedElements.push((AliasedExpression) visitable);
		}

		int nextLevel = ++currentLevel + 1;
		if (visitable instanceof TypedSubtree) {
			enableSeparator(nextLevel, true);
		}

		separatorOnCurrentLevel().ifPresent(ref -> builder.append(ref.getAndSet("")));

		return !skipNodeContent;
	}

	@Override
	protected void postLeave(Visitable visitable) {

		separatorOnCurrentLevel().ifPresent(ref -> ref.set(", "));

		if (visitable instanceof TypedSubtree) {
			enableSeparator(currentLevel + 1, false);
		}

		if (currentAliasedElements.peek() == visitable) {
			currentAliasedElements.pop();
		}

		--currentLevel;
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
			case ON_CREATE:
				builder.append("ON CREATE");
				break;
			case ON_MATCH:
				builder.append("ON MATCH");
				break;
		}
		builder.append(" ");
	}

	void enter(Distinct distinct) {
		builder.append("DISTINCT ");
	}

	void enter(Return returning) {
		builder.append("RETURN ");
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

	void enter(AliasedExpression aliased) {

		if (this.visitableToAliased.containsValue(aliased)) {
			builder.append(escapeIfNecessary(aliased.getAlias()));
		}
	}

	void leave(AliasedExpression aliased) {

		if (!this.visitableToAliased.containsValue(aliased)) {
			builder.append(" AS ").append(escapeIfNecessary(aliased.getAlias()));
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

		String propertyKeyName = propertyLookup.getPropertyKeyName();
		builder
			.append(".")
			.append(Asterisk.INSTANCE.getContent().equals(propertyKeyName) ? propertyKeyName : escapeIfNecessary(propertyKeyName));
	}

	void enter(FunctionInvocation functionInvocation) {
		builder
			.append(functionInvocation.getFunctionName())
			.append("(");
	}

	void leave(FunctionInvocation functionInvocation) {
		builder
			.append(")");
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
		if (type != Operator.Type.PREFIX && operator != Operator.EXPONENTIATION) {
			builder.append(" ");
		}
		builder.append(operator.getRepresentation());
		if (type != Operator.Type.POSTFIX && operator != Operator.EXPONENTIATION) {
			builder.append(" ");
		}
	}

	void leave(Operation operation) {

		if (operation.needsGrouping()) {
			builder.append(")");
		}
	}

	void enter(CompoundCondition compoundCondition) {
		builder.append("(");
	}

	void leave(CompoundCondition compoundCondition) {
		builder.append(")");
	}

	void enter(Literal<?> expression) {
		builder.append(expression.asString());
	}

	void enter(Node node) {

		builder.append("(");

		// This is only relevant for nodes in relationships.
		// Otherwise all the labels would be rendered again.
		node.getSymbolicName().map(SymbolicName::getValue).ifPresent(symbolicName -> {
			skipNodeContent = visitedNamed.contains(node);
			visitedNamed.add(node);

			if (skipNodeContent) {
				builder.append(symbolicName);
			}
		});
	}

	void leave(Node node) {

		builder.append(")");

		skipNodeContent = false;
	}

	void enter(NodeLabel nodeLabel) {

		escapeName(nodeLabel.getValue()).ifPresent(label -> builder.append(Symbols.NODE_LABEL_START).append(label));
	}

	void enter(Properties properties) {

		builder.append(" ");
	}

	void enter(SymbolicName symbolicName) {
		builder.append(resolve(symbolicName));
	}

	void enter(Relationship.Details details) {

		Relationship.Direction direction = details.getDirection();
		builder.append(direction.getSymbolLeft());
		if (details.hasContent()) {
			builder.append("[");
		}
	}

	void enter(RelationshipTypes types) {

		builder
			.append(types.getValues().stream()
				.map(RenderingVisitor::escapeName)
				.map(Optional::get).collect(Collectors.joining(Symbols.REL_TYP_SEPARATOR, Symbols.REL_TYPE_START, "")));
	}

	void enter(RelationshipLength length) {

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

		Relationship.Direction direction = details.getDirection();
		if (details.hasContent()) {
			builder.append("]");
		}
		builder.append(direction.getSymbolRight());
	}

	void enter(Parameter parameter) {

		builder.append("$").append(parameter.getName());
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

		builder.append(" AS ")
			.append(unwind.getVariable())
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

	void enter(Case.CaseWhenThen caseWhenExpression) {
		builder.append(" WHEN ");
	}

	void leave(Case.CaseWhenThen caseWhenExpression) {
		builder.append(" THEN ");
	}

	void enter(Case.CaseElse caseElseExpression) {
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

	void enter(Arguments arguments) {

		builder.append("(");
	}

	void leave(Arguments arguments) {

		builder.append(")");
	}

	void enter(YieldItems yieldItems) {

		builder.append(" YIELD ");
	}

	void leave(ProcedureCall procedureCall) {

		builder.append(" ");
	}

	void enter(ListOperator.Details details) {

		builder.append("[");
	}

	void leave(ListOperator.Details details) {

		builder.append("]");
	}

	void enter(Enum<?> statement) {

		builder.append(statement.name()).append(" ");
	}

	void enter(Subquery subquery) {

		builder.append("CALL {");
	}

	void leave(Subquery subquery) {

		builder.append("} ");
	}

	void enter(ExistentialSubquery subquery) {

		builder.append("EXISTS {");
	}

	void leave(ExistentialSubquery subquery) {

		// Trimming the inner match without having to do this in the match (looking up if inside subquery).
		builder.replace(builder.length() - 1, builder.length(), "}");
	}

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
	static Optional<String> escapeName(CharSequence unescapedName) {

		if (unescapedName == null) {
			return Optional.empty();
		}

		Matcher matcher = LABEL_AND_TYPE_QUOTATION.matcher(unescapedName);
		return Optional.of(String.format(Locale.ENGLISH, "`%s`", matcher.replaceAll("``")));
	}

	static String escapeIfNecessary(String potentiallyNonIdentifier) {

		if (potentiallyNonIdentifier == null || Strings.isIdentifier(potentiallyNonIdentifier) || potentiallyNonIdentifier.trim().isEmpty()) {
			return potentiallyNonIdentifier;
		}

		Matcher matcher = LABEL_AND_TYPE_QUOTATION.matcher(potentiallyNonIdentifier);
		return String.format(Locale.ENGLISH, "`%s`", matcher.replaceAll("``"));
	}
}
