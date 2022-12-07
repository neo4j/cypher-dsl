/*
 * Copyright (c) 2019-2022 "Neo4j,"
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

import static org.apiguardian.api.API.Status.INTERNAL;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Optional;

import org.apiguardian.api.API;
import org.jetbrains.annotations.NotNull;
import org.neo4j.cypherdsl.core.ast.ProvidesAffixes;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * A condition that consists of one or two {@link Condition conditions} connected by a
 * <a href="https://en.wikipedia.org/wiki/Logical_connective">Logical connective (operator)</a>.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = INTERNAL, since = "1.0")
final class CompoundCondition implements Condition, ProvidesAffixes {

	/**
	 * The empty, compound condition.
	 */
	private static final CompoundCondition EMPTY_CONDITION = new CompoundCondition(null);
	static final EnumSet<Operator> VALID_OPERATORS = EnumSet.of(Operator.AND, Operator.OR, Operator.XOR);

	static CompoundCondition create(Condition left, Operator operator, Condition right) {

		Assertions.isTrue(VALID_OPERATORS.contains(operator),
			"Operator " + operator + " is not a valid operator for a compound condition.");

		Assertions.notNull(left, "Left hand side condition is required.");
		Assertions.notNull(operator, "Operator is required.");
		Assertions.notNull(right, "Right hand side condition is required.");
		return new CompoundCondition(operator)
			.add(operator, left)
			.add(operator, right);
	}

	static CompoundCondition copyOf(CompoundCondition other) {

		CompoundCondition result = new CompoundCondition(other.operator);
		result.conditions.addAll(other.conditions);
		return result;
	}

	static CompoundCondition empty() {

		return EMPTY_CONDITION;
	}

	private final Operator operator;

	private final List<Condition> conditions;

	private CompoundCondition(Operator operator) {
		this.operator = operator;
		this.conditions = new ArrayList<>();
	}

	@NotNull
	@Override
	public Condition and(Condition condition) {
		return this.add(Operator.AND, condition);
	}

	@NotNull
	@Override
	public Condition or(Condition condition) {
		return this.add(Operator.OR, condition);
	}

	@NotNull
	@Override
	public Condition xor(Condition condition) {
		return this.add(Operator.XOR, condition);
	}

	private CompoundCondition add(
		Operator chainingOperator,
		Condition condition
	) {
		if (this == EMPTY_CONDITION) {
			return new CompoundCondition(chainingOperator).add(chainingOperator, condition);
		}

		if (condition instanceof CompoundCondition compoundCondition && !compoundCondition.hasConditions()) {
			return this;
		}

		if (condition instanceof CompoundCondition compoundCondition) {
			CompoundCondition target;
			if (this.operator == chainingOperator && chainingOperator == compoundCondition.operator) {
				target = CompoundCondition.copyOf(this);
			} else {
				CompoundCondition inner = new CompoundCondition(chainingOperator);
				if (this.hasConditions()) {
					inner.conditions.add(this);
				}
				target = inner;
			}
			if (compoundCondition.canBeFlattenedWith(chainingOperator)) {
				target.conditions.addAll(compoundCondition.conditions);
			} else {
				target.conditions.add(compoundCondition);
			}

			return target;
		}

		if (this.operator == chainingOperator) {
			CompoundCondition target = CompoundCondition.copyOf(this);
			target.conditions.add(condition);
			return target;
		}

		return CompoundCondition.create(this, chainingOperator, condition);
	}

	boolean hasConditions() {
		return !(this == EMPTY_CONDITION || this.conditions.isEmpty());
	}

	/**
	 * @param operatorBefore The operator that is to be used before this condition
	 * @return True if all conditions in this condition are either simple or compound annotation with the same boolean operator as {@code operatorBefore}
	 */
	private boolean canBeFlattenedWith(Operator operatorBefore) {

		if (this.operator != operatorBefore) {
			return false;
		}
		for (Condition c : this.conditions) {
			if (c instanceof CompoundCondition compoundCondition && compoundCondition.operator != operatorBefore) {
				return false;
			}
		}
		return true;
	}

	@Override
	public void accept(Visitor visitor) {

		// There is nothing to visit here
		if (this.conditions.isEmpty()) {
			return;
		}

		// Fold single condition
		boolean hasManyConditions = this.conditions.size() > 1;
		if (hasManyConditions) {
			visitor.enter(this);
		}

		// The first nested condition does not need an operator
		acceptVisitorWithOperatorForChildCondition(visitor, null, conditions.get(0));

		// All others do
		if (hasManyConditions) {
			for (Condition condition : conditions.subList(1, conditions.size())) {
				acceptVisitorWithOperatorForChildCondition(visitor, operator, condition);
			}
			visitor.leave(this);
		}
	}

	private static void acceptVisitorWithOperatorForChildCondition(
		Visitor visitor, Operator operator, Condition condition
	) {
		Visitable.visitIfNotNull(operator, visitor);
		condition.accept(visitor);
	}

	@Override
	public Optional<String> getPrefix() {
		return Optional.of("(");
	}

	@Override
	public Optional<String> getSuffix() {
		return Optional.of(")");
	}
}
