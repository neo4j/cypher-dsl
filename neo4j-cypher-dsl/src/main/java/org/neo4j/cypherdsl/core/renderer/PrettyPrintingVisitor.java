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

import java.util.function.BiConsumer;

import org.neo4j.cypherdsl.build.annotations.RegisterForReflection;
import org.neo4j.cypherdsl.core.Condition;
import org.neo4j.cypherdsl.core.Create;
import org.neo4j.cypherdsl.core.KeyValueMapEntry;
import org.neo4j.cypherdsl.core.MapExpression;
import org.neo4j.cypherdsl.core.Match;
import org.neo4j.cypherdsl.core.Merge;
import org.neo4j.cypherdsl.core.MergeAction;
import org.neo4j.cypherdsl.core.Operator;
import org.neo4j.cypherdsl.core.Parameter;
import org.neo4j.cypherdsl.core.PropertyLookup;
import org.neo4j.cypherdsl.core.Return;
import org.neo4j.cypherdsl.core.Set;
import org.neo4j.cypherdsl.core.StatementContext;
import org.neo4j.cypherdsl.core.Subquery;
import org.neo4j.cypherdsl.core.SubqueryExpression;
import org.neo4j.cypherdsl.core.Unwind;
import org.neo4j.cypherdsl.core.Use;
import org.neo4j.cypherdsl.core.Where;
import org.neo4j.cypherdsl.core.With;
import org.neo4j.cypherdsl.core.ast.ProvidesAffixes;
import org.neo4j.cypherdsl.core.internal.ConstantParameterHolder;
import org.neo4j.cypherdsl.core.renderer.Configuration.IndentStyle;

/**
 * This visitor extends the default visitor and tries to pretty print a Cypher statement,
 * i.e. using some indentation and linebreaks.
 *
 * @author Andreas Berger
 * @author Michael J. Simons
 * @author Christophe Willemsen
 */
@SuppressWarnings("unused")
@RegisterForReflection
class PrettyPrintingVisitor extends DefaultVisitor {

	private final BiConsumer<StringBuilder, Integer> indentionProvider;

	/**
	 * In contrast to the current level in the {@link DefaultVisitor} that contains the
	 * level of elements in the tree.
	 */
	private int indentationLevel;

	private boolean passedFirstReadingOrUpdatingClause;

	PrettyPrintingVisitor(StatementContext statementContext, Configuration configuration) {
		this(statementContext, false, configuration);
	}

	PrettyPrintingVisitor(StatementContext statementContext, boolean renderConstantsAsParameters,
			Configuration configuration) {
		super(statementContext, renderConstantsAsParameters, configuration);

		IndentStyle indentStyle = configuration.getIndentStyle();
		int indentSize = configuration.getIndentSize();

		if (indentStyle == IndentStyle.TAB) {
			this.indentionProvider = (builder, width) -> builder.append("\t".repeat(Math.max(0, width)));
		}
		else {
			this.indentionProvider = (builder, width) -> builder.append(" ".repeat(Math.max(0, width * indentSize)));
		}
	}

	private void indent(int width) {
		this.indentionProvider.accept(this.builder, width);
	}

	@Override
	void enter(Where where) {
		if (this.currentVisitedElements.stream().noneMatch(Return.class::isInstance)) {
			this.builder.append("\n");
			indent(this.indentationLevel);
			this.builder.append("WHERE ");
		}
		else {
			super.enter(where);
		}
	}

	@Override
	void enter(Return returning) {
		trimNewline();
		indent(this.indentationLevel);
		super.enter(returning);
	}

	@Override
	void enter(Unwind unwind) {
		trimNewline();
		indent(this.indentationLevel);
		super.enter(unwind);
	}

	@Override
	void enter(With with) {
		if (this.passedFirstReadingOrUpdatingClause) {
			trimNewline();
			indent(this.indentationLevel);
		}
		this.passedFirstReadingOrUpdatingClause = true;
		super.enter(with);
	}

	@Override
	void enter(Set set) {
		if (this.currentVisitedElements.stream().noneMatch(MergeAction.class::isInstance)) {
			trimNewline();
			indent(this.indentationLevel);
		}
		super.enter(set);
	}

	@Override
	void enter(Match match) {
		if (this.passedFirstReadingOrUpdatingClause) {
			trimNewline();
			indent(this.indentationLevel);
		}
		this.passedFirstReadingOrUpdatingClause = true;
		super.enter(match);
	}

	@Override
	void enter(Create create) {
		if (this.passedFirstReadingOrUpdatingClause) {
			trimNewline();
			indent(this.indentationLevel);
		}
		this.passedFirstReadingOrUpdatingClause = true;
		super.enter(create);
	}

	@Override
	void enter(PropertyLookup propertyLookup) {
		if (this.currentVisitedElements.stream().skip(1).limit(1).anyMatch(MapExpression.class::isInstance)) {
			trimNewline();
			indent(this.indentationLevel);
		}
		super.enter(propertyLookup);
	}

	@Override
	void enter(KeyValueMapEntry map) {
		if (this.indentationLevel > 0) {
			trimNewline();
			indent(this.indentationLevel);
		}
		super.enter(map);
	}

	@Override
	void enter(Condition condition) {

		if (condition instanceof ProvidesAffixes) {
			this.indentationLevel++;
		}
		super.enter(condition);
	}

	@Override
	void leave(Condition condition) {

		if (condition instanceof ProvidesAffixes) {
			this.indentationLevel--;
		}
		super.leave(condition);
	}

	@Override
	void enter(Operator operator) {
		Operator.Type type = operator.getType();
		if (type == Operator.Type.LABEL) {
			return;
		}
		if (type != Operator.Type.PREFIX && operator != Operator.EXPONENTIATION) {
			this.builder.append(" ");
		}
		if (operator == Operator.OR || operator == Operator.AND || operator == Operator.XOR) {
			trimNewline();
			indent(this.indentationLevel);
		}
		this.builder.append(operator.getRepresentation());
		if (type != Operator.Type.POSTFIX && operator != Operator.EXPONENTIATION) {
			this.builder.append(" ");
		}
	}

	@Override
	void enter(MapExpression map) {
		this.indentationLevel++;
		int cnt = 0;
		for (int i = this.builder.length() - 1; i >= 0; --i) {
			if (Character.isWhitespace(this.builder.charAt(i))) {
				++cnt;
			}
			else {
				break;
			}
		}
		this.builder.setLength(this.builder.length() - cnt);
		this.builder.append(" ");
		super.enter(map);
	}

	@Override
	void leave(MapExpression map) {
		this.indentationLevel--;
		trimNewline();
		indent(this.indentationLevel);
		super.leave(map);
	}

	@Override
	void enter(MergeAction onCreateOrMatchEvent) {
		trimNewline();
		indent(1);
		super.enter(onCreateOrMatchEvent);
	}

	@Override
	void enter(Merge merge) {
		if (this.passedFirstReadingOrUpdatingClause) {
			trimNewline();
			indent(this.indentationLevel);
		}
		this.passedFirstReadingOrUpdatingClause = true;
		super.enter(merge);
	}

	@Override
	void enter(SubqueryExpression subquery) {
		super.enter(subquery);
		this.indentationLevel++;
	}

	@Override
	void leave(SubqueryExpression subquery) {
		this.indentationLevel--;
		trimNewline();
		indent(this.indentationLevel);
		this.builder.append("}");
	}

	@Override
	void enter(Subquery subquery) {
		this.passedFirstReadingOrUpdatingClause = true;
		trimNewline();
		indent(this.indentationLevel);
		this.indentationLevel++;
		super.enter(subquery);
	}

	@Override
	void enter(Use use) {
		trimNewline();
		indent(this.indentationLevel);
		super.enter(use);
	}

	@Override
	void leave(Subquery subquery) {
		this.indentationLevel--;
		trimNewline();
		indent(this.indentationLevel);

		this.inSubquery = false;
		this.builder.append("} ");
	}

	private void trimNewline() {
		for (int i = this.builder.length() - 1; i >= 0; i--) {
			if (this.builder.charAt(i) == ' ') {
				this.builder.deleteCharAt(i);
			}
			else {
				break;
			}
		}
		this.builder.append("\n");
	}

	@Override
	void enter(Parameter<?> parameter) {

		Object value = parameter.getValue();
		if (value instanceof ConstantParameterHolder constantParameterHolder) {
			this.builder.append(constantParameterHolder.asString());
		}
		else {
			super.enter(parameter);
		}
	}

}
