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
package org.neo4j.cypherdsl.core.querydsl;

import static org.apiguardian.api.API.Status.INTERNAL;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.MapExpression;
import org.neo4j.cypherdsl.core.support.UnsupportedLiteralException;

import com.querydsl.core.types.Constant;
import com.querydsl.core.types.Expression;
import com.querydsl.core.types.FactoryExpression;
import com.querydsl.core.types.Operation;
import com.querydsl.core.types.ParamExpression;
import com.querydsl.core.types.Path;
import com.querydsl.core.types.SubQueryExpression;
import com.querydsl.core.types.Template;
import com.querydsl.core.types.TemplateExpression;
import com.querydsl.core.types.Visitor;

/**
 * This is basically a copy of Query-DSL's {@link com.querydsl.core.types.ToStringVisitor}. The main purpose of the string
 * generated here is to be used with our {@link Cypher#raw(String, Object...)} feature, that allows to insert arbitrary
 * query fragments into the AST. It is easier to render the Query-DSL fragments than recreating our AST from Query-DSL.
 * <p>
 * The main difference in the original {@code ToStringVisitor} is to be found in {@link #visit(ParamExpression, CypherContext)}
 * and {@link #visit(Constant, CypherContext)}. Both methods will use the {@literal $E} notation to indicate an expression
 * for the {@code RawLiteral} and add the expression (either a literal or parameter) to the {@link CypherContext}. After
 * all rendering has been done by Query-DSL, the adapter will take the renderer string as a format string and pass it on
 * to {@link Cypher#raw(String, Object...)} along with all expressions collected along the way.
 *
 * @author Michael J. Simons
 * @soundtrack Iron Maiden - The Book Of Souls: Live Chapter
 * @since 2021.1.0
 */
@API(status = INTERNAL, since = "2021.1.0")
public final class ToCypherFormatStringVisitor implements Visitor<String, CypherContext> {

	public static final ToCypherFormatStringVisitor INSTANCE = new ToCypherFormatStringVisitor();

	private ToCypherFormatStringVisitor() {
	}

	@Override
	public String visit(FactoryExpression<?> e, CypherContext context) {

		final StringBuilder builder = new StringBuilder();
		builder.append("new ").append(e.getType().getSimpleName()).append("(");
		boolean first = true;
		for (Expression<?> arg : e.getArgs()) {
			if (!first) {
				builder.append(", ");
			}
			builder.append(arg.accept(this, context));
			first = false;
		}
		builder.append(")");
		return builder.toString();
	}

	@Override
	public String visit(Operation<?> o, CypherContext context) {

		final Template template = context.getTemplate(o.getOperator());
		if (template != null) {
			final int precedence = context.getPrecedence(o.getOperator());
			final StringBuilder builder = new StringBuilder();
			for (Template.Element element : template.getElements()) {
				final Object rv = element.convert(o.getArgs());
				if (rv instanceof Expression) {
					if (precedence > -1 && rv instanceof Operation && precedence < context.getPrecedence(((Operation<?>) rv).getOperator())) {
						builder
							.append("(")
							.append(((Expression<?>) rv).accept(this, context))
							.append(")");
						continue;
					}
					builder.append(((Expression<?>) rv).accept(this, context));
				} else {
					builder.append(rv.toString());
				}
			}
			return builder.toString();
		} else {
			return "unknown operation with operator " + o.getOperator().name() + " and args " + o.getArgs();
		}
	}

	@Override
	public String visit(ParamExpression<?> param, CypherContext context) {

		context.add(Cypher.parameter(param.getName()));
		return "$E";
	}

	@Override
	public String visit(Path<?> p, CypherContext context) {

		final Path<?> parent = p.getMetadata().getParent();
		final Object elem = p.getMetadata().getElement();
		if (parent != null) {
			Template pattern = context.getTemplate(p.getMetadata().getPathType());
			if (pattern != null) {
				final List<?> args = Arrays.asList(parent, elem);
				final StringBuilder builder = new StringBuilder();
				for (Template.Element element : pattern.getElements()) {
					Object rv = element.convert(args);
					if (rv instanceof Expression) {
						builder.append(((Expression<?>) rv).accept(this, context));
					} else {
						builder.append(rv.toString());
					}
				}
				return builder.toString();
			} else {
				throw new IllegalArgumentException("No pattern for " + p.getMetadata().getPathType());
			}
		} else {
			return elem.toString();
		}
	}

	@Override
	public String visit(SubQueryExpression<?> expr, CypherContext context) {
		return expr.getMetadata().toString();
	}

	@Override
	public String visit(TemplateExpression<?> expr, CypherContext context) {

		final StringBuilder builder = new StringBuilder();
		for (Template.Element element : expr.getTemplate().getElements()) {
			Object rv = element.convert(expr.getArgs());
			if (rv instanceof Expression) {
				builder.append(((Expression<?>) rv).accept(this, context));
			} else {
				builder.append(rv.toString());
			}
		}
		return builder.toString();
	}

	@Override
	public String visit(Constant<?> expr, CypherContext context) {

		Object constantValue = expr.getConstant();
		if (constantValue instanceof Map) {
			context.add(createMapExpressionOfLiterals((Map<?, ?>) constantValue));
		} else {
			context.add(Cypher.literalOf(constantValue));
		}
		return "$E";
	}

	private MapExpression createMapExpressionOfLiterals(Map<?, ?> aMap) {

		Map<String, Object> baseMap = new HashMap(aMap.size());
		aMap.forEach((k, v) -> {
			if (k instanceof CharSequence) {
				baseMap.put(((CharSequence) k).toString(), Cypher.literalOf(v));
			} else {
				throw new UnsupportedLiteralException("The keys of a literal map must be Strings", k);
			}
		});
		return Cypher.asExpression(baseMap);
	}
}
