/*
 * Copyright (c) 2019-2024 "Neo4j,"
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

import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.Expression;
import org.neo4j.cypherdsl.core.Literal.UnsupportedLiteralException;
import org.neo4j.cypherdsl.core.Parameter;
import org.neo4j.cypherdsl.core.internal.ConstantParameterHolder;

import com.querydsl.core.types.Operator;
import com.querydsl.core.types.Template;

/**
 * @author Michael J. Simons
 * @soundtrack Iron Maiden - The Book Of Souls: Live Chapter
 * @since 2021.1.0
 */
@API(status = INTERNAL, since = "2021.1.0")
public final class CypherContext {

	@SuppressWarnings("FieldCanBeLocal")
	private static final String TO_STRING_VALUE_OF_UNSUPPORTED = "'" + CypherTemplates.UNSUPPORTED_MARKER + "'";

	private final List<Expression> expressions = new ArrayList<>();

	private final Map<Object, Parameter<?>> parameters = new IdentityHashMap<>();

	public void add(Expression expression) {
		this.expressions.add(expression);
	}

	public Expression[] getExpressions() {
		return this.expressions.toArray(new Expression[0]);
	}

	Template getTemplate(Operator op) {

		Template template = CypherTemplates.DEFAULT.getTemplate(op);
		if (template != null) {
			for (Template.Element element : template.getElements()) {
				if (TO_STRING_VALUE_OF_UNSUPPORTED.equals(element.toString())) {
					throw new UnsupportedOperatorException(op);
				}
			}
		}
		return template;
	}

	int getPrecedence(Operator op) {
		return CypherTemplates.DEFAULT.getPrecedence(op);
	}

	Parameter<?> getOrCreateParameterFor(Object object) {

		return parameters.computeIfAbsent(object, o -> {
			Object value;
			try {
				value = new ConstantParameterHolder(o);
			} catch (UnsupportedLiteralException e) {
				value = o;
			}
			return Cypher.anonParameter(value);
		});
	}
}
