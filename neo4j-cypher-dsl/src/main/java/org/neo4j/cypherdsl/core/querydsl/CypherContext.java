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

import java.util.ArrayList;
import java.util.List;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.Expression;

import com.querydsl.core.types.Operator;
import com.querydsl.core.types.Template;
import com.querydsl.core.types.Templates;

/**
 * @author Michael J. Simons
 * @soundtrack Iron Maiden - The Book Of Souls: Live Chapter
 * @since 2021.1.0
 */
@API(status = INTERNAL, since = "2021.1.0")
public final class CypherContext {

	private final Templates templates = CypherTemplates.DEFAULT;

	private final List<Expression> expressions = new ArrayList<>();
	private final String TO_STRING_VALUE_OF_UNSUPPORTED = "'" + CypherTemplates.UNSUPPORTED_MARKER + "'";

	public void add(Expression expression) {
		this.expressions.add(expression);
	}

	public Expression[] getExpressions() {
		return this.expressions.toArray(new Expression[this.expressions.size()]);
	}

	Template getTemplate(Operator op) {

		Template template = templates.getTemplate(op);
		for (Template.Element element : template.getElements()) {
			if (TO_STRING_VALUE_OF_UNSUPPORTED.equals(element.toString())) {
				throw new UnsupportedOperatorException(op);
			}
		}
		return template;
	}

	int getPrecedence(Operator op) {
		return templates.getPrecedence(op);
	}
}
