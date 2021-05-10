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
package org.neo4j.cypherdsl.core;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apiguardian.api.API;
import org.apiguardian.api.API.Status;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.internal.LiteralBase;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * Generates a raw cypher literal. The factory method is able to replace {@code $E} placeholders with expressions passed
 * to it. To use a {@literal $E} escape it as {$literal \$E}.
 *
 * @author Michael J. Simons
 * @soundtrack Foo Fighters - Echoes, Silence, Patience &amp; Grace
 * @since 2021.0.2
 */
@API(status = Status.INTERNAL)
final class RawLiteral implements Expression {

	private static final Pattern EXPRESSION_PATTERN = Pattern.compile("((\\\\?\\$(\\w+))(?:\\s*|$))");
	private static final String EXPRESSION_PLACEHOLDER = "$E";

	static class RawElement extends LiteralBase<String> {

		RawElement(String content) {
			super(RawLiteral.unescapeEscapedPlaceholders(content));
		}

		@Override
		public String asString() {

			return super.getContent();
		}
	}

	static RawLiteral create(String format, Object... mixedArgs) {

		Assertions.hasText(format, "Cannot create a raw literal without a format.");

		Map<String, Parameter> parameters = new HashMap<>();
		List<Object> all = new ArrayList<>();
		for (Object mixedArg : mixedArgs) {
			if (mixedArg instanceof Parameter) {
				Parameter parameter = (Parameter) mixedArg;
				if (!parameter.isAnon()) {
					parameters.put(parameter.getName(), parameter);
				}
			}
			all.add(mixedArg);
		}

		Matcher m;
		if (!parameters.isEmpty()) {
			m = EXPRESSION_PATTERN.matcher(format);
			// "Reserve" all placeholders that match actual parameter names by removing them from the all
			// args list so that they are taken with precedence.
			while (m.find()) {
				if (parameters.containsKey(m.group(3))) {
					all.remove(parameters.get(m.group(3)));
				}
			}
		}

		List<Expression> content = new ArrayList<>();
		m = EXPRESSION_PATTERN.matcher(format);
		int i = 0;
		int cnt = 0;
		while (m.find()) {
			if (EXPRESSION_PLACEHOLDER.equals(m.group(2))) {
				content.add(new RawElement(format.substring(i, m.start(2))));
				if (cnt >= all.size()) {
					throw new IllegalArgumentException(
						"Too few arguments for the raw literal format `" + format + "`.");
				}
				content.add(getMixedArg(all.get(cnt++)));
				i = m.end(2);
			} else if (parameters.containsKey(m.group(3))) {
				Parameter e = parameters.get(m.group(3));

				content.add(new RawElement(format.substring(i, m.start(2))));
				content.add(e);
				i = m.end(2);

				all.remove(e);
			} else {
				content.add(new RawElement(format.substring(i, m.end())));
				i = m.end();
			}
		}

		if (cnt < all.size()) {
			throw new IllegalArgumentException("Too many arguments for the raw literal format `" + format + "`.");
		}

		content.add(new RawElement(format.substring(i)));

		return new RawLiteral(Collections.unmodifiableList(content));
	}

	private final List<Expression> content;

	RawLiteral(List<Expression> content) {
		this.content = content;
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		content.forEach(expression -> expression.accept(visitor));
		visitor.leave(this);
	}

	private static String unescapeEscapedPlaceholders(String content) {
		return content.replaceAll("\\\\\\$E", "\\$E");
	}

	private static Expression getMixedArg(Object argument) {
		if (argument instanceof Expression) {
			return (Expression) argument;
		}
		return Cypher.literalOf(argument);
	}
}
