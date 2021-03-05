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
package org.neo4j.cypherdsl.codegen.core;

import static org.apiguardian.api.API.Status.INTERNAL;

import org.apiguardian.api.API;

/**
 * Default generator for creating names of relationship classes. It uses the given type as class name, generates a valid type
 * from it and then tries to derive a camel cased name.
 *
 * @author Michael J. Simons
 * @since 2021.1.0
 */
@API(status = INTERNAL, since = "2021.1.0")
final class RelationshipNameGenerator extends AbstractClassNameGenerator implements ClassNameGenerator {

	@Override
	public String generate(String suggestedName) {

		final StringBuilder sb = generateTypeName(suggestedName);
		int i = 0;
		int last = 0;
		while (i <= sb.length()) {
			if (i == sb.length() || sb.charAt(i) == '_') {
				char copy = sb.charAt(last);
				sb.setCharAt(last, Character.toUpperCase(sb.charAt(last)));
				while (++last < i) {
					boolean flip = last + 1 == i && Character.isUpperCase(copy) || last + 1 < i && Character.isUpperCase(sb.charAt(last + 1));
					copy = sb.charAt(last);
					if (flip) {
						sb.setCharAt(last, Character.toLowerCase(sb.charAt(last)));
					}
				}
				if (i != sb.length()) {
					sb.deleteCharAt(i);
				}
				last = i;
			}
			++i;
		}

		return sb.toString();
	}
}
