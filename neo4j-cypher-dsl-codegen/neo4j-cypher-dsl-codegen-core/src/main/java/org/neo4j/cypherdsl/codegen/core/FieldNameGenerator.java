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
package org.neo4j.cypherdsl.codegen.core;

import org.neo4j.cypherdsl.core.utils.Strings;

/**
 * A generator for constant field names.
 *
 * @author Michael J. Simons
 * @since 2021.1.0
 */
interface FieldNameGenerator {

	enum Default implements FieldNameGenerator {
		INSTANCE
	}

	default String generate(String name) {

		StringBuilder sb = new StringBuilder();

		int codePoint;
		int previousIndex = 0;
		boolean prevWasLower = false;
		boolean nextIsLower = false;
		int i = 0;
		while (i < name.length()) {
			codePoint = name.codePointAt(i);
			if (Strings.isValidJavaIdentifierPartAt(i, codePoint)) {
				if (nextIsLower || Character.isLowerCase(codePoint)) {
					prevWasLower = true;
					nextIsLower = false;
					if (sb.length() > 0 && !Character.isLetter(name.codePointAt(previousIndex))) {
						sb.append("_");
					}
					codePoint = Character.toUpperCase(codePoint);
				} else if (sb.length() > 0 && (
					prevWasLower ||
					i + 1 < name.length() && Character.isLowerCase(name.codePointAt(i + 1))
				)) {
					sb.append("_");
					nextIsLower = true;
				}
				sb.append(Character.toChars(codePoint));
			}
			previousIndex = i;
			i += Character.charCount(codePoint);
		}
		return sb.toString();
	}
}
