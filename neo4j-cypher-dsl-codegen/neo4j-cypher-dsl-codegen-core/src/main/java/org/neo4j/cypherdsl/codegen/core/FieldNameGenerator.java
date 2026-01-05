/*
 * Copyright (c) 2019-2026 "Neo4j,"
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

/**
 * A generator for constant field names.
 *
 * @author Michael J. Simons
 * @since 2021.1.0
 */
@FunctionalInterface
public interface FieldNameGenerator {

	/**
	 * Generates a new name for constant field.
	 * @param name the name of a node or relationship
	 * @return a valid constant field name
	 */
	String generate(String name);

	/**
	 * Single instance of the default {@link FieldNameGenerator}.
	 */
	enum Default implements FieldNameGenerator {

		/**
		 * Singleton holder.
		 */
		INSTANCE;

		@Override
		public String generate(String name) {
			StringBuilder sb = new StringBuilder();

			int codePoint;
			int previousIndex = 0;
			boolean prevWasLower = false;
			boolean nextIsLower = false;
			int i = 0;
			while (i < name.length()) {
				codePoint = name.codePointAt(i);
				if (Identifiers.isValidAt(i, codePoint)) {
					if (nextIsLower || Character.isLowerCase(codePoint)) {
						prevWasLower = true;
						nextIsLower = false;
						if (!(sb.isEmpty() || Character.isLetter(name.codePointAt(previousIndex)))) {
							sb.append("_");
						}
						codePoint = Character.toUpperCase(codePoint);
					}
					else if (!sb.isEmpty() && (prevWasLower
							|| i + 1 < name.length() && Character.isLowerCase(name.codePointAt(i + 1)))) {
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

}
