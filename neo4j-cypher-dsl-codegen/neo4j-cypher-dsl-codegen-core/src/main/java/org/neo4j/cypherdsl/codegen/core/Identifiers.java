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
package org.neo4j.cypherdsl.codegen.core;

/**
 * Utilities for dealing with identifiers.
 *
 * @author Michael J. Simons
 */
final class Identifiers {

	/**
	 * A convenience method to decide whether a given {@code codePoint} is a valid Java identifier at the given position {@code p}.
	 *
	 * @param p         Position on which the {@code codePoint} is supposed to be used as identifier
	 * @param codePoint A codepoint
	 * @return True if the codePoint could be used as part of an identifier at the given position
	 */
	static boolean isValidAt(int p, int codePoint) {
		return p == 0 && Character.isJavaIdentifierStart(codePoint) || p > 0 && Character.isJavaIdentifierPart(
			codePoint);
	}

	private Identifiers() {
	}
}
