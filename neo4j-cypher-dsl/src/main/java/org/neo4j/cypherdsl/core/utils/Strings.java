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
package org.neo4j.cypherdsl.core.utils;

import java.util.concurrent.ThreadLocalRandom;

import org.apiguardian.api.API;

/**
 * The usual, static class with helper methods centered around missing functionality in
 * {@link String}. Not supported for external use in any way.
 *
 * @author Michael J. Simons
 * @since 2020.1.0
 */
@API(status = API.Status.INTERNAL, since = "2020.1.0")
public final class Strings {

	private Strings() {
	}

	/**
	 * Returns true, if the string is neither null nor empty nor blank.
	 * @param str a string to be checked for text.
	 * @return true, if the string is neither null nor empty nor blank
	 */
	public static boolean hasText(String str) {
		return str != null && !str.isBlank();
	}

	/**
	 * Returns a random identifier that is a valid identifier.
	 * @param length the length of the identifier to generate.
	 * @return a random identifier that is a valid identifier
	 */
	public static String randomIdentifier(int length) {

		int leftLimit = 65; // letter 'A'
		int rightLimit = 122; // letter 'z'
		// I really need only random string values, this is fine
		@SuppressWarnings("squid:S2245")
		ThreadLocalRandom random = ThreadLocalRandom.current();

		return random.ints(leftLimit, rightLimit + 1)
			.filter(Character::isLetter)
			.limit(length)
			.collect(StringBuilder::new, StringBuilder::appendCodePoint, StringBuilder::append)
			.toString();
	}

}
