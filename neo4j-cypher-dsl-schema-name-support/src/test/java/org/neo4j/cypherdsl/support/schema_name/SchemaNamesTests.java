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
package org.neo4j.cypherdsl.support.schema_name;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Timeout.ThreadMode;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.assertThatNoException;

/**
 * @author Michael J. Simons
 */
class SchemaNamesTests {

	@Test
	void shouldDealWithNull() {
		assertThat(SchemaNames.sanitize(null)).isEmpty();
	}

	@Test
	void shouldDealWithEmpty() {
		assertThat(SchemaNames.sanitize("")).isEmpty();
	}

	@Test
	void shouldThrowOnInvalidVersions() {
		assertThatIllegalArgumentException().isThrownBy(() -> SchemaNames.sanitize("whatever", false, 2, 3))
			.withMessage("Unsupported major version: 2");
		assertThatIllegalArgumentException().isThrownBy(() -> SchemaNames.sanitize("whatever", false, 6, 0))
			.withMessage("Unsupported major version: 6");
		assertThatIllegalArgumentException().isThrownBy(() -> SchemaNames.sanitize("whatever", false, 7, 3))
			.withMessage("Unsupported major version: 7");
		assertThatIllegalArgumentException().isThrownBy(() -> SchemaNames.sanitize("whatever", false, 2024, 1))
			.withMessage("Unsupported major version: 2024");
	}

	@Test
	void shouldSupportCalVerNeo4j() {
		assertThat(SchemaNames.sanitize("x\\\\y", false, 2025, -1)).hasValue("`x\\y`");
		assertThat(SchemaNames.sanitize("x\\\\y", false, 2026, 10)).hasValue("`x\\y`");
	}

	@Test
	void shouldBeAbleToForceQuotes() {
		assertThat(SchemaNames.sanitize("a", true, -1, -1)).hasValue("`a`");
	}

	@Test
	void shouldQuoteEmptyString() {
		assertThat(SchemaNames.sanitize(" ", false, -1, -1)).hasValue("` `");
	}

	@Test
	void shouldCacheData() throws Exception {
		Field cacheField = SchemaNames.class.getDeclaredField("CACHE");
		cacheField.setAccessible(true);
		Map<?, ?> cache = (Map<?, ?>) cacheField.get(null);

		cache.clear();
		assertThatNoException().isThrownBy(() -> SchemaNames.sanitize("a"));
		assertThatNoException().isThrownBy(() -> SchemaNames.sanitize("b"));
		assertThatNoException().isThrownBy(() -> SchemaNames.sanitize("a"));
		assertThatNoException().isThrownBy(() -> SchemaNames.sanitize("a", false, -23, -42));

		assertThat(cache).hasSize(2);
	}

	@Test
	void shouldOnlyObfuscateUnicodeWhenRequired() {
		String plain = "`administrator\\user`";
		String obfuscated = "`administrator\\u005C\\u0075ser`";

		Optional<String> v = SchemaNames.sanitize("administrator\\user", false, 3, -1);
		assertThat(v).hasValue(plain);

		v = SchemaNames.sanitize("administrator\\user", false, 4, 0);
		assertThat(v).hasValue(plain);

		v = SchemaNames.sanitize("administrator\\user", false, 4, -1);
		assertThat(v).hasValue(obfuscated);

		v = SchemaNames.sanitize("administrator\\user", false, 4, 2);
		assertThat(v).hasValue(obfuscated);

		v = SchemaNames.sanitize("administrator\\user", false, 5, 26);
		assertThat(v).hasValue(obfuscated);

		v = SchemaNames.sanitize("administrator\\user", false, 2025, 0);
		assertThat(v).hasValue(obfuscated);

		v = SchemaNames.sanitize("administrator\\user");
		assertThat(v).hasValue(obfuscated);
	}

	@Test
	void shouldNeutralizeUnicodeEscapesOnCalVerNeo4j() {

		String payload = ") SET n:Injected //";

		// Decoded and escaped as a backtick
		assertThat(SchemaNames.sanitize("x\\u0060" + payload, false, 2025, 0)).hasValue("`x``" + payload + "`");

		// Not valid escapes, so the start of the escape gets obfuscated
		for (String invalidEscape : List.of("u+060", "u٠٠٦٠", "u００６０")) {
			assertThat(SchemaNames.sanitize("x\\" + invalidEscape + payload, false, 2025, 0))
				.hasValue("`x\\u005C\\u0075" + invalidEscape.substring(1) + payload + "`");
		}
	}

	@Test
	void shouldDecodeUnicodeEscapes() {

		assertThat(SchemaNames.sanitize("x\\u0041y")).hasValue("xAy");
	}

	@Test
	void shouldDecodeUnicodeEscapesWithRepeatedU() {

		assertThat(SchemaNames.sanitize("x\\uu0060y")).hasValue("`x``y`");
	}

	@Test
	void shouldNotTreatEscapedBackslashesAsStartOfEscapes() {

		String bs = "\\";

		assertThat(SchemaNames.sanitize("x" + bs + bs + "u0060y")).hasValue("`x\\u005C\\u00750060y`");
		assertThat(SchemaNames.sanitize("x" + bs + bs + bs + bs + "u0041y")).hasValue("`x\\u005C\\u005C\\u00750041y`");
		assertThat(SchemaNames.sanitize("x" + bs + bs + "n")).hasValue("`x\\n`");
		assertThat(SchemaNames.sanitize("x" + bs + bs + "`")).hasValue("`x\\```");

		assertThat(SchemaNames.sanitize("x" + bs + bs + "u0060y", false, 3, -1)).hasValue("`x\\u0060y`");
	}

	@Test
	@Timeout(value = 2, threadMode = ThreadMode.SEPARATE_THREAD)
	void shouldHandleLongRunsOfBackslashesInLinearTime() {

		// Decodes to 100_001 backslashes not followed by a u, which took several seconds
		// with a backtracking pattern
		String value = "\\".repeat(200_001) + "x";
		assertThat(SchemaNames.sanitize(value)).hasValueSatisfying(v -> assertThat(v).hasSize(100_001 + 3));
	}

	@Test
	void shouldKeepTrailingBackslash() {

		assertThat(SchemaNames.sanitize("x\\")).hasValue("`x\\`");
	}

	@Test
	void shouldHandleEscapedBackslashes() {

		for (int major : new int[] { 3, 4, 5, 2025, -1 }) {
			for (String input : new String[] { "x\\y", "x\\\\y" }) {
				assertThat(SchemaNames.sanitize(input, false, major, -1)).hasValue("`x\\y`");
			}
			assertThat(SchemaNames.sanitize("x\\\\\\\\y", false, major, -1)).hasValue("`x\\\\y`");
		}
	}

}
