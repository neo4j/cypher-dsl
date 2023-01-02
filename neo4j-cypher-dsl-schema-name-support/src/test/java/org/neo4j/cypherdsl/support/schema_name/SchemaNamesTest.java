/*
 * Copyright (c) 2019-2023 "Neo4j,"
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

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.assertThatNoException;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.Optional;

import org.junit.jupiter.api.Test;

/**
 * @author Michael J. Simons
 */
class SchemaNamesTest {

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
		assertThatIllegalArgumentException().isThrownBy(() -> SchemaNames.sanitize("whatever", false, 7, 3))
			.withMessage("Unsupported major version: 7");
	}

	@Test
	void shouldSupportFutureNeo4j() {
		assertThat(SchemaNames.sanitize("x\\\\y", false, 6, -1)).hasValue("`x\\\\y`");
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
	}

	@Test
	void shouldHandleEscapedBackslashes() {

		for (String input : new String[] {"x\\y", "x\\\\y"}) {
			Optional<String> v = SchemaNames.sanitize(input, false, 4, -1);
			String expected = "`x\\y`";
			assertThat(v).hasValue(expected);

			v = SchemaNames.sanitize("x\\\\y", false, 4, -1);
			assertThat(v).hasValue(expected);
		}

		Optional<String> v = SchemaNames.sanitize("x\\\\y", false, 6, -1);
		assertThat(v).hasValue("`x\\\\y`");
	}
}
