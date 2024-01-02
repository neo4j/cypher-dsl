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
package org.neo4j.cypherdsl.core;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.fail;

import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.jupiter.api.Test;

/**
 * @author Michael J. Simons
 */
class CypherTest {

	@Test
	void sortDirectionShouldBeSpecified() {

		SortItem sortItem = Cypher.sort(Cypher.literalFalse(), SortItem.Direction.ASC);
		sortItem.accept(segment -> {

			if (segment instanceof SortItem.Direction direction) {
				assertThat(direction)
					.extracting(SortItem.Direction::getSymbol).isEqualTo("ASC");
			}
		});
	}

	@Test
	@SuppressWarnings("ResultOfMethodCallIgnored")
	void shouldNotCreateIllegalLiterals() {
		assertThatIllegalArgumentException().isThrownBy(() -> Cypher.literalOf(new CypherTest()))
			.withMessageStartingWith("Unsupported literal type: ");
	}

	@Test
	void nullLiteralShouldBeSameInstance() {

		assertThat(Cypher.literalNull()).isSameAs(NullLiteral.INSTANCE);
	}

	@Test
	void shouldCreateListLiterals() {

		List<Literal<?>> params = new ArrayList<>();
		params.add(Cypher.literalFalse());
		params.add(Cypher.literalTrue());

		Literal<?> listLiteral = Cypher.literalOf(params);

		assertThat(listLiteral).isInstanceOf(ListLiteral.class)
			.returns("[false, true]", Literal::asString);
	}

	@Test
	void shouldCreateMapLiterals() {

		Map<Object, Literal<?>> params = new LinkedHashMap<>();

		Map<String, Object> mapValue = new LinkedHashMap<>();
		mapValue.put("m1", 1);
		mapValue.put("m2", "m2");

		params.put('k', Cypher.literalFalse()); // Character key also valid
		params.put("k2", Cypher.literalTrue());
		params.put("k3", Cypher.literalOf(Arrays.asList("a", "b", "c")));
		params.put("k4", Cypher.literalOf(mapValue));

		Literal<?> mapLiteral = Cypher.literalOf(params);

		assertThat(mapLiteral).isInstanceOf(MapLiteral.class)
				.returns("{k: false, k2: true, k3: ['a', 'b', 'c'], k4: {m1: 1, m2: 'm2'}}", Literal::asString);
	}

	@Test
	void shouldQuoteStrings() {

		assertThat(Cypher.quote("foo")).isEqualTo("'foo'");
		assertThat(Cypher.quote("fo`o")).isEqualTo("'fo`o'");
	}

	@Test
	void shouldCreatePropertyPointingToSymbolicName() {
		Property property = Cypher.property("a", "b");
		AtomicInteger counter = new AtomicInteger(0);
		property.accept(segment -> {
			int cnt = counter.incrementAndGet();
			switch (cnt) {
				case 1 -> assertThat(segment).isInstanceOf(Property.class);
				case 2 -> assertThat(segment).isInstanceOf(SymbolicName.class).extracting("value").isEqualTo("a");
				case 3 -> assertThat(segment).isInstanceOf(PropertyLookup.class);
				case 4 -> assertThat(segment).isInstanceOf(SymbolicName.class).extracting("value").isEqualTo("b");
				default -> fail("Unexpected segment: " + segment.getClass());
			}
		});
	}

	@Test // GH-189
	void shouldCreatePropertyWithNameFromCollectionPointingToSymbolicName() {
		Property property = Cypher.property("a", Collections.singleton("b"));
		AtomicInteger counter = new AtomicInteger(0);
		property.accept(segment -> {
			int cnt = counter.incrementAndGet();
			switch (cnt) {
				case 1 -> assertThat(segment).isInstanceOf(Property.class);
				case 2 -> assertThat(segment).isInstanceOf(SymbolicName.class).extracting("value").isEqualTo("a");
				case 3 -> assertThat(segment).isInstanceOf(PropertyLookup.class);
				case 4 -> assertThat(segment).isInstanceOf(SymbolicName.class).extracting("value").isEqualTo("b");
				default -> fail("Unexpected segment: " + segment.getClass());
			}
		});
	}

	@Test // GH-189
	void shouldCreateAdditionalLabelsFromCollection() {
		Node node = Cypher.node("Primary", MapExpression.create(false), Collections.singleton("Secondary"));
		assertThat(node.getLabels()).extracting("value").containsExactly("Primary", "Secondary");
	}

}
