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
package org.neo4j.cypherdsl.core;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetTime;
import java.time.Period;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Stream;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.neo4j.cypherdsl.core.DriverValueAdapter.TemporalAmountAdapter;
import org.neo4j.cypherdsl.core.renderer.Configuration;
import org.neo4j.cypherdsl.core.renderer.GeneralizedRenderer;
import org.neo4j.cypherdsl.core.renderer.Renderer;
import org.neo4j.driver.Value;
import org.neo4j.driver.Values;
import org.neo4j.driver.types.IsoDuration;

/**
 * @author Michael J. Simons
 * @soundtrack Prezident - Gesunder Eskapismus
 * @since TBA
 */
class DriverValueAdapterTest {

	private final GeneralizedRenderer renderer = Renderer.getRenderer(Configuration.prettyPrinting(), GeneralizedRenderer.class);

	@Test
	void namesAreNotSupported() {

		var adapter = Cypher.adapt(Values.value(23));
		assertThatExceptionOfType(UnsupportedOperationException.class)
			.isThrownBy(adapter::asName);
	}

	@Test
	void generalConditionsAreNotSupported() {

		var adapter = Cypher.adapt(Values.value(23));
		assertThatExceptionOfType(UnsupportedOperationException.class)
			.isThrownBy(adapter::asCondition)
			.withMessage("Only Boolean values can be adapted as condition");
	}

	@Test
	void booleanConditionsAreNotSupported() {

		var adapter = Cypher.adapt(Values.value(false));
		assertThat(renderer.render(adapter.asCondition()))
			.isEqualTo("false");
	}

	static Stream<Arguments> scalarValuesShouldWork() {

		return Stream.of(
			Arguments.of(Values.value("Hallo, 'Cypher"), "'Hallo, \\'Cypher'"),
			Arguments.of(Values.value(23), "23"),
			Arguments.of(Values.value(42L), "42"),
			Arguments.of(Values.value((short) 1), "1"),
			Arguments.of(Values.value(23.1f), "23.1"),
			Arguments.of(Values.value(42.1), "42.1"),
			Arguments.of(Values.value(true), "true"),
			Arguments.of(Values.point(7203, 3.0, 0.0), "point({srid: 7203, x: 3.0, y: 0.0})"),
			Arguments.of(Values.point(4326, 56.0, 12.0), "point({srid: 4326, x: 56.0, y: 12.0})"),
			Arguments.of(Values.point(9157, 0, 4, 1), "point({srid: 9157, x: 0.0, y: 4.0, z: 1.0})"),
			Arguments.of(Values.point(4979, 56, 12, 1000), "point({srid: 4979, x: 56.0, y: 12.0, z: 1000.0})"),
			Arguments.of(Values.value(LocalDate.of(2023, 3, 24)), "date('2023-03-24')"),
			Arguments.of(Values.value(OffsetTime.of(10, 50, 23, 0, ZoneOffset.of("+2"))), "time('10:50:23+02:00')"),
			Arguments.of(Values.value(LocalTime.of(10, 50, 23)), "localtime('10:50:23')"),
			Arguments.of(Values.value(ZonedDateTime.of(LocalDate.of(2023, 3, 24), LocalTime.of(10, 50, 23), ZoneId.of("Europe/Berlin"))), "datetime('2023-03-24T10:50:23+01:00[Europe/Berlin]')"),
			Arguments.of(Values.value(LocalDateTime.of(LocalDate.of(2023, 3, 24), LocalTime.of(10, 50, 23))), "localdatetime('2023-03-24T10:50:23')"),
			Arguments.of(Values.value(Period.ofDays(23).plusMonths(1)), "duration('P1M23D')"),
			Arguments.of(Values.value(Period.ofYears(1).plusMonths(23).plusMonths(2).plusDays(400)), "duration('P3Y1M400D')"),
			Arguments.of(Values.value(Duration.ofHours(23).plusMinutes(61).plusSeconds(120)), "duration('P1DT3M')"),
			Arguments.of(Values.value(List.of("Hallo", 123, List.of(Values.value(1)), Values.value(1, 2, 3), Values.point(7203, 3.0, 0.0))), "['Hallo', 123, [1], [1, 2, 3], point({srid: 7203, x: 3.0, y: 0.0})]"),
			Arguments.of(Values.value(
				Map.of(
					"aList", List.of("Hallo", 123, Values.value(1, 2, 3)),
					"aNumber", 47,
					"aNestedMap", new TreeMap<>(Map.of("aPoint", Values.point(7203, 3.0, 0.0), "aDuration", Values.value(Duration.ofHours(23).plusMinutes(61).plusSeconds(120)))),
					"asd", Values.value("haha")
				)
			), "{aNumber: 47, asd: 'haha', aList: ['Hallo', 123, [1, 2, 3]], aNestedMap: {aDuration: duration('P1DT3M'), aPoint: point({srid: 7203, x: 3.0, y: 0.0})}}"),
			Arguments.of(Values.NULL, "NULL")
		);
	}

	@ParameterizedTest
	@MethodSource
	void scalarValuesShouldWork(Value value, String expected) {

		var adapter = Cypher.adapt(value);
		var adapted = adapter.asExpression();
		assertThat(renderer.render(adapted))
			.isEqualTo(expected);
	}

	@Test
	void byteArrayCannotWork() {

		var adapter = Cypher.adapt(Values.value("hallo".getBytes(StandardCharsets.UTF_8)));
		assertThatIllegalArgumentException().isThrownBy(adapter::asExpression)
			.withMessage("byte[] values cannot be represented as expression.");
	}

	@Test
	void shouldNotAdaptEverythingIntoNodes() {

		var adapter = Cypher.adapt(Values.value("hallo"));
		assertThatIllegalArgumentException().isThrownBy(adapter::asNode)
			.withMessage("Cannot adopt value with type STRING as node");
	}

	@Test
	void shouldNotAdaptEverythingIntoRelationships() {

		var adapter = Cypher.adapt(Values.value("hallo"));
		assertThatIllegalArgumentException().isThrownBy(adapter::asRelationship)
			.withMessage("Cannot adopt value with type STRING as relationship");
	}

	@Nested
	class TemporalAmountAdapterTest {

		private final TemporalAmountAdapter underTest = new TemporalAmountAdapter();

		@Test
		void internallyCreatedTypesShouldBeConvertedCorrect() {

			assertThat(underTest.apply(Values.isoDuration(1, 0, 0, 0).asIsoDuration())).isEqualTo(Period.ofMonths(1));
			assertThat(underTest.apply(Values.isoDuration(1, 1, 0, 0).asIsoDuration())).isEqualTo(Period.ofMonths(1).plusDays(1));
			assertThat(underTest.apply(Values.isoDuration(1, 1, 1, 0).asIsoDuration()))
				.isEqualTo(Values.isoDuration(1, 1, 1, 0).asIsoDuration());
			assertThat(underTest.apply(Values.isoDuration(0, 0, 120, 1).asIsoDuration()))
				.isEqualTo(Duration.ofMinutes(2).plusNanos(1));
		}

		@Test
		void durationsShouldStayDurations() {

			Duration duration = ChronoUnit.MONTHS.getDuration().multipliedBy(13).plus(ChronoUnit.DAYS.getDuration().multipliedBy(32)).plusHours(25)
				.plusMinutes(120);

			assertThat(underTest.apply(Values.value(duration).asIsoDuration())).isEqualTo(duration);
		}

		@Test
		void periodsShouldStayPeriods() {

			Period period = Period.between(LocalDate.of(2018, 11, 15), LocalDate.of(2020, 12, 24));

			assertThat(underTest.apply(Values.value(period).asIsoDuration())).isEqualTo(period.normalized());
		}

		@Test // GH-2324
		void zeroDurationShouldReturnTheIsoDuration() {

			IsoDuration zeroDuration = Values.isoDuration(0, 0, 0, 0).asIsoDuration();
			assertThat(underTest.apply(zeroDuration)).isSameAs(zeroDuration);
		}
	}
}
