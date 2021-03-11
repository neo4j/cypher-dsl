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

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.temporal.TemporalAccessor;
import java.time.temporal.TemporalField;

import org.junit.jupiter.api.Test;

/**
 * @author Michael J. Simons
 * @soundtrack Fritz Kalkbrenner - Drown
 */
class TemporalLiteralTest {

	@Test
	void localDateShouldWork() {

		TemporalLiteral literal = new TemporalLiteral(LocalDate.of(2021, 3, 10));
		assertThat(literal.asString()).isEqualTo("date('2021-03-10')");
	}

	@Test
	void localDateTimeShouldWork() {

		TemporalLiteral literal = new TemporalLiteral(LocalDateTime.of(2021, 3, 10, 15, 13, 0));
		assertThat(literal.asString()).isEqualTo("localdatetime('2021-03-10T15:13:00')");
	}

	@Test
	void zonedDateTimeShouldWork() {

		TemporalLiteral literal = new TemporalLiteral(
			ZonedDateTime.of(LocalDate.of(2021, 3, 10), LocalTime.of(15, 41, 0), ZoneId.of("Europe/Berlin")));
		assertThat(literal.asString()).isEqualTo("datetime('2021-03-10T15:41:00+01:00[Europe/Berlin]')");
	}

	@Test
	void localTimeShouldWork() {

		TemporalLiteral literal = new TemporalLiteral(LocalTime.of(15, 42));
		assertThat(literal.asString()).isEqualTo("localtime('15:42:00')");
	}

	@Test
	void offsetTimeShouldWork() {

		TemporalLiteral literal = new TemporalLiteral(OffsetTime.of(LocalTime.of(15, 15, 0), ZoneOffset.ofHours(2)));
		assertThat(literal.asString()).isEqualTo("time('15:15:00+02:00')");
	}

	@Test
	void shouldThrowAnExceptionOnUnsupportedTemporalAccessor() {

		assertThatIllegalArgumentException()
			.isThrownBy(() -> new TemporalLiteral(new TemporalAccessor() {
				@Override
				public boolean isSupported(TemporalField field) {
					return false;
				}

				@Override
				public long getLong(TemporalField field) {
					return 0;
				}
			}));
	}
}
