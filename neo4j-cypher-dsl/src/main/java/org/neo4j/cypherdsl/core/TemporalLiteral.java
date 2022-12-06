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
package org.neo4j.cypherdsl.core;

import static org.apiguardian.api.API.Status.STABLE;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetTime;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.TemporalAccessor;

import org.apiguardian.api.API;
import org.jetbrains.annotations.NotNull;

/**
 * A literal representing a temporal value to be formatted in a way that Neo4j's Cypher understands it.
 *
 * @author Michael J. Simons
 * @soundtrack Fritz Kalkbrenner - Drown
 * @since 2021.1.0
 */
@API(status = STABLE, since = "2021.1.0")
public final class TemporalLiteral extends LiteralBase<TemporalAccessor> {

	private final String value;

	TemporalLiteral(TemporalAccessor content) {
		super(content);

		String method;
		DateTimeFormatter formatter;
		if (content instanceof LocalDate) {
			method = "date";
			formatter = DateTimeFormatter.ISO_LOCAL_DATE;
		} else if (content instanceof LocalDateTime) {
			method = "localdatetime";
			formatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME;
		} else if (content instanceof ZonedDateTime) {
			method = "datetime";
			formatter = DateTimeFormatter.ISO_ZONED_DATE_TIME;
		} else if (content instanceof LocalTime) {
			method = "localtime";
			formatter = DateTimeFormatter.ISO_LOCAL_TIME;
		} else if (content instanceof OffsetTime) {
			method = "time";
			formatter = DateTimeFormatter.ISO_OFFSET_TIME;
		} else {
			throw new UnsupportedLiteralException(content);
		}
		this.value = String.format("%s('%s')", method, formatter.format(content));
	}

	@NotNull
	@Override
	public String asString() {
		return value;
	}
}
