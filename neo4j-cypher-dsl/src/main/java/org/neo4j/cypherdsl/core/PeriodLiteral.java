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

import java.time.Period;

import org.jetbrains.annotations.NotNull;

/**
 * A literal representing a period value to be formatted in a way that Neo4j's Cypher understands it.
 *
 * @author Michael J. Simons
 * @since TBA
 */
final class PeriodLiteral extends LiteralBase<Period> {

	static Literal<Period> of(Period duration) {
		return new PeriodLiteral(duration);
	}

	private PeriodLiteral(Period content) {
		super(content);
	}

	@Override
	@NotNull
	public String asString() {
		var content = super.getContent();
		var result = new StringBuilder();
		result.append("duration('P");

		if (content.getYears() != 0) {
			result.append(content.getYears()).append("Y");
		}
		if (content.getMonths() != 0) {
			result.append(content.getMonths()).append("M");
		}
		if (content.getDays() != 0) {
			result.append(content.getDays()).append("D");
		}
		result.append("')");
		return result.toString();
	}
}
