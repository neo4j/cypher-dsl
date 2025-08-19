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
package org.neo4j.cypherdsl.core;

import java.time.Period;

/**
 * A literal representing a period value to be formatted in a way that Neo4j's Cypher
 * understands it.
 *
 * @author Michael J. Simons
 * @since 2023.2.1
 */
final class PeriodLiteral extends LiteralBase<Period> {

	private PeriodLiteral(Period content) {
		super(content);
	}

	static Literal<Period> of(Period duration) {
		return new PeriodLiteral(duration);
	}

	@Override
	public Period getContent() {
		return this.content;
	}

	@Override
	public String asString() {
		var result = new StringBuilder();
		result.append("duration('P");

		if (this.content.getYears() != 0) {
			result.append(this.content.getYears()).append("Y");
		}
		if (this.content.getMonths() != 0) {
			result.append(this.content.getMonths()).append("M");
		}
		if (this.content.getDays() != 0) {
			result.append(this.content.getDays()).append("D");
		}
		result.append("')");
		return result.toString();
	}

}
