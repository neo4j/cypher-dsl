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
package org.neo4j.cypherdsl.core;

import java.time.Duration;

/**
 * A literal representing a duration value to be formatted in a way that Neo4j's Cypher
 * understands it.
 *
 * @author Michael J. Simons
 * @since 2023.2.1
 */
final class DurationLiteral extends LiteralBase<Duration> {

	private DurationLiteral(Duration content) {
		super(content);
	}

	static Literal<Duration> of(Duration duration) {
		return new DurationLiteral(duration);
	}

	@Override
	public Duration getContent() {
		return this.content;
	}

	@Override
	public String asString() {
		var result = new StringBuilder();
		result.append("duration('P");

		var hours = this.content.toHours();
		var minutes = this.content.toMinutesPart();
		var seconds = this.content.toSecondsPart();
		var nanos = this.content.toNanosPart();

		if (hours != 0 || minutes != 0 || seconds != 0 || nanos != 0) {
			result.append("T");
		}
		if (hours != 0) {
			result.append(hours).append("H");
		}
		if (minutes != 0) {
			result.append(minutes).append("M");
		}
		if (seconds != 0 || nanos != 0) {
			if (nanos == 0) {
				result.append(seconds);
			}
			else {
				result.append(seconds + nanos / 1_000_000_000.0);
			}
			result.append("S");
		}

		result.append("')");
		return result.toString();
	}

}
