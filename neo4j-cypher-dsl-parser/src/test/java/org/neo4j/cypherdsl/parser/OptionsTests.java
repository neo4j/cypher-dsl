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
package org.neo4j.cypherdsl.parser;

import java.util.HashMap;
import java.util.Map;
import java.util.function.UnaryOperator;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatNoException;

/**
 * @author Michael J. Simons
 */
class OptionsTests {

	@Test // GH-299
	void patternElementCallBacksShouldBeConfigurable() {

		var builder = Options.newOptions()
			.withCallback(PatternElementCreatedEventType.ON_CREATE, UnaryOperator.identity());
		var options = builder.build();
		assertThat(options.getOnNewPatternElementCallbacks()).containsKey(PatternElementCreatedEventType.ON_CREATE);
		assertThat(options.getOnNewPatternElementCallbacks().get(PatternElementCreatedEventType.ON_CREATE)).hasSize(1);

		builder.withCallback(PatternElementCreatedEventType.ON_CREATE, UnaryOperator.identity());
		options = builder.build();
		assertThat(options.getOnNewPatternElementCallbacks()).containsKey(PatternElementCreatedEventType.ON_CREATE);
		assertThat(options.getOnNewPatternElementCallbacks().get(PatternElementCreatedEventType.ON_CREATE)).hasSize(2);
	}

	@Test // GH-785
	void optionsMustAllowNullParameterValues() {
		Map<String, Object> wurst = new HashMap<>();
		wurst.put("salat", null);
		var builder = Options.newOptions();
		assertThatNoException().isThrownBy(() -> builder.withParameterValues(wurst));
	}

	@Test // Grrr aaargh sonar, that code was there before, but alas, here's your 75%.
	void nullParameterShouldResetThings() {

		Map<String, Object> wurst = new HashMap<>();
		wurst.put("salat", null);
		var builder = Options.newOptions();
		builder.withParameterValues(wurst);
		Options options = builder.build();
		assertThat(options.getParameterValues()).containsEntry("salat", null);
		builder.withParameterValues(null);
		options = builder.build();
		assertThat(options.getParameterValues()).isEmpty();
	}

}
