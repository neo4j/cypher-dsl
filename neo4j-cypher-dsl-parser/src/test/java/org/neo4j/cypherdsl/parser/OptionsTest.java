/*
 * Copyright (c) "Neo4j"
 * Neo4j Sweden AB [http://neo4j.com]
 *
 * This file is part of Neo4j.
 *
 * Neo4j is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.neo4j.cypherdsl.parser;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatNoException;

import java.util.HashMap;
import java.util.Map;
import java.util.function.UnaryOperator;

import org.junit.jupiter.api.Test;

/**
 * @author Michael J. Simons
 */
class OptionsTest {

	@Test // GH-299
	void patternElementCallBacksShouldBeConfigurable() {

		var builder = Options.newOptions()
			.withCallback(PatternElementCreatedEventType.ON_CREATE, UnaryOperator.identity());
		var options = builder.build();
		assertThat(options.getOnNewPatternElementCallbacks())
			.containsKey(PatternElementCreatedEventType.ON_CREATE);
		assertThat(options.getOnNewPatternElementCallbacks().get(PatternElementCreatedEventType.ON_CREATE)).hasSize(1);

		builder.withCallback(PatternElementCreatedEventType.ON_CREATE, UnaryOperator.identity());
		options = builder.build();
		assertThat(options.getOnNewPatternElementCallbacks())
			.containsKey(PatternElementCreatedEventType.ON_CREATE);
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
