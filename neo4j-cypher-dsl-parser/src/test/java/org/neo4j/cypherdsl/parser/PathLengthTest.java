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

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * @author Michael J. Simons
 */
class PathLengthTest {

	@ParameterizedTest
	@CsvSource(nullValues = "N/A", value = { "N/A, N/A", "N/A, 5", "5, N/A", "5, 10" })
	void length1(String minimum, String maximimum) {
		PathLength pathLength = PathLength.of(minimum, maximimum);
		if (minimum == null && maximimum == null) {
			assertThat(pathLength.isUnbounded()).isTrue();
		}

		if (minimum != null) {
			assertThat(pathLength.getMinimum()).isEqualTo(Integer.parseInt(minimum));
		}

		if (maximimum != null) {
			assertThat(pathLength.getMaximum()).isEqualTo(Integer.parseInt(maximimum));
		}
	}
}
