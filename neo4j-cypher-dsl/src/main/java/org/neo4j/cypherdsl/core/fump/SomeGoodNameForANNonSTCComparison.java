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
package org.neo4j.cypherdsl.core.fump;

import org.neo4j.cypherdsl.core.Operator;

/**
 * @author Michael J. Simons
 * @param property The property used in this condition
 * @param left The left hand side of the comparison represented as Cypher
 * @param operator The operator used
 * @param right The right hand side of the comparison represented as Cypher
 * @soundtrack Die Ärzte - Le Frisur
 * @since TBA
 */
public record SomeGoodNameForANNonSTCComparison(Property property, String left, Operator operator, String right) {
}
