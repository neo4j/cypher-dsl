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

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

/**
 * Allows to specify the where condition for the list based predicate.
 *
 * @author Michael J. Simons
 * @since 2024.0.0
 */
public interface OngoingListBasedPredicateFunctionWithList {

	/**
	 * @param condition The condition for the list based predicate.
	 * @return The final list based predicate function
	 */
	@NotNull
	@Contract(pure = true)
	Condition where(Condition condition);
}
