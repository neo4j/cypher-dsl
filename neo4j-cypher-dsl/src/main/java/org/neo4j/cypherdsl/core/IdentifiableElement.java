/*
 * Copyright (c) 2019-2024 "Neo4j,"
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
 * This interface represents an element that can be for example an identifiable part of the {@code WITH} clause.
 * It has been introduced to circumvent the absence of union types in Java
 * and to avoid an overload of {@link StatementBuilder#with(Expression...)} with an {@code Object...} parameter
 * to allow passing {@link Named named things} or {@link AliasedExpression aliased expression} into a pipeline.
 * <p>
 * There should be no need to implement this on your own.
 *
 * @author Michael J. Simons
 * @soundtrack Fatoni &amp; Edgar Wasser - Delirium
 * @since 2021.2.2
 */
public interface IdentifiableElement {

	/**
	 * Transform this element into an expression
	 *
	 * @return this element as an expression. Will return the same instance if it is already an expression.
	 * @since 2021.2.2
	 */
	@NotNull @Contract(pure = true)
	Expression asExpression();
}
