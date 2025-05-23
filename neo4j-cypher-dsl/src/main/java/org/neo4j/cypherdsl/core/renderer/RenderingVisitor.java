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
package org.neo4j.cypherdsl.core.renderer;

import org.neo4j.cypherdsl.core.ast.Visitor;

/**
 * Internal interface for various implementations of {@link Visitor visitors} that provide a rendered view of the visitable
 * they have been used with.
 *
 * @author Michael J. Simons
 */
interface RenderingVisitor extends Visitor {

	/**
	 * @return Renderer content after this visitor has been accepted by a {@link org.neo4j.cypherdsl.core.ast.Visitable}.
	 */
	String getRenderedContent();
}
