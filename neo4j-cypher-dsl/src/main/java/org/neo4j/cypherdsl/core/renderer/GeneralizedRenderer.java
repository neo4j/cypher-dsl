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

import static org.apiguardian.api.API.Status.STABLE;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitable;

/**
 * This is a more general renderer than {@link Renderer}. The generalized renderer will render any {@link Visitable}.
 * This renderer will not cache rendered results for visitables.
 *
 * @author Michael J. Simons
 * @since 2023.1.0
 */
@API(status = STABLE, since = "2023.1.0")
public sealed interface GeneralizedRenderer extends Renderer permits ConfigurableRenderer {

	/**
	 * Renders any {@link Visitable}.
	 *
	 * @param visitable the visitable to render
	 * @return a Cypher fragment (in case of arbitrary visitables), a full statement in case a {@link org.neo4j.cypherdsl.core.Statement} was passed to the renderer.
	 */
	String render(Visitable visitable);
}
