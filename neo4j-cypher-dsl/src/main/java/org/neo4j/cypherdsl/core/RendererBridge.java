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

import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.renderer.Configuration;
import org.neo4j.cypherdsl.core.renderer.Dialect;
import org.neo4j.cypherdsl.core.renderer.GeneralizedRenderer;
import org.neo4j.cypherdsl.core.renderer.Renderer;

/**
 * A bridge to the renderer as a single entry point from core to the renderer
 * infrastructure.
 *
 * @author Michael J. Simons
 * @since 2023.1.0
 */
final class RendererBridge {

	private static final Configuration CONFIGURATION = Configuration.newConfig()
		.withDialect(Dialect.NEO4J_5)
		.alwaysEscapeNames(false)
		.build();

	private RendererBridge() {
	}

	static String render(Visitable visitable) {
		String name;
		Class<? extends Visitable> clazz = visitable.getClass();
		if (clazz.isAnonymousClass()) {
			name = clazz.getName();
		}
		else {
			name = clazz.getSimpleName();
		}
		return "%s{cypher=%s}".formatted(name,
				Renderer.getRenderer(CONFIGURATION, GeneralizedRenderer.class).render(visitable));
	}

}
