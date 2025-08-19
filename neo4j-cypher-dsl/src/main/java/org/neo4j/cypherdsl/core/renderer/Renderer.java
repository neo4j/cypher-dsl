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

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.Statement;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * Instances of this class are supposed to be thread-safe. Please use
 * {@link Renderer#getDefaultRenderer()} to get hold of an implementation.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public sealed interface Renderer permits ConfigurableRenderer, GeneralizedRenderer {

	/**
	 * Provides the default renderer. This method may or may not provide shared instances
	 * of the renderer.
	 * @return the default renderer.
	 */
	static Renderer getDefaultRenderer() {
		return getRenderer(Configuration.defaultConfig());
	}

	/**
	 * Creates a new renderer for the given configuration.
	 * @param configuration the configuration for this renderer
	 * @return a new renderer (might be a shared instance).
	 */
	static Renderer getRenderer(Configuration configuration) {
		return getRenderer(configuration, Renderer.class);
	}

	/**
	 * Creates a new renderer for the given configuration.
	 * @param <T> the specific type of the renderer
	 * @param configuration the configuration for this renderer
	 * @param type reification of the renderers type
	 * @return a new renderer (might be a shared instance).
	 * @since 2023.1.0
	 */
	static <T extends Renderer> T getRenderer(Configuration configuration, Class<T> type) {
		return type.cast(ConfigurableRenderer.create(configuration));
	}

	/**
	 * Renders a statement.
	 * @param statement the statement to render
	 * @return the rendered Cypher statement.
	 */
	String render(Statement statement);

}
