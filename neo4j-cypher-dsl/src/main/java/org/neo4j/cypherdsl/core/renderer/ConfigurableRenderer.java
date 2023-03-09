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
package org.neo4j.cypherdsl.core.renderer;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.function.BiFunction;

import org.neo4j.cypherdsl.core.Statement;
import org.neo4j.cypherdsl.core.internal.DefaultStatementContext;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.utils.LRUCache;
import org.neo4j.cypherdsl.core.StatementContext;

/**
 * @author Michael J. Simons
 * @author Gerrit Meier
 * @since 1.0
 */
final class ConfigurableRenderer implements GeneralizedRenderer, Renderer {

	private static final Map<Configuration, ConfigurableRenderer> CONFIGURATIONS = new ConcurrentHashMap<>(8);
	private static final int STATEMENT_CACHE_SIZE = 128;

	/**
	 * Creates a new instance of the configurable renderer or uses an existing one matching the  given configuration
	 *
	 * @param configuration The configuration for the render
	 * @return A new renderer
	 */
	static ConfigurableRenderer create(Configuration configuration) {
		return CONFIGURATIONS.computeIfAbsent(configuration, ConfigurableRenderer::new);
	}

	private final LRUCache<Integer, String> renderedStatementCache = new LRUCache<>(STATEMENT_CACHE_SIZE);

	private final ReadWriteLock lock = new ReentrantReadWriteLock();
	private final Lock read = lock.readLock();
	private final Lock write = lock.writeLock();

	private final Configuration configuration;

	ConfigurableRenderer(Configuration configuration) {
		this.configuration = configuration;
	}

	@Override
	public String render(Statement statement) {
		return render((Visitable) statement);
	}

	@Override
	// This is about not using map.computeIfAbsent. This is done very much on purpose to keep this
	// class thread safe. The LRUCache is basically LinkedHashMap and the method wouldn't be threadsafe.
	@SuppressWarnings("squid:S3824")
	public String render(Visitable visitable) {

		record RenderingConfig(StatementContext ctx, boolean renderConstantsAsParameters) {
		}

		BiFunction<RenderingConfig, Visitable, String> renderOp = (cfg, v) -> {
			var renderingVisitor = createVisitor(cfg.ctx, cfg.renderConstantsAsParameters);
			v.accept(renderingVisitor);
			return renderingVisitor.getRenderedContent().trim();
		};

		if (visitable instanceof Statement statement) {
			String renderedContent;

			//var contextSupplier = configuration.getContextSupplier();
			int key = Objects.hash(statement, statement.isRenderConstantsAsParameters());
			try {
				read.lock();
				renderedContent = renderedStatementCache.get(key);
			} finally {
				read.unlock();
			}

			if (renderedContent == null) {
				try {
					write.lock();
					renderedContent = renderOp.apply(new RenderingConfig(statement.getContext(), statement.isRenderConstantsAsParameters()), statement);
					renderedStatementCache.put(key, renderedContent);
				} finally {
					write.unlock();
				}
			}
			return renderedContent;
		} else {
			return renderOp.apply(new RenderingConfig(new DefaultStatementContext(), false), visitable);
		}
	}

	private RenderingVisitor createVisitor(StatementContext statementContext, boolean renderConstantsAsParameters) {

		if (!this.configuration.isPrettyPrint()) {
			return new DefaultVisitor(statementContext, renderConstantsAsParameters, this.configuration);
		} else {
			return new PrettyPrintingVisitor(statementContext, renderConstantsAsParameters, this.configuration);
		}
	}

}
