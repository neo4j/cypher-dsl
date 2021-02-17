package org.neo4j.cypherdsl.core.renderer;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import org.neo4j.cypherdsl.core.Statement;

/**
 * A minimal, shared based class for renderer. It's main responsibility is keeping some rendered content around.
 *
 * @author Michael J. Simons
 * @soundtrack Slayer - Undisputed Attitude
 * @since 2020.0.1
 */
abstract class AbstractRenderer implements Renderer {

	private final int STATEMENT_CACHE_SIZE = 128;
	private final LinkedHashMap<Integer, String> renderedStatementCache = new LRUCache<>(STATEMENT_CACHE_SIZE);

	private final ReadWriteLock lock = new ReentrantReadWriteLock();
	private final Lock read = lock.readLock();
	private final Lock write = lock.writeLock();

	private final Configuration configuration;

	AbstractRenderer(Configuration configuration) {
		this.configuration = configuration;
	}

	@Override
	public String render(Statement statement) {

		int key = Objects.hashCode(statement);

		String renderedContent;
		try {
			read.lock();
			renderedContent = renderedStatementCache.get(key);
		} finally {
			read.unlock();
		}

		if (renderedContent == null) {
			try {
				write.lock();

				DefaultVisitor renderingVisitor = new DefaultVisitor();
				statement.accept(renderingVisitor);
				renderedContent = doRender(statement);

				renderedStatementCache.put(key, renderedContent);
			} finally {
				write.unlock();
			}
		}

		return renderedContent;
	}

	protected abstract String doRender(Statement statement);

	private static class LRUCache<K, V> extends LinkedHashMap<K, V> {

		private static final long serialVersionUID = -6819899594092598277L;

		private final int cacheSize;

		LRUCache(int cacheSize) {
			super(cacheSize / 4, 0.75f, true);
			this.cacheSize = cacheSize;
		}

		@Override
		protected boolean removeEldestEntry(Map.Entry<K, V> eldest) {
			return size() >= cacheSize;
		}
	}
}
