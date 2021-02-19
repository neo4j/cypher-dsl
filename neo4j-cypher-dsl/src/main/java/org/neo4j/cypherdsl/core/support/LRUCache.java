/*
 * Copyright (c) 2019-2021 "Neo4j,"
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
package org.neo4j.cypherdsl.core.support;

import static org.apiguardian.api.API.Status.INTERNAL;

import java.util.LinkedHashMap;
import java.util.Map;

import org.apiguardian.api.API;

/**
 * A non thread safe least recently used (LRU) cache. It must be used with a locking implementation around it.
 *
 * @author Michael J. Simons
 * @param <K> Type of the keys
 * @param <V> Type of the values
 * @soundtrack Evanescence - Synthesis Live
 * @since 2021.0.2
 */
@API(status = INTERNAL, since = "2021.0.2")
public final class LRUCache<K, V> extends LinkedHashMap<K, V> {

	private static final long serialVersionUID = -6819899594092598277L;

	private final int cacheSize;

	/**
	 * Creates a new LRU cache with the given cache size
	 *
	 * @param cacheSize The number of entries to keep around.
	 */
	public LRUCache(int cacheSize) {
		super(cacheSize / 4, 0.75f, true);
		this.cacheSize = cacheSize;
	}

	@Override
	protected boolean removeEldestEntry(Map.Entry<K, V> eldest) {
		return size() >= cacheSize;
	}
}
