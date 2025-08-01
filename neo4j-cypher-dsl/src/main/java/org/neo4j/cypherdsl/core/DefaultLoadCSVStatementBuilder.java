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

import static org.apiguardian.api.API.Status.INTERNAL;

import java.net.URI;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.internal.LoadCSV;
import org.neo4j.cypherdsl.core.internal.UsingPeriodicCommit;

/**
 * @author Michael J. Simons
 * @soundtrack Thees Uhlmann - #2
 * @since 2021.2.1
 */
@API(status = INTERNAL, since = "2021.2.1")
final class DefaultLoadCSVStatementBuilder extends DefaultStatementBuilder implements LoadCSVStatementBuilder {

	static final class PrepareLoadCSVStatementImpl implements ExposesLoadCSV, OngoingLoadCSV {

		private final UsingPeriodicCommit usingPeriodicCommit;
		private final DefaultStatementBuilder source;

		private URI uri;

		private boolean withHeaders;

		PrepareLoadCSVStatementImpl(Integer rate) {

			this.usingPeriodicCommit = new UsingPeriodicCommit(rate);
			this.source = null;
		}

		PrepareLoadCSVStatementImpl(URI uri, boolean withHeaders) {

			this(uri, withHeaders, null);
		}

		PrepareLoadCSVStatementImpl(URI uri, boolean withHeaders, DefaultStatementBuilder source) {

			this.usingPeriodicCommit = null;

			this.uri = uri;
			this.withHeaders = withHeaders;
			this.source = source;
		}

			@Override
		public LoadCSVStatementBuilder as(String alias) {

			return DefaultLoadCSVStatementBuilder.create(this, alias, source);
		}

			@Override
		public OngoingLoadCSV loadCSV(URI newUri, boolean newWithHeaders) {

			this.uri = newUri;
			this.withHeaders = newWithHeaders;
			return this;
		}
	}

	static DefaultLoadCSVStatementBuilder create(PrepareLoadCSVStatementImpl config, String alias,
		DefaultStatementBuilder source) {

		// It should be reasonable safe to keep that immutable object around
		final LoadCSV loadCSV = new LoadCSV(config.uri, config.withHeaders, alias);
		return source == null ? new DefaultLoadCSVStatementBuilder(config.usingPeriodicCommit, loadCSV) :
			new DefaultLoadCSVStatementBuilder(source, config.usingPeriodicCommit, loadCSV);
	}

	private final UsingPeriodicCommit usingPeriodicCommit;

	private final LoadCSV loadCSV;

	private DefaultLoadCSVStatementBuilder(UsingPeriodicCommit usingPeriodicCommit, LoadCSV loadCSV) {
		super(usingPeriodicCommit, loadCSV);

		this.usingPeriodicCommit = usingPeriodicCommit;
		this.loadCSV = loadCSV;
	}

	private DefaultLoadCSVStatementBuilder(
		DefaultStatementBuilder source, UsingPeriodicCommit usingPeriodicCommit, LoadCSV loadCSV) {
		super(source, usingPeriodicCommit, loadCSV);
		this.usingPeriodicCommit = usingPeriodicCommit;
		this.loadCSV = loadCSV;
	}

	@Override
	public StatementBuilder withFieldTerminator(String fieldTerminator) {
		return new DefaultStatementBuilder(this.usingPeriodicCommit, this.loadCSV.withFieldTerminator(fieldTerminator));
	}
}
