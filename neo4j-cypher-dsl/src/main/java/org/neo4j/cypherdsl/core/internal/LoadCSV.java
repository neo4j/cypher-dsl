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
package org.neo4j.cypherdsl.core.internal;

import static org.apiguardian.api.API.Status.INTERNAL;

import java.net.URI;
import java.util.Objects;
import java.util.Optional;

import org.apiguardian.api.API;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.neo4j.cypherdsl.core.Clause;

/**
 * A representation of the {@code LOAD CSV} clause, including it's periodic commit and field terminator configuration.
 * Not meant to be used outside the Cypher-DSL directly. Will be changed without further notice.
 *
 * @author Michael J. Simons
 * @soundtrack Thees Uhlmann - #2
 * @since 2021.2.1
 */
@API(status = INTERNAL, since = "2021.2.1")
public final class LoadCSV implements Clause {

	private final URI uri;

	private final boolean withHeaders;

	private final String alias;

	private final String fieldTerminator;

	/**
	 * Constructs a new {@link LoadCSV} clause.
	 *
	 * @param uri Required uri
	 * @param withHeaders With or without headers
	 * @param alias The alias per row
	 */
	public LoadCSV(URI uri, boolean withHeaders, String alias) {
		this(uri, withHeaders, alias, null);
	}

	private LoadCSV(URI uri, boolean withHeaders, String alias, @Nullable String fieldTerminator) {
		this.uri = uri;
		this.withHeaders = withHeaders;
		this.alias = alias;
		this.fieldTerminator = fieldTerminator;
	}

	/**
	 * @return The uri of the csv file.
	 */
	public URI getUri() {
		return uri;
	}

	/**
	 * @return {@literal true} if headers are to be evaluated
	 */
	public boolean isWithHeaders() {
		return withHeaders;
	}

	/**
	 * @return The field terminator to use
	 */
	@Nullable
	public String getFieldTerminator() {
		return fieldTerminator;
	}

	/**
	 * @return The alias for one row in the csv file
	 */
	public String getAlias() {
		return alias;
	}

	/**
	 * Creates a new {@link LoadCSV LOAD CSV clause} with the given field terminator
	 * @param newFieldTerminator the new field terminator
	 * @return A new instance or this instance if the terminator hasn't changed
	 */
	@NotNull @Contract(pure = true)
	public LoadCSV withFieldTerminator(@Nullable final String newFieldTerminator) {

		String value = Optional.ofNullable(newFieldTerminator).map(String::trim)
			.filter(v -> !v.isEmpty()).orElse(null);

		if (Objects.equals(this.fieldTerminator, value)) {
			return this;
		}

		return new LoadCSV(uri, withHeaders, alias, value);
	}
}
