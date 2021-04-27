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
package org.neo4j.cypherdsl.core;

import static org.apiguardian.api.API.Status.EXPERIMENTAL;

import java.net.URI;

import org.apiguardian.api.API;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

/**
 * Exposes methods to configure a {@code LOAD CSV} clause.
 *
 * @author Michael J. Simons
 * @since 2020.2.1
 */
@API(status = EXPERIMENTAL, since = "2020.2.1")
public interface ExposesLoadCSV {

	/**
	 * Starts building a {@code LOAD CSV}.
	 *
	 * @param from The {@link URI} to load data from. Any uri that is resolvable by the database itself is valid.
	 * @return An ongoing definition of a {@code LOAD CSV} clause
	 */
	@NotNull @Contract(pure = true)
	default LoadCSVStatementBuilder.OngoingLoadCSV loadCSV(URI from) {
		return loadCSV(from, false);
	}

	/**
	 * Starts building a {@code LOAD CSV}.
	 *
	 * @param from        The {@link URI} to load data from. Any uri that is resolvable by the database itself is valid.
	 * @param withHeaders Set to {@literal true} if the csv file contains header
	 * @return An ongoing definition of a {@code LOAD CSV} clause
	 */
	@NotNull @Contract(pure = true)
	LoadCSVStatementBuilder.OngoingLoadCSV loadCSV(URI from, boolean withHeaders);
}
