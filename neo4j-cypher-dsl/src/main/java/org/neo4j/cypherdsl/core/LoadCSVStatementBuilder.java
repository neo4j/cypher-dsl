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

import java.net.URI;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.DefaultLoadCSVStatementBuilder.PrepareLoadCSVStatementImpl;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * A builder dedicated to the creation of the {@code LOAD CSV} statement.
 *
 * @author Michael J. Simons
 * @since 2021.2.1
 */
@API(status = STABLE, since = "2021.2.1")
public interface LoadCSVStatementBuilder extends StatementBuilder {

	/**
	 * Starts building a {@code LOAD CSV} clause by using a periodic commit.
	 * @param rate the rate to be used. No checks are done on the rate, the database will
	 * verify valid values.
	 * @return an ongoing definition of a {@code LOAD CSV} clause
	 */
	static ExposesLoadCSV usingPeriodicCommit(Integer rate) {
		return new PrepareLoadCSVStatementImpl(rate);
	}

	/**
	 * Starts building a {@code LOAD CSV}.
	 * @param from the {@link URI} to load data from. Any uri that is resolvable by the
	 * database itself is valid.
	 * @param withHeaders set to {@literal true} if the csv file contains header
	 * @return an ongoing definition of a {@code LOAD CSV} clause
	 */
	static OngoingLoadCSV loadCSV(URI from, boolean withHeaders) {
		return new PrepareLoadCSVStatementImpl(from, withHeaders);
	}

	/**
	 * Configure a field terminator in case the fields aren't separated with the default
	 * {@code ,}.
	 * @param fieldTerminator a new field terminator
	 * @return a statement builder supporting all available clauses
	 */
	StatementBuilder withFieldTerminator(String fieldTerminator);

	/**
	 * An instance of this interface will be provided after pointing the database to a
	 * valid {@link URI} of a CSV resource.
	 */
	interface OngoingLoadCSV {

		/**
		 * Configure the alias for each line contained in the CSV resource.
		 * @param alias the alias for each line
		 * @return a statement builder supporting all available clauses plus an option to
		 * configure the field terminator
		 */
		default LoadCSVStatementBuilder as(SymbolicName alias) {
			return as(alias.getValue());
		}

		/**
		 * Configure the alias for each line contained in the CSV resource.
		 * @param alias the alias for each line
		 * @return a statement builder supporting all available clauses plus an option to
		 * configure the field terminator
		 */
		LoadCSVStatementBuilder as(String alias);

	}

}
