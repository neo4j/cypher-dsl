/*
 * Copyright (c) 2019-2026 "Neo4j,"
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

import org.apiguardian.api.API;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * An element with an alias. An alias has a subtle difference to a symbolic name in
 * cypher. Nodes and relationships can have symbolic names which in turn can be aliased as
 * well.
 * <p>
 * Therefore, the Cypher generator needs both {@code Named} and {@code Aliased}.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public interface Aliased {

	/**
	 * {@return the alias}
	 */
	String getAlias();

	/**
	 * Turns this alias into a symbolic name that can be used as an {@link Expression}.
	 * @return a new symbolic name
	 */
	default SymbolicName asName() {
		return SymbolicName.of(this.getAlias());
	}

}
