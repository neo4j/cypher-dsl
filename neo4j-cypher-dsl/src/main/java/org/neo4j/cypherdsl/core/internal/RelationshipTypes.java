/*
 * Copyright (c) 2019-2024 "Neo4j,"
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

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitable;

/**
 * See <a href="https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/RelationshipDetail.html#RelationshipTypes">RelationshipTypes</a>
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = INTERNAL, since = "1.0")
public final class RelationshipTypes implements Visitable {

	private final List<String> values;

	public static RelationshipTypes of(String... types) {

		List<String> listOfTypes = Arrays.stream(types)
			.filter(type -> !(type == null || type.isEmpty()))
			.collect(Collectors.toList());

		return new RelationshipTypes(listOfTypes);
	}

	private RelationshipTypes(List<String> values) {
		this.values = values;
	}

	/**
	 * @return the list of types. The types are not escaped and must be escaped prior to rendering.
	 */
	public List<String> getValues() {
		return values;
	}
}
