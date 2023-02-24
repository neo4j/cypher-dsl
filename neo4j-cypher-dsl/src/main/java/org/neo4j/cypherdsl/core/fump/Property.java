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
package org.neo4j.cypherdsl.core.fump;

import java.util.Collections;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;

/**
 * A property that has been  resolved. In case this property has been resolved for an  entity, the entity itself will be
 * defined by its set of tokens. Tokens are guaranteed to be sorted and will be of the same type.
 *
 * @author Michael J. Simons
 * @param name        The name of the resolved property
 * @param owningToken Zero or many owning tokens for a property
 * @since TBA
 */
public record Property(Set<Token> owningToken, String name) {

	public Property(String name) {
		this(Set.of(), name);
	}

	public Property(Token owningToken, String name) {
		this(Set.of(owningToken), name);
	}

	public Property {
		if (owningToken.stream().map(Token::type).distinct().count() > 1) {
			throw new IllegalArgumentException("Owning tokens are of multiple types");
		}
		owningToken = Collections.unmodifiableSet(new TreeSet<>(owningToken));
	}

	/**
	 * @return An optional, owning type.
	 */
	public Optional<Token.Type> owningType() {
		return owningToken.stream().map(Token::type).distinct().findFirst();
	}
}
