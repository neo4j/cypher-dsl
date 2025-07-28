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
package org.neo4j.cypherdsl.codegen.ogm;

import java.util.function.Function;

import javax.lang.model.element.TypeElement;
import javax.lang.model.util.SimpleElementVisitor8;

/**
 * Utility class that can be used to extract a {@link TypeElement} from a type
 * or anything else that one can retrieve
 * from the actual element.
 *
 * @author Michael J. Simons
 * @param <E> The type of the returned value
 * @soundtrack Metallica - Helping Hands… Live & Acoustic At The Masonic
 */
final class TypeElementVisitor<E> extends SimpleElementVisitor8<E, Void> {
	private final Function<TypeElement, E> delegate;

	TypeElementVisitor(Function<TypeElement, E> delegate) {
		this.delegate = delegate;
	}

	@Override
	public E visitType(TypeElement e, Void unused) {
		return delegate.apply(e);
	}
}
