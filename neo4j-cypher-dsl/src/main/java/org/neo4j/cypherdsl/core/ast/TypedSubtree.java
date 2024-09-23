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
package org.neo4j.cypherdsl.core.ast;

import static org.apiguardian.api.API.Status.INTERNAL;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.apiguardian.api.API;

/**
 * This class helps to group items of the same type on the same level of the tree into a list structure that can be
 * recognized by visitors.
 *
 * @author Michael J. Simons
 * @param <T>    The children's type
 * @since 1.0
 */
@API(status = INTERNAL, since = "1.0")
public abstract class TypedSubtree<T extends Visitable> implements Visitable {

	/**
	 * The content of this typed subtree.
	 */
	protected final List<T> children;

	/**
	 * Creates a new typed subtree with the given content.
	 *
	 * @param children The content of this subtree.
	 */
	@SafeVarargs
	@SuppressWarnings("varargs")
	protected TypedSubtree(T... children) {

		this.children = Arrays.asList(children);
	}

	/**
	 * Creates a new typed subtree with the given content.
	 *
	 * @param children The content of this subtree.
	 */
	protected TypedSubtree(Collection<T> children) {

		this.children = new ArrayList<>(children);
	}

	@Override
	public final void accept(Visitor visitor) {

		visitor.enter(this);
		this.children.forEach(child -> prepareVisit(child).accept(visitor));
		visitor.leave(this);
	}

	/**
	 * A hook for interfere with the visitation of child elements.
	 *
	 * @param child The current child element
	 * @return The visitable that has been prepared
	 */
	protected Visitable prepareVisit(T child) {
		return child;
	}

	@API(status = INTERNAL)
	protected List<T> getChildren() {
		return children;
	}

	@API(status = INTERNAL)
	public String separator() {
		return ", ";
	}
}
