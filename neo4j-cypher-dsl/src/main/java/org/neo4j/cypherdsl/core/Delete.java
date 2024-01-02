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
package org.neo4j.cypherdsl.core;

import static org.apiguardian.api.API.Status.STABLE;
import static org.apiguardian.api.API.Status.INTERNAL;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitor;

/**
 * See <a href="https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/Delete.html">Delete</a>.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public final class Delete extends AbstractClause implements UpdatingClause {

	/**
	 * Creates a {@literal DELETE} clause deleting the given items.
	 *
	 * @param toBeDeleted The item to be deleted
	 * @param more        More items to be deleted
	 * @return A {@link Delete} clause
	 * @since 2023.4.0
	 */
	static Delete delete(Expression toBeDeleted, Expression... more) {
		return delete(false, toBeDeleted, more);
	}

	/**
	 * Creates a {@literal DELETE} clause deleting the given items.
	 *
	 * @param detach      Set to {@literal true} for {@literal DELETE} clause detaching all items
	 * @param toBeDeleted The item to be deleted
	 * @param more        More items to be deleted
	 * @return A {@link Delete} clause
	 * @since 2023.4.0
	 */
	static Delete delete(boolean detach, Expression toBeDeleted, Expression... more) {
		if (more == null || more.length == 0) {
			return new Delete(new ExpressionList(List.of(toBeDeleted)), detach);
		}

		List<Expression> finalExpressionList = new ArrayList<>();
		finalExpressionList.add(toBeDeleted);
		Collections.addAll(finalExpressionList, more);

		return new Delete(new ExpressionList(finalExpressionList), detach);
	}

	/**
	 * Creates a detaching {@literal DELETE} clause deleting the given items.
	 *
	 * @param toBeDeleted The item to be deleted
	 * @param more        More items to be deleted
	 * @return A {@link Delete} clause
	 * @since 2023.4.0
	 */
	static Delete detachDelete(Expression toBeDeleted, Expression... more) {
		return delete(true, toBeDeleted, more);
	}

	private final ExpressionList deleteItems;

	private final boolean detach;

	Delete(ExpressionList deleteItems, boolean detach) {
		this.deleteItems = deleteItems;
		this.detach = detach;
	}

	/**
	 * @return True, if the {@code DETACH} keyword needs to be included.
	 */
	@API(status = INTERNAL)
	public boolean isDetach() {
		return detach;
	}

	@Override
	public void accept(Visitor visitor) {
		visitor.enter(this);
		deleteItems.accept(visitor);
		visitor.leave(this);
	}
}
