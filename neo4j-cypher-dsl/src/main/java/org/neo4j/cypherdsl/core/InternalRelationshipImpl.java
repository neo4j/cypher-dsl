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

import static org.apiguardian.api.API.Status.INTERNAL;

import org.apiguardian.api.API;

/**
 * An internal implementation of the {@link RelationshipBase}. It's primary purpose is to have {@link RelationshipBase#named(SymbolicName)}
 * and {@link RelationshipBase#withProperties(MapExpression)} abstract method to enforce the correct return type. Otherwise one
 * could extend {@link RelationshipBase} without overriding those, ignoring unchecked casts and eventually running into a {@link ClassCastException}.
 *
 * @author Michael J. Simons
 * @since 2021.1.0
 */
@API(status = INTERNAL, since = "2021.1.0")
final class InternalRelationshipImpl extends RelationshipBase<NodeBase<?>, NodeBase<?>, InternalRelationshipImpl> {

	InternalRelationshipImpl(SymbolicName symbolicName, Node left,
		Direction direction, Node right, String... types) {
		super(symbolicName, left, direction, right, types);
	}

	InternalRelationshipImpl(SymbolicName symbolicName, Node left,
		Direction direction, Properties properties, Node right, String... types) {
		super(symbolicName, left, direction, properties, right, types);
	}

	InternalRelationshipImpl(Node left, Details details, Node right) {
		super(left, details, right);
	}

	@Override
	public InternalRelationshipImpl named(SymbolicName newSymbolicName) {

		return new InternalRelationshipImpl(this.left, this.details.named(newSymbolicName), this.right);
	}

	@Override
	public InternalRelationshipImpl withProperties(MapExpression newProperties) {

		return new InternalRelationshipImpl(this.left, this.details.with(Properties.create(newProperties)), this.right);
	}
}
