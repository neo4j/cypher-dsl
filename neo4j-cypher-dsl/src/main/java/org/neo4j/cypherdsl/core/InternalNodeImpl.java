/*
 * Copyright (c) 2019-2022 "Neo4j,"
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

import java.util.List;

import org.apiguardian.api.API;
import org.jetbrains.annotations.NotNull;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * An internal implementation of the {@link NodeBase}. It's primary purpose is to have {@link NodeBase#named(SymbolicName)}
 * and {@link NodeBase#withProperties(MapExpression)} abstract method to enforce the correct return type. Otherwise one
 * could extend {@link NodeBase} without overriding those, ignoring unchecked casts and eventually running into a {@link ClassCastException}.
 *
 * @author Michael J. Simons
 * @since 2021.1.0
 */
@API(status = INTERNAL, since = "2021.1.0")
final class InternalNodeImpl extends NodeBase<InternalNodeImpl> {

	InternalNodeImpl(String primaryLabel, String... additionalLabels) {
		super(primaryLabel, additionalLabels);
	}

	InternalNodeImpl(SymbolicName symbolicName, List<NodeLabel> labels,
		Properties properties) {
		super(symbolicName, labels, properties);
	}

	InternalNodeImpl() {
	}

	InternalNodeImpl(SymbolicName symbolicName, String primaryLabel,
		MapExpression properties, String... additionalLabels) {
		super(symbolicName, primaryLabel, properties, additionalLabels);
	}

	@NotNull
	@Override
	public InternalNodeImpl named(SymbolicName newSymbolicName) {

		Assertions.notNull(newSymbolicName, "Symbolic name is required.");
		return new InternalNodeImpl(newSymbolicName, labels, properties);

	}

	@NotNull
	@Override
	public InternalNodeImpl withProperties(MapExpression newProperties) {

		return new InternalNodeImpl(this.getSymbolicName().orElse(null), labels, Properties.create(newProperties));
	}
}
