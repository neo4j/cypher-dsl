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
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.internal.ProcedureName;
import org.neo4j.cypherdsl.core.internal.YieldItems;

import static org.apiguardian.api.API.Status.INTERNAL;

/**
 * An internal implementation of a {@link ProcedureCall} to distinguish between calls that
 * return ("yield") a set of things and those who don't.
 *
 * @author Michael J. Simons
 * @since 2021.2.1
 */
@API(status = INTERNAL, since = "2021.2.1")
class ProcedureCallImpl extends AbstractStatement implements ProcedureCall {

	private final ProcedureName name;

	private final Arguments arguments;

	private final YieldItems yieldItems;

	private final Where optionalWhere;

	private ProcedureCallImpl(ProcedureName name, Arguments arguments, YieldItems yieldItems, Where optionalWhere) {

		this.name = name;
		this.arguments = (arguments != null) ? arguments : new Arguments();
		this.yieldItems = yieldItems;
		this.optionalWhere = optionalWhere;
	}

	static ProcedureCall create(ProcedureName name, Arguments arguments, YieldItems yieldItems, Where optionalWhere) {

		if (yieldItems != null) {
			return new ProcedureCallImplWithResult(name, arguments, yieldItems, optionalWhere);
		}
		else {
			return new ProcedureCallImpl(name, arguments, null, optionalWhere);
		}
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		this.name.accept(visitor);
		Visitable.visitIfNotNull(this.arguments, visitor);
		Visitable.visitIfNotNull(this.yieldItems, visitor);
		Visitable.visitIfNotNull(this.optionalWhere, visitor);
		visitor.leave(this);
	}

	static final class ProcedureCallImplWithResult extends ProcedureCallImpl implements ResultStatement {

		private ProcedureCallImplWithResult(ProcedureName name, Arguments arguments, YieldItems yieldItems,
				Where optionalWhere) {
			super(name, arguments, yieldItems, optionalWhere);
		}

	}

}
