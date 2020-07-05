/*
 * Copyright (c) 2019-2020 "Neo4j,"
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

import org.neo4j.cypherdsl.core.support.Visitable;
import org.neo4j.cypherdsl.core.support.Visitor;

/**
 * See <a href="https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/StandaloneCall.html">StandaloneCall</a>.
 *
 * @author Michael J. Simons
 * @soundtrack Apocalyptica - Cell-0
 * @since 2020.1
 */
public final class StandaloneCall implements Visitable, Statement {

	private final ProcedureName name;

	private final Arguments arguments;

	static StandaloneCall of(String procedureName) {

		return new StandaloneCall(new ProcedureName(null, procedureName));
	}

	static StandaloneCall of(SymbolicName namespace, String procedureName) {

		return new StandaloneCall(new ProcedureName(new Namespace(namespace), procedureName));
	}

	static StandaloneCall of(SymbolicName namespace, String procedureName, Expression... arguments) {

		Arguments argumentsList = null;
		if (arguments != null && arguments.length > 0) {
			argumentsList = new Arguments(arguments);
		}

		return new StandaloneCall(new ProcedureName(new Namespace(namespace), procedureName), argumentsList);
	}

	StandaloneCall(ProcedureName name) {

		this.name = name;
		this.arguments = null;
	}

	StandaloneCall(ProcedureName name, Arguments arguments) {

		this.name = name;
		this.arguments = arguments;
	}

	/**
	 * Creates a new standalone call with the given argument list.
	 *
	 * @param args The new arguments.
	 * @return A new call (the old one is not changed).
	 */
	public StandaloneCall withArgs(Expression... args) {

		if (args == null || args.length == 0) {

			return new StandaloneCall(this.name);
		}

		return new StandaloneCall(this.name, new Arguments(args));
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		this.name.accept(visitor);
		Visitable.visitIfNotNull(arguments, visitor);
		visitor.leave(this);
	}
}
