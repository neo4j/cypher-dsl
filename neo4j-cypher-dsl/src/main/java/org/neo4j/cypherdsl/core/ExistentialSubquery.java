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
package org.neo4j.cypherdsl.core;

import static org.apiguardian.api.API.Status.STABLE;

import java.util.List;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;

/**
 * An existential sub-query  can only be used in  a where clause. The  sub-query must consist only of  a match statement
 * which may have a {@code WHERE} clause on its own but is not allowed to return anything.
 *
 * @author Michael J. Simons
 * @soundtrack Die Ärzte - Seitenhirsch
 * @neo4j.version 4.0.0
 * @since 2020.1.2
 */
@API(status = STABLE, since = "2020.1.2")
@Neo4jVersion(minimum = "4.0.0")
public final class ExistentialSubquery implements SubqueryExpression, Condition {

	static ExistentialSubquery exists(Match fragment) {

		return new ExistentialSubquery(fragment);
	}

	static Condition exists(Statement statement, IdentifiableElement... imports) {
		return new ExistentialSubquery(statement, imports);
	}

	static Condition exists(List<PatternElement> patternElements, Where innerWhere) {
		return new ExistentialSubquery(patternElements, innerWhere);
	}

	private final ImportingWith importingWith;
	private final List<Visitable> fragments;
	private  final Where innerWhere;

	ExistentialSubquery(List<PatternElement> fragments, Where innerWhere) {
		this.fragments = List.of(Pattern.of(fragments));
		this.importingWith = new ImportingWith();
		this.innerWhere = innerWhere;
	}

	ExistentialSubquery(Match fragment) {
		this.fragments = List.of(fragment);
		this.importingWith = new ImportingWith();
		this.innerWhere = null;
	}

	ExistentialSubquery(Statement statement, IdentifiableElement... imports) {
		this.fragments = List.of(statement);
		this.importingWith = ImportingWith.of(imports);
		this.innerWhere = null;
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		importingWith.accept(visitor);
		fragments.forEach(v -> v.accept(visitor));
		Visitable.visitIfNotNull(innerWhere, visitor);
		visitor.leave(this);
	}
}
