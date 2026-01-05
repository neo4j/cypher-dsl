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

import java.util.Arrays;
import java.util.Objects;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;

import static org.apiguardian.api.API.Status.INTERNAL;

/**
 * This type is used in sub-queries, both for sub-queries with an implicit scope such as
 * {@literal EXISTS{}} and {@literal COUNT{}} and full sub-queries. In the latter case, it
 * will create and visite multiple {@link With with-instances} so that the import actually
 * shadows the outer scope.
 *
 * @author Michael J. Simons
 * @param imports the imported expressions
 * @param renames the renamed expressions, shadowing the outer scope
 * @since 2023.1.0
 */
@API(status = INTERNAL, since = "2023.1.0")
record ImportingWith(With imports, With renames) implements Visitable {

	ImportingWith() {
		this(null, null);
	}

	static ImportingWith of(IdentifiableElement... imports) {

		With optionalImports;
		With optionalRenames;

		ExpressionList returnItems = new ExpressionList(Arrays.stream(imports).map(i -> {
			if (i instanceof AliasedExpression aliasedExpression) {
				var delegate = aliasedExpression.getDelegate();
				if (delegate instanceof Literal<?>) {
					return null;
				}
				return delegate;
			}
			else {
				return i.asExpression();
			}
		}).filter(Objects::nonNull).toList());

		optionalImports = returnItems.isEmpty() ? null : new With(false, returnItems, null, null, null, null);

		returnItems = new ExpressionList(Arrays.stream(imports)
			.filter(AliasedExpression.class::isInstance)
			.map(Expression.class::cast)
			.toList());

		optionalRenames = returnItems.isEmpty() ? null : new With(false, returnItems, null, null, null, null);

		return new ImportingWith(optionalImports, optionalRenames);
	}

	@Override
	public void accept(Visitor visitor) {
		Visitable.visitIfNotNull(this.imports, visitor);
		Visitable.visitIfNotNull(this.renames, visitor);
	}
}
