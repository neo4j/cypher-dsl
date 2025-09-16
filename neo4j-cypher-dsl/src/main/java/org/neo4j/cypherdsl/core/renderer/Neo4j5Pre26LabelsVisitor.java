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
package org.neo4j.cypherdsl.core.renderer;

import java.util.EnumSet;
import java.util.List;

import org.neo4j.cypherdsl.build.annotations.RegisterForReflection;
import org.neo4j.cypherdsl.core.Labels;
import org.neo4j.cypherdsl.core.ListExpression;
import org.neo4j.cypherdsl.core.ListLiteral;
import org.neo4j.cypherdsl.core.NodeLabel;
import org.neo4j.cypherdsl.core.Parameter;
import org.neo4j.cypherdsl.core.StringLiteral;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.internal.SchemaNamesBridge;

/**
 * Express colon conjunctions containing string resolvable expressions as static labels
 * prior to Neo4j 5.26.
 *
 * @author Michael J. Simons
 * @since 2025.0.2
 */
@RegisterForReflection(allDeclaredConstructors = true)
final class Neo4j5Pre26LabelsVisitor implements Visitor {

	private final DefaultVisitor delegate;

	Neo4j5Pre26LabelsVisitor(DefaultVisitor delegate) {
		this.delegate = delegate;
	}

	void render(Object visitable) {
		if (visitable instanceof Parameter<?> parameter) {
			render(parameter.getValue());
		}
		else if (visitable instanceof ListExpression listExpression) {
			listExpression.accept(segment -> {
				if (segment instanceof ListExpression) {
					return;
				}
				render(segment);
			});
		}
		else if (visitable instanceof ListLiteral listLiteral) {
			for (var literal : listLiteral.getContent()) {
				render(literal);
			}
		}
		else if (visitable instanceof StringLiteral stringLiteral) {
			renderAsLabel(stringLiteral.getContent().toString());
		}
		else if (visitable instanceof List<?> list) {
			for (var item : list) {
				if (item instanceof String string) {
					renderAsLabel(string);
				}
			}
		}
		else if (visitable instanceof String string) {
			renderAsLabel(string);
		}
		else if (visitable instanceof NodeLabel label) {
			renderAsLabel(label.getValue());
		}
		else if (!"org.neo4j.cypherdsl.core.ExpressionList".equals(visitable.getClass().getName())) {
			throw new IllegalArgumentException("Cannot render the given Labels in a Cypher pre 5.26 compatible way");
		}
	}

	private void renderAsLabel(String string) {
		this.delegate.builder.append(":")
			.append(SchemaNamesBridge.sanitize(string, this.delegate.alwaysEscapeNames).orElseThrow());
	}

	@Override
	public void enter(Visitable segment) {
		if (!(segment instanceof Labels labels)) {
			return;
		}
		if (labels.getLhs() == null && labels.getRhs() == null
				&& EnumSet.of(Labels.Type.LEAF, Labels.Type.COLON_CONJUNCTION).contains(labels.getType())) {
			for (var value : labels.getValue()) {
				render(value.visitable());
			}
		}
		else {
			this.delegate.enter(labels);
		}
	}

}
