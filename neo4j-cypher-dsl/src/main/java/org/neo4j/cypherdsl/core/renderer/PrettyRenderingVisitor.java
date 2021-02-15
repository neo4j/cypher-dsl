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
package org.neo4j.cypherdsl.core.renderer;

import org.neo4j.cypherdsl.core.Create;
import org.neo4j.cypherdsl.core.KeyValueMapEntry;
import org.neo4j.cypherdsl.core.MapExpression;
import org.neo4j.cypherdsl.core.Match;
import org.neo4j.cypherdsl.core.Operator;
import org.neo4j.cypherdsl.core.PropertyLookup;
import org.neo4j.cypherdsl.core.Return;
import org.neo4j.cypherdsl.core.Set;
import org.neo4j.cypherdsl.core.Where;
import org.neo4j.cypherdsl.core.With;

import java.util.Optional;

/**
 * This is a implementation of a visitor to the Cypher AST created by the Cypher builder
 * based on the {@link RenderingVisitor reflective visitor}.
 * <p>
 * It takes care of separating elements of sub trees containing the element type with a separator and provides pairs of
 * {@code enter} / {@code leave} for the structuring elements of the Cypher AST as needed.
 * <p>
 * This rendering visitor is not meant to be used outside framework code and we don't give any guarantees on the format
 *
 * @author Andreas Berger
 */
@SuppressWarnings("unused")
class PrettyRenderingVisitor extends RenderingVisitor {

	private int level;
	private int topCount;

	@Override
	void enter(Where where) {
		if (currentVisitedElements.stream().noneMatch(visitable -> visitable instanceof Return)) {
			builder.append("\nWHERE ");
		} else {
			super.enter(where);
		}
	}

	@Override
	void enter(Return returning) {
		builder.append("\n");
		super.enter(returning);
	}

	@Override
	void enter(With with) {
		builder.append("\n");
		super.enter(with);
	}

	@Override
	void enter(Set set) {
		builder.append("\n");
		super.enter(set);
	}

	@Override
	void enter(Match match) {
		if (topCount > 0) {
			builder.append("\n");
		}
		super.enter(match);
	}

	@Override
	void leave(Match match) {
		topCount++;
		super.leave(match);
	}

	@Override
	void enter(Create create) {
		if (topCount > 0) {
			builder.append("\n");
		}
		super.enter(create);
	}

	@Override
	void leave(Create create) {
		topCount++;
		super.leave(create);
	}

	@Override
	void enter(PropertyLookup propertyLookup) {
		if (currentVisitedElements.stream().skip(1).limit(1).anyMatch(visitable -> visitable instanceof MapExpression)) {
			builder.append("\n");
			indent(level);
		}
		super.enter(propertyLookup);
	}

	@Override
	void enter(KeyValueMapEntry map) {
		if (level > 0) {
			builder.append("\n");
			indent(level);
		}
		super.enter(map);
	}

	@Override
	void enter(Operator operator) {
		Operator.Type type = operator.getType();
		if (type == Operator.Type.LABEL) {
			return;
		}
		if (type != Operator.Type.PREFIX && operator != Operator.EXPONENTIATION) {
			builder.append(" ");
		}
		if (operator == Operator.OR || operator == Operator.AND || operator == Operator.XOR) {
			builder.append("\n\t");
		}
		builder.append(operator.getRepresentation());
		if (type != Operator.Type.POSTFIX && operator != Operator.EXPONENTIATION) {
			builder.append(" ");
		}
	}

	@Override
	void enter(MapExpression map) {
		level++;
		builder.append(" ");
		super.enter(map);
	}

	@Override
	void leave(MapExpression map) {
		level--;
		builder.append("\n");
		indent(level);
		super.leave(map);
	}

	@Override
	protected Optional<String> escapeName(CharSequence unescapedName) {
		if (unescapedName == null) {
			return Optional.empty();
		}
		return Optional.of(super.escapeIfNecessary(unescapedName.toString()));
	}

	private void indent(int indentionSize) {
		for (int i = 0; i < indentionSize; i++) {
			builder.append("\t");
		}
	}
}
