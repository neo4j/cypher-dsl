/*
 * Copyright (c) 2019-2023 "Neo4j,"
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
package org.neo4j.cypherdsl.core.fump;

import java.util.ArrayDeque;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.neo4j.cypherdsl.build.annotations.RegisterForReflection;
import org.neo4j.cypherdsl.core.Condition;
import org.neo4j.cypherdsl.core.Create;
import org.neo4j.cypherdsl.core.Delete;
import org.neo4j.cypherdsl.core.IdentifiableElement;
import org.neo4j.cypherdsl.core.KeyValueMapEntry;
import org.neo4j.cypherdsl.core.Match;
import org.neo4j.cypherdsl.core.Merge;
import org.neo4j.cypherdsl.core.Named;
import org.neo4j.cypherdsl.core.Node;
import org.neo4j.cypherdsl.core.NodeLabel;
import org.neo4j.cypherdsl.core.Operator;
import org.neo4j.cypherdsl.core.PatternElement;
import org.neo4j.cypherdsl.core.PropertyContainer;
import org.neo4j.cypherdsl.core.Relationship;
import org.neo4j.cypherdsl.core.SymbolicName;
import org.neo4j.cypherdsl.core.With;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.fump.SomeGoodNameForANNonSTCComparison.Clause;
import org.neo4j.cypherdsl.core.internal.ReflectiveVisitor;
import org.neo4j.cypherdsl.core.internal.ScopingStrategy;
import org.neo4j.cypherdsl.core.renderer.Configuration;
import org.neo4j.cypherdsl.core.renderer.GeneralizedRenderer;
import org.neo4j.cypherdsl.core.renderer.Renderer;

/**
 * @author Michael J. Simons
 * @soundtrack Avenger - Prayers Of Steel
 * @since TBA
 */
@RegisterForReflection
public class Thing extends ReflectiveVisitor {

	private static final Configuration CYPHER_RENDERER_CONFIGURATION = Configuration.newConfig().alwaysEscapeNames(false).build();

	private static final GeneralizedRenderer RENDERER = Renderer.getRenderer(CYPHER_RENDERER_CONFIGURATION, GeneralizedRenderer.class);

	/**
	 * Constant class name for skipping compounds, not inclined to make this type public.
	 */
	private static final String TYPE_OF_COMPOUND_CONDITION = "org.neo4j.cypherdsl.core.CompoundCondition";

	private Clause currentClause = Clause.UNKNOWN;

	private final AtomicReference<PatternElement> currentPatternElement = new AtomicReference<>();

	private final Set<Token> tokens = new HashSet<>();

	private final Set<Property> properties = new HashSet<>();

	private final Set<SomeGoodNameForANNonSTCComparison> conditions = new HashSet<>();

	private final Deque<Map<SymbolicName, PatternElement>> patternLookup = new ArrayDeque<>();

	private final Deque<Condition> currentConditions = new ArrayDeque<>();

	private final ScopingStrategy scopingStrategy;

	// TODO make private
	public Thing() {

		this.scopingStrategy = ScopingStrategy.create(
			List.of((cause, imports) -> {
				Map<SymbolicName, PatternElement> currentScope = patternLookup.isEmpty() ? Collections.emptyMap() : patternLookup.peek();
				Map<SymbolicName, PatternElement> newScope = new HashMap<>();
				for (IdentifiableElement e : imports) {
					if (e instanceof SymbolicName s && currentScope.containsKey(s)) {
						newScope.put(s, currentScope.get(s));
					} else if (e instanceof Named n && e instanceof PatternElement p) {
						newScope.put(n.getRequiredSymbolicName(), p);
					}
				}
				patternLookup.push(newScope);
			}),
			List.of((cause, exports) -> {
				Map<SymbolicName, PatternElement> previousScope = patternLookup.pop();
				Map<SymbolicName, PatternElement> currentScope = patternLookup.isEmpty() ? Collections.emptyMap() : patternLookup.peek();
				for (IdentifiableElement e : exports) {
					if (e instanceof SymbolicName s && previousScope.containsKey(s)) {
						currentScope.put(s, previousScope.get(s));
					} else if (e instanceof Named n && e instanceof PatternElement p) {
						currentScope.put(n.getRequiredSymbolicName(), p);
					}
				}
			})
		);
		this.patternLookup.push(new HashMap<>());
	}

	// TODO make package private
	public Things getResult() {
		return new Things(this.tokens, this.properties, this.conditions);
	}

	@Override
	protected boolean preEnter(Visitable visitable) {
		scopingStrategy.doEnter(visitable);
		return true;
	}

	@Override
	protected void postLeave(Visitable visitable) {
		scopingStrategy.doLeave(visitable);
	}

	void enter(Match match) {
		currentClause = Clause.MATCH;
	}

	void leave(Match match) {
		currentClause = Clause.UNKNOWN;
	}

	void enter(Create create) {
		currentClause = Clause.CREATE;
	}

	void leave(Create create) {
		currentClause = Clause.UNKNOWN;
	}

	void enter(Merge merge) {
		currentClause = Clause.MERGE;
	}

	void leave(Merge merge) {
		currentClause = Clause.UNKNOWN;
	}

	void enter(Delete delete) {
		currentClause = Clause.DELETE;
	}

	void leave(Delete delete) {
		currentClause = Clause.UNKNOWN;
	}

	void enter(With with) {
		currentClause = Clause.WITH;
	}

	void leave(With with) {
		currentClause = Clause.UNKNOWN;
	}

	void enter(Node node) {

		node.getSymbolicName().ifPresent(s -> store(s, node));
		currentPatternElement.compareAndSet(null, node);
	}

	void enter(KeyValueMapEntry mapEntry) {

		var owner = currentPatternElement.get();
		if (owner == null) {
			return;
		}

		Property property;
		if (owner instanceof Node node) {
			property = new Property(node.getLabels().stream().map(Token::label).collect(Collectors.toSet()), mapEntry.getKey());
		} else if (owner instanceof Relationship relationship) {
			property = new Property(relationship.getDetails().getTypes().stream().map(Token::type).collect(Collectors.toSet()), mapEntry.getKey());
		} else {
			property = null;
		}

		if (property == null) {
			return;
		}
		this.properties.add(property);
		var left = ((PropertyContainer) owner).getSymbolicName().map(s -> s.getValue() + ".").or(() -> Optional.of("")).map(v -> v + property.name()).get();
		this.conditions.add(new SomeGoodNameForANNonSTCComparison(currentClause, property, left, Operator.EQUALITY, RENDERER.render(mapEntry.getValue())));
	}

	void leave(Node node) {
		currentPatternElement.compareAndSet(node, null);
	}

	void enter(Relationship relationship) {

		relationship.getSymbolicName().ifPresent(s -> store(s, relationship));
		currentPatternElement.compareAndSet(null, relationship);
	}

	void leave(Relationship relationship) {
		currentPatternElement.compareAndSet(relationship, null);
	}

	void enter(org.neo4j.cypherdsl.core.Property property) {

		if (property.getNames().size() != 1) {
			return;
		}
		var lookup = property.getNames().get(0);
		if (lookup.isDynamicLookup()) {
			return;
		}

		if (!(property.getContainerReference() instanceof SymbolicName s)) {
			return;
		}

		var storedProperty = new AtomicReference<Property>();
		var patternElement = lookup(s);

		Function<String, Token> mapper;
		Stream<String> tokenStream;

		if (patternElement instanceof Node node) {
			mapper = Token::label;
			tokenStream = node.getLabels().stream().map(NodeLabel::getValue);
		} else if (patternElement instanceof Relationship relationship) {
			mapper = Token::type;
			tokenStream = relationship.getDetails().getTypes().stream();
		} else {
			return;
		}

		lookup.accept(segment -> {
			if (segment instanceof SymbolicName name) {
				storedProperty.set(new Property(tokenStream.map(mapper).collect(Collectors.toSet()), name.getValue()));
			}
		});
		properties.add(storedProperty.get());
		if (inCurrentCondition(property)) {
			conditions.add(extractComparison(storedProperty.get(), currentConditions.peek()));
		}
	}

	private boolean inCurrentCondition(org.neo4j.cypherdsl.core.Property property) {
		var currentCondition = this.currentConditions.peek();

		if (currentCondition == null) {
			return false;
		}

		var result = new AtomicBoolean();
		currentCondition.accept(segment -> {
			if (segment == property) {
				result.compareAndSet(false, true);
			}
		});
		return result.get();
	}

	private SomeGoodNameForANNonSTCComparison extractComparison(Property property, Condition condition) {

		AtomicReference<String> left = new AtomicReference<>();
		AtomicReference<Operator> op = new AtomicReference<>();
		AtomicReference<String> right = new AtomicReference<>();
		condition.accept(new Visitor() {
			int cnt;

			@Override
			public void enter(Visitable segment) {
				if (++cnt != 2) {
					return;
				}
				var cypher = RENDERER.render(segment);
				if (segment instanceof Operator operator) {
					op.compareAndSet(null, operator);
				} else if (!left.compareAndSet(null, cypher)) {
					right.compareAndSet(null, cypher);
				}
			}

			@Override
			public void leave(Visitable segment) {
				--cnt;
			}
		});
		return new SomeGoodNameForANNonSTCComparison(currentClause, property, left.get(), op.get(), right.get());
	}

	void enter(NodeLabel label) {
		this.tokens.add(new Token(Token.Type.NODE_LABEL, label.getValue()));
	}

	void enter(Relationship.Details details) {
		details.getTypes().stream().map(Token::type).forEach(tokens::add);
	}

	PatternElement lookup(SymbolicName s) {
		if (patternLookup.isEmpty()) {
			throw new IllegalStateException("Invalid scope");
		}
		return patternLookup.peek().get(s);
	}

	void enter(Condition condition) {
		if (TYPE_OF_COMPOUND_CONDITION.equals(condition.getClass().getName())) {
			return;
		}
		this.currentConditions.push(condition);
	}

	void leave(Condition condition) {
		if (TYPE_OF_COMPOUND_CONDITION.equals(condition.getClass().getName())) {
			return;
		}
		this.currentConditions.pop();
	}

	void store(SymbolicName s, PatternElement patternElement) {
		if (patternLookup.isEmpty()) {
			throw new IllegalStateException("Invalid scope");
		}
		var currentScope = patternLookup.peek();
		// Don't overwrite in same scope or when imported
		if (currentScope.containsKey(s) && scopingStrategy.getCurrentImports().contains(s)) {
			return;
		}
		currentScope.put(s, patternElement);
	}
}
