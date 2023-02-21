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
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import org.neo4j.cypherdsl.build.annotations.RegisterForReflection;
import org.neo4j.cypherdsl.core.IdentifiableElement;
import org.neo4j.cypherdsl.core.KeyValueMapEntry;
import org.neo4j.cypherdsl.core.Match;
import org.neo4j.cypherdsl.core.Named;
import org.neo4j.cypherdsl.core.Node;
import org.neo4j.cypherdsl.core.NodeLabel;
import org.neo4j.cypherdsl.core.PatternElement;
import org.neo4j.cypherdsl.core.Relationship;
import org.neo4j.cypherdsl.core.SymbolicName;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.internal.ReflectiveVisitor;
import org.neo4j.cypherdsl.core.internal.ScopingStrategy;

/**
 * @author Michael J. Simons
 * @soundtrack Avenger - Prayers Of Steel
 * @since TBA
 */
@RegisterForReflection
public class Thing extends ReflectiveVisitor {

	private boolean inMatch = false;

	private final AtomicReference<PatternElement> currentPatternElement = new AtomicReference<>();

	private final Set<Token> tokens = new HashSet<>();

	private final Set<Property> properties = new HashSet<>();

	private final Deque<Map<SymbolicName, PatternElement>> patternLookup = new ArrayDeque<>();

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
		return new Things(this.tokens, this.properties);
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
		inMatch = true;
	}

	void leave(Match match) {
		inMatch = false;
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

		if (owner instanceof Node node) {
			this.properties.add(new Property(node.getLabels().stream().map(Token::label).collect(Collectors.toSet()), mapEntry.getKey()));
		} else if (owner instanceof Relationship relationship) {
			this.properties.add(new Property(relationship.getDetails().getTypes().stream().map(Token::type).collect(Collectors.toSet()), mapEntry.getKey()));
		}
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

		if (property.getContainerReference() instanceof SymbolicName s) {
			var patternElement = lookup(s);
			if (patternElement instanceof Node node) {
				lookup.accept(segment -> {
					if (segment instanceof SymbolicName name) {
						properties.add(new Property(node.getLabels().stream().map(Token::label).collect(Collectors.toSet()), name.getValue()));
					}
				});
			} else if (patternElement instanceof Relationship relationship) {
				lookup.accept(segment -> {
					if (segment instanceof SymbolicName name) {
						properties.add(new Property(relationship.getDetails().getTypes().stream().map(Token::type).collect(Collectors.toSet()), name.getValue()));
					}
				});
			}
		}
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
		var patternElement = patternLookup.peek().get(s);
		return patternElement;
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
