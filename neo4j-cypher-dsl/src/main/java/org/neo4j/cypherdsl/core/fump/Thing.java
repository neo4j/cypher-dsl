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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import org.neo4j.cypherdsl.build.annotations.RegisterForReflection;
import org.neo4j.cypherdsl.core.IdentifiableElement;
import org.neo4j.cypherdsl.core.KeyValueMapEntry;
import org.neo4j.cypherdsl.core.Match;
import org.neo4j.cypherdsl.core.Node;
import org.neo4j.cypherdsl.core.NodeLabel;
import org.neo4j.cypherdsl.core.PatternElement;
import org.neo4j.cypherdsl.core.PropertyLookup;
import org.neo4j.cypherdsl.core.Statement;
import org.neo4j.cypherdsl.core.SymbolicName;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.internal.ReflectiveVisitor;

/**
 * @author Michael J. Simons
 * @since TBA
 * @soundtrack Avenger - Prayers Of Steel
 */
@RegisterForReflection
public class Thing extends ReflectiveVisitor {

	private boolean inMatch = false;

	private final AtomicReference<PatternElement> currentPatternElement = new AtomicReference<>();

	private final Set<Token> tokens = new HashSet<>();

	private final Set<Property> properties = new HashSet<>();

	// TODO this is bonkers without scope
	private final Map<SymbolicName, PatternElement> patternLookup = new HashMap<>();

	// TODO this is bonkers without scope


	// This is not going to be public API
	@Deprecated(forRemoval = true)
	public static void getThings(Statement statement) {
		var thing = new Thing();
		statement.accept(thing);
		System.out.println("labels matched " + thing.tokens.stream().filter(t -> t.type() == Token.Type.LABEL).map(Token::value).collect(Collectors.toSet()));
		thing.properties.forEach(p -> {
			System.out.printf("Matched property `%s` on label `%s`%n", p.name(), p.owningToken().stream().map(Token::value).collect(Collectors.joining(", ")));
		});
	}

	Thing() {
	}


	@Override
	protected boolean preEnter(Visitable visitable) {
		return true;
	}

	@Override
	protected void postLeave(Visitable visitable) {
	}

	void enter(Match match) {
		inMatch = true;
	}


	void leave(Match match) {
		inMatch = false;
	}

	void enter(Node node) {

		node.getSymbolicName().ifPresent(s -> {
			patternLookup.put(s, node);
		});
		currentPatternElement.compareAndSet(null, node);
	}

	void enter(KeyValueMapEntry mapEntry) {
		var owner = currentPatternElement.get();
		if (owner == null) {
			return;
		}

		if (owner instanceof Node node) {
			this.properties.add(new Property(mapEntry.getKey(), node.getLabels().stream().map(Token::of).collect(Collectors.toSet())));
		}
	}

	void leave(Node node) {
		currentPatternElement.compareAndSet(node, null);
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
			var patternElement = patternLookup.get(s);
			if (patternElement instanceof Node node) {
				lookup.accept(new Visitor() {
					@Override
					public void enter(Visitable segment) {
						if(segment instanceof SymbolicName name) {
							properties.add(new Property(name.getValue(), node.getLabels().stream().map(Token::of).collect(Collectors.toSet())));
						}
					}
				});
			}
		}

		// TODO more identifiable.
	}

	void enter(NodeLabel label) {
		this.tokens.add(new Token(Token.Type.LABEL, label.getValue()));
	}
}
