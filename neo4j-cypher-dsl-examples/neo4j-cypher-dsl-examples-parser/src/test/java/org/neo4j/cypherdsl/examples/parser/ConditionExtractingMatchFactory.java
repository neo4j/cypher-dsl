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
package org.neo4j.cypherdsl.examples.parser;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

import org.neo4j.cypherdsl.core.Comparison;
import org.neo4j.cypherdsl.core.Condition;
import org.neo4j.cypherdsl.core.KeyValueMapEntry;
import org.neo4j.cypherdsl.core.Match;
import org.neo4j.cypherdsl.core.Node;
import org.neo4j.cypherdsl.core.Property;
import org.neo4j.cypherdsl.core.PropertyContainer;
import org.neo4j.cypherdsl.core.Relationship;
import org.neo4j.cypherdsl.core.SymbolicName;
import org.neo4j.cypherdsl.core.ast.EnterResult;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.ast.VisitorWithResult;
import org.neo4j.cypherdsl.parser.MatchDefinition;

abstract class ConditionExtractingMatchFactory implements Function<MatchDefinition, Match> {

	/**
	 * Conditions extracted from matching properties on nodes aka {@code (n:Movie {title: 'WHATEVER'}) }.
	 */
	protected final Map<Node, List<Condition>> nodeConditions = new LinkedHashMap<>();

	/**
	 * Conditions extracted from matching properties on nodes aka {@code () -[r:ACTED_IN {as: 'WHATEVER'}] ->() }.
	 */
	protected final Map<Relationship, List<Condition>> relationshipConditions = new LinkedHashMap<>();

	@Override
	public Match apply(MatchDefinition matchDefinition) {

		matchDefinition.patternElements().forEach(patternElement -> {
			patternElement.accept(outer -> {
				List<Condition> extractedConditions = new ArrayList<>();
				if (outer instanceof PropertyContainer container) {
					outer.accept(inner -> {
						if (inner instanceof KeyValueMapEntry kvm) {
							extractedConditions.add(container.property(kvm.getKey()).eq(kvm.getValue()));
						}
					});
				}
				if (outer instanceof Node node && node.getSymbolicName().isPresent()) {
					nodeConditions.computeIfAbsent(node, key -> new ArrayList<>()).addAll(extractedConditions);
				} else if (outer instanceof Relationship relationship && relationship.getSymbolicName().isPresent()) {
					relationshipConditions.computeIfAbsent(relationship, key -> new ArrayList<>()).addAll(extractedConditions);
				}
			});
		});

		Optional.ofNullable(matchDefinition.optionalWhere())
			.ifPresent(e -> e.accept(extractPropertyComparisons()));
		return apply0(matchDefinition);
	}

	abstract Match apply0(MatchDefinition matchDefinition);

	private Visitor extractPropertyComparisons() {
		return segment -> {
			if (segment instanceof Comparison c) {
				c.accept(inner -> {
					if (inner instanceof Property p) {
						var reference = new AtomicReference<SymbolicName>();
						p.accept(new VisitorWithResult() {
							@Override
							public EnterResult enterWithResult(Visitable propertyContent) {
								if (propertyContent instanceof SymbolicName symbolicName && reference.compareAndSet(null, symbolicName)) {
									return EnterResult.SKIP_CHILDREN;
								}
								return EnterResult.CONTINUE;
							}
						});
						if (reference.get() != null) {
							nodeConditions.forEach((k, v) -> {
								if (k.getSymbolicName().filter(reference.get()::equals).isPresent()) {
									v.add(c);
								}
							});
						}
					}
				});
			}
		};
	}
}