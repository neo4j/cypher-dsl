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
package org.neo4j.cypherdsl.examples.parser;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;
import org.neo4j.cypherdsl.core.AliasedExpression;
import org.neo4j.cypherdsl.core.Clauses;
import org.neo4j.cypherdsl.core.Condition;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.Expression;
import org.neo4j.cypherdsl.core.KeyValueMapEntry;
import org.neo4j.cypherdsl.core.Match;
import org.neo4j.cypherdsl.core.Node;
import org.neo4j.cypherdsl.core.NodeLabel;
import org.neo4j.cypherdsl.core.Operation;
import org.neo4j.cypherdsl.core.PatternElement;
import org.neo4j.cypherdsl.core.Property;
import org.neo4j.cypherdsl.core.Relationship;
import org.neo4j.cypherdsl.core.Return;
import org.neo4j.cypherdsl.core.StringLiteral;
import org.neo4j.cypherdsl.core.SymbolicName;
import org.neo4j.cypherdsl.core.Where;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.renderer.Configuration;
import org.neo4j.cypherdsl.core.renderer.GeneralizedRenderer;
import org.neo4j.cypherdsl.core.renderer.Renderer;
import org.neo4j.cypherdsl.parser.CypherParser;
import org.neo4j.cypherdsl.parser.ExpressionCreatedEventType;
import org.neo4j.cypherdsl.parser.MatchDefinition;
import org.neo4j.cypherdsl.parser.Options;
import org.neo4j.cypherdsl.parser.PatternElementCreatedEventType;
import org.neo4j.cypherdsl.parser.ReturnDefinition;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatNoException;

/**
 * @author Michael J. Simons
 */
class CypherDSLParserExamplesTests {

	@Test
	void createASubqueryCallWithUserProvidedCypher() {

		// tag::example-using-input[]
		var userProvidedCypher = "MATCH (this)-[:LINK]-(o:Other) RETURN o as result"; // <.>
		var userStatement = CypherParser.parse(userProvidedCypher); // <.>

		var node = Cypher.node("Node").named("node");
		var result = Cypher.name("result");
		var cypher = Cypher // <.>
			.match(node)
			.call(// <.>
					userStatement, node.as("this"))
			.returning(result.project("foo", "bar"))
			.build()
			.getCypher();

		assertThat(cypher).isEqualTo("MATCH (node:`Node`) " + "CALL (node) {" + "WITH node AS this " // <.>
				+ "MATCH (this)-[:`LINK`]-(o:`Other`) RETURN o AS result" // <.>
				+ "} " + "RETURN result{.foo, .bar}");
		// end::example-using-input[]
	}

	@Test
	void extractLabels() {

		var cypher = "OPTIONAL MATCH (m) SET m:Foo REMOVE m:Bar WITH m CREATE (m:Person) SET n.name = 'John' RETURN n, m";

		class LabelCollector implements Function<Expression, Operation> {

			List<String> labelsSeen = new ArrayList<>();

			@Override
			public Operation apply(Expression expression) {
				Operation op = (Operation) expression;
				op.accept(segment -> {
					if (segment instanceof NodeLabel) {
						this.labelsSeen.add(((NodeLabel) segment).getValue());
					}
				});
				return op;
			}

		}

		var labelsAdded = new LabelCollector();
		var labelsRemoved = new LabelCollector();

		var options = Options.newOptions()
			.withCallback(ExpressionCreatedEventType.ON_SET_LABELS, Operation.class, labelsAdded)
			.withCallback(ExpressionCreatedEventType.ON_REMOVE_LABELS, Operation.class, labelsRemoved)

			.build();

		assertThatNoException().isThrownBy(() -> CypherParser.parse(cypher, options));
		assertThat(labelsAdded.labelsSeen).containsExactly("Foo");
		assertThat(labelsRemoved.labelsSeen).containsExactly("Bar");
	}

	@Test // GH-299
	void extractLabelsWithFilters() {

		var cypher = "OPTIONAL MATCH (m:Bazbar) SET m:Foo REMOVE m:Bar WITH m CREATE (m:Person) SET n.name = 'John' RETURN n, m";
		var labelsSeen = new HashSet<String>();
		var options = Options.newOptions().withLabelFilter((labelParsedEventType, strings) -> {

			// The event type could be used to distinguish the call site at which labels
			// where needed
			switch (labelParsedEventType) {
				case ON_REMOVE:
					break;
				case ON_SET:
					break;
				case ON_NODE_PATTERN:
					break;
			}
			labelsSeen.addAll(strings);
			return strings;
		}).build();

		assertThatNoException().isThrownBy(() -> CypherParser.parse(cypher, options));
		assertThat(labelsSeen).containsExactlyInAnyOrder("Bazbar", "Foo", "Bar", "Person");
	}

	@Test // GH-299
	void extractLabelsInMatchOrCreate() {

		var cypher = "OPTIONAL MATCH (m:Bazbar) SET m:Foo REMOVE m:Bar WITH m CREATE (m:Person) SET n.name = 'John' RETURN n, m";

		class LabelCollector implements UnaryOperator<PatternElement> {

			final Set<String> labelsSeen = new HashSet<>();

			@Override
			public PatternElement apply(PatternElement patternElement) {
				if (patternElement instanceof Node) {
					((Node) patternElement).getLabels().forEach(l -> this.labelsSeen.add(l.getValue()));
				}
				return patternElement;
			}

		}

		var labelsOnNodesCreated = new LabelCollector();
		var labelsOnNodesMatched = new LabelCollector();

		var options = Options.newOptions()
			.withCallback(PatternElementCreatedEventType.ON_CREATE, labelsOnNodesCreated)
			.withCallback(PatternElementCreatedEventType.ON_MATCH, labelsOnNodesMatched)
			.build();

		assertThatNoException().isThrownBy(() -> CypherParser.parse(cypher, options));
		assertThat(labelsOnNodesCreated.labelsSeen).containsExactly("Person");
		assertThat(labelsOnNodesMatched.labelsSeen).containsExactly("Bazbar");
	}

	@Test
	void rewriteQuery() {

		var cypher = "MATCH (n:Phone)-[:CALLED]->(o:Phone) RETURN *";

		class LabelCollector implements UnaryOperator<PatternElement> {

			@Override
			public PatternElement apply(PatternElement patternElement) {

				if (patternElement instanceof Relationship) {
					Relationship relationship = (Relationship) patternElement;
					if (relationship.getDetails().getTypes().contains("CALLED")) {

						var call = Cypher.node("Call").named("c");
						var left = relationship.getLeft();
						var right = relationship.getRight();
						return left.relationshipTo(call, "CALL").relationshipFrom(right, "CALL");
					}
				}
				return patternElement;
			}

		}

		var labelsOnNodesCreated = new LabelCollector();
		var labelsOnNodesMatched = new LabelCollector();

		var options = Options.newOptions()
			.withCallback(PatternElementCreatedEventType.ON_CREATE, labelsOnNodesCreated)
			.withCallback(PatternElementCreatedEventType.ON_MATCH, labelsOnNodesMatched)
			.build();
		var statement = CypherParser.parse(cypher, options);
		var rewritten = Renderer.getRenderer(Configuration.prettyPrinting()).render(statement);
		assertThat(rewritten).isEqualTo("MATCH (n:Phone)-[:CALL]->(c:Call)<-[:CALL]-(o:Phone)\n" + "RETURN *");
	}

	@Test // GH-299
	void allLabelsSeen() {

		var cypher = "OPTIONAL MATCH (m:Bazbar) SET m:Foo REMOVE m:Bar WITH m CREATE (m:Person) SET n.name = 'John' RETURN n, m";

		Set<String> labelsSeen = new HashSet<>();

		class LabelsInsideExpressions implements Function<Expression, Operation> {

			@Override
			public Operation apply(Expression expression) {
				Operation op = (Operation) expression;
				op.accept(segment -> {
					if (segment instanceof NodeLabel) {
						labelsSeen.add(((NodeLabel) segment).getValue());
					}
				});
				return op;
			}

		}

		class LabelsInsidePatterns implements UnaryOperator<PatternElement> {

			@Override
			public PatternElement apply(PatternElement patternElement) {
				if (patternElement instanceof Node) {
					((Node) patternElement).getLabels().forEach(l -> labelsSeen.add(l.getValue()));
				}
				return patternElement;
			}

		}

		var labelsInsideExpressions = new LabelsInsideExpressions();
		var labelsInsidePatterns = new LabelsInsidePatterns();

		var options = Options.newOptions()
			.withCallback(ExpressionCreatedEventType.ON_SET_LABELS, Operation.class, labelsInsideExpressions)
			.withCallback(ExpressionCreatedEventType.ON_REMOVE_LABELS, Operation.class, labelsInsideExpressions)
			.withCallback(PatternElementCreatedEventType.ON_CREATE, labelsInsidePatterns)
			.withCallback(PatternElementCreatedEventType.ON_MATCH, labelsInsidePatterns)
			.build();

		assertThatNoException().isThrownBy(() -> CypherParser.parse(cypher, options));
		assertThat(labelsSeen).containsExactlyInAnyOrder("Foo", "Bar", "Bazbar", "Person");
	}

	@Test // GH-302
	void trackChangedPropertiesToNodes() {

		var nodes = new HashMap<String, Node>();
		UnaryOperator<PatternElement> nodesCollector = patternElement -> {
			patternElement.accept(segment -> {
				if (segment instanceof Node) {
					Node node = (Node) segment;
					node.getSymbolicName().map(SymbolicName::getValue).ifPresent(n -> nodes.put(n, node));
				}
			});
			return patternElement;
		};

		class PropertyRecord {

			String reference;

			String name;

		}

		var removedProperties = new HashSet<PropertyRecord>();
		Function<Expression, Expression> removedPropertiesCollector = expression -> {
			var property = (Property) expression;
			property.accept(segment -> {
				if (segment instanceof SymbolicName) {
					var value = ((SymbolicName) segment).getValue();
					if (nodes.containsKey(value)) {
						var propertyRecord = new PropertyRecord();
						propertyRecord.reference = ((SymbolicName) segment).getValue();
						propertyRecord.name = property.getName();
						removedProperties.add(propertyRecord);
					}
				}
			});
			return expression;
		};

		var setProperties = new HashSet<PropertyRecord>();
		Function<Expression, Operation> setPropertiesCollector = expression -> {
			var op = (Operation) expression;
			var property = new AtomicReference<String>();
			var name = new AtomicReference<String>();
			op.accept(segment -> {
				if (segment instanceof SymbolicName) {
					// First symbolic name will be the property reference
					var value = ((SymbolicName) segment).getValue();
					if (!property.compareAndSet(null, value)) {
						// Second will be the name
						name.compareAndSet(null, value);
					}
				}
			});
			if (nodes.containsKey(property.get())) {
				var propertyRecord = new PropertyRecord();
				propertyRecord.reference = property.get();
				propertyRecord.name = name.get();
				setProperties.add(propertyRecord);
			}
			return op;
		};

		var options = Options.newOptions()
			.withCallback(PatternElementCreatedEventType.ON_CREATE, nodesCollector)
			.withCallback(PatternElementCreatedEventType.ON_MATCH, nodesCollector)
			.withCallback(ExpressionCreatedEventType.ON_REMOVE_PROPERTY, Expression.class, removedPropertiesCollector)
			.withCallback(ExpressionCreatedEventType.ON_SET_PROPERTY, Operation.class, setPropertiesCollector)
			.build();

		CypherParser.parse("MATCH (c:Person {uuid: $uuid})-[:WORKS_AT]->(p:Company) REMOVE c.name SET c.thing = 1",
				options);
		assertThat(removedProperties).hasSize(1).first().satisfies(p -> {
			assertThat(p.name).isEqualTo("name");
			assertThat(p.reference).isEqualTo("c");
		});
		assertThat(setProperties).hasSize(1).first().satisfies(p -> {
			assertThat(p.name).isEqualTo("thing");
			assertThat(p.reference).isEqualTo("c");
		});
		assertThat(nodes).containsKey("c").hasEntrySatisfying("c", n -> {
			var selectingProperty = new AtomicReference<String>();
			Visitor propExtractor = segment -> {
				if (segment instanceof KeyValueMapEntry) {
					var entry = (KeyValueMapEntry) segment;
					selectingProperty.compareAndSet(null, entry.getKey());
				}
			};
			n.accept(propExtractor);
			assertThat(selectingProperty).hasValue("uuid");
		});
	}

	@Test
	void ensureAReturnAlias() {

		// tag::example-required-alias[]
		var userProvidedCypher = "MATCH (this)-[:LINK]-(o:Other) RETURN o";

		Function<Expression, AliasedExpression> ensureAlias = r -> {
			if (!(r instanceof AliasedExpression)) {
				return r.as("result");
			}
			return (AliasedExpression) r;
		}; // <.>

		var options = Options.newOptions() // <.>
			.withCallback( // <.>
					ExpressionCreatedEventType.ON_RETURN_ITEM, AliasedExpression.class, ensureAlias)
			.build();

		var userStatement = CypherParser.parse(userProvidedCypher, options); // <.>
		// end::example-required-alias[]

		var node = Cypher.node("Node").named("node");
		var result = Cypher.name("result");
		var cypher = Cypher.match(node)
			.call(userStatement, node.as("this")) // <.>
			.returning(result.project("foo", "bar"))
			.build()
			.getCypher();

		assertThat(cypher).isEqualTo(
				"MATCH (node:`Node`) CALL (node) {WITH node AS this MATCH (this)-[:`LINK`]-(o:`Other`) RETURN o AS result} RETURN result{.foo, .bar}");
	}

	@Test
	void preventPropertyDeletion() {

		// tag::example-preventing-things[]
		var userProvidedCypher = "MATCH (this)-[:LINK]-(o:Other) REMOVE this.something RETURN o";

		UnaryOperator<Expression> preventPropertyDeletion = r -> {
			throw new RuntimeException("Not allowed to remove properties!"); // <.>
		};

		var options = Options.newOptions()
			.withCallback( // <.>
					ExpressionCreatedEventType.ON_REMOVE_PROPERTY, Expression.class, preventPropertyDeletion)
			.build();

		assertThatExceptionOfType(RuntimeException.class)
			.isThrownBy(() -> CypherParser.parse(userProvidedCypher, options)); // <.>
		// end::example-preventing-things[]
	}

	@Test
	void preventDeleteClause() {

		var userProvidedCypher = "MATCH (this)-[:LINK]-(o:Other) DELETE this, o";

		UnaryOperator<Expression> preventDelete = r -> {
			throw new RuntimeException("Not allowed to delete things!");
		};

		var options = Options.newOptions()
			.withCallback( // <.>
					ExpressionCreatedEventType.ON_DELETE_ITEM, Expression.class, preventDelete)
			.build();

		assertThatExceptionOfType(RuntimeException.class)
			.isThrownBy(() -> CypherParser.parse(userProvidedCypher, options))
			.havingCause()
			.withMessage("Not allowed to delete things!");
	}

	@Test
	void modifyReturnClause() {

		// tag::example-shape-the-return-clause[]
		var userProvidedCypher = "MATCH (this)-[:LINK]-(o:Other) RETURN distinct this, o LIMIT 23";

		Function<ReturnDefinition, Return> returnClauseFactory = d -> { // <.>
			var finalExpressionsReturned = d.getExpressions()
				.stream()
				.filter(e -> e instanceof SymbolicName && "o".equals(((SymbolicName) e).getValue()))
				.map(e -> e.as("result"))
				.collect(Collectors.<Expression>toList());

			return Clauses.returning(false, finalExpressionsReturned,
					List.of(Cypher.name("o").property("x").descending()), d.getOptionalSkip(), d.getOptionalLimit());
		};

		var options = Options.newOptions()
			.withReturnClauseFactory(returnClauseFactory) // <.>
			.build();

		var userStatement = CypherParser.parse(userProvidedCypher, options);
		var cypher = userStatement.getCypher();

		assertThat(cypher) // <.>
			.isEqualTo("MATCH (this)-[:`LINK`]-(o:`Other`) RETURN o AS result ORDER BY o.x DESC LIMIT 23");
		// end::example-shape-the-return-clause[]
	}

	@Test // GH-574
	void collectingPropertyReferencesShouldWork() {

		var query = "MATCH (m:Movie {title: 'The Matrix'}) <-[r:ACTED_IN] - (p:Person {born: 1964}) WHERE m.releaseYear IS NOT NULL AND p.name = 'Keanu Reeves' RETURN m";
		var collectingPropertyReferences = new ConditionExtractingMatchFactory() {
			@Override
			Match apply0(MatchDefinition matchDefinition) {
				var newConditions = Cypher.noCondition();
				for (Condition value : this.nodeConditions.values().stream().flatMap(Collection::stream).toList()) {
					newConditions = newConditions.and(value);
				}
				return (Match) Clauses.match(matchDefinition.optional(), List
					.of(Cypher.node("Movie").named("m").relationshipFrom(Cypher.node("Person").named("p"), "ACTED_IN")),
						Where.from(newConditions), matchDefinition.optionalHints());
			}
		};
		var options = Options.newOptions().withMatchClauseFactory(collectingPropertyReferences).build();

		var cypher = CypherParser.parse(query, options).getCypher();
		assertThat(cypher).isEqualTo(
				"MATCH (m:`Movie`)<-[:`ACTED_IN`]-(p:`Person`) WHERE (m.title = 'The Matrix' AND m.releaseYear IS NOT NULL AND p.born = 1964 AND p.name = 'Keanu Reeves') RETURN m");
	}

	@Test // GH-739
	void rewritingStringLiterals() {

		var query = "MATCH (m:Movie {title: 'The Matrix'}) <-[r:ACTED_IN] - (p:Person {born: 1964}) WHERE m.releaseYear IS NOT NULL AND p.name = 'Keanu Reeves' RETURN m";
		var options = Options.newOptions()
			.withCallback(ExpressionCreatedEventType.ON_NEW_LITERAL, Expression.class, l -> {
				if (l instanceof StringLiteral sl) {
					return Cypher.anonParameter(sl.getContent());
				}
				return l;
			})
			.build();
		var stmt = CypherParser.parse(query, options);
		assertThat(stmt.getCypher()).isEqualTo(
				"MATCH (m:`Movie` {title: $pcdsl01})<-[r:`ACTED_IN`]-(p:`Person` {born: 1964}) WHERE (m.releaseYear IS NOT NULL AND p.name = $pcdsl02) RETURN m");
		assertThat(stmt.getCatalog().getParameters()).containsValues("The Matrix", "Keanu Reeves");
	}

	@Test // GH-1060
	void buildingWhereClauseShouldWork() {

		var node = Cypher.node("Product").named("Product");
		var where = Where.from(node.property("product_id").eq(Cypher.literalOf("BG2")));
		assertThat(where).hasToString("Where{cypher=WHERE Product.product_id = 'BG2'}");

		var cypher = Renderer.getRenderer(Configuration.defaultConfig(), GeneralizedRenderer.class).render(where);
		assertThat(cypher).isEqualTo("WHERE Product.product_id = 'BG2'");
	}

}
