/*
 * Copyright (c) "Neo4j"
 * Neo4j Sweden AB [http://neo4j.com]
 *
 * This file is part of Neo4j.
 *
 * Neo4j is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.neo4j.cypherdsl.examples.parser;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatNoException;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Function;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;
import org.neo4j.cypherdsl.core.AliasedExpression;
import org.neo4j.cypherdsl.core.Clauses;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.Expression;
import org.neo4j.cypherdsl.core.Node;
import org.neo4j.cypherdsl.core.NodeLabel;
import org.neo4j.cypherdsl.core.Operation;
import org.neo4j.cypherdsl.core.PatternElement;
import org.neo4j.cypherdsl.core.Return;
import org.neo4j.cypherdsl.core.SymbolicName;
// tag::main-entry-point[]
import org.neo4j.cypherdsl.parser.CypherParser;
// end::main-entry-point[]
import org.neo4j.cypherdsl.parser.ExpressionCreatedEventType;
import org.neo4j.cypherdsl.parser.Options;
import org.neo4j.cypherdsl.parser.PatternElementCreatedEventType;
import org.neo4j.cypherdsl.parser.ReturnDefinition;

/**
 * @author Michael J. Simons
 */
class CypherDSLParserExamplesTest {

	@Test
	void createASubqueryCallWithUserProvidedCypher() {

		// tag::example-using-input[]
		var userProvidedCypher
			= "MATCH (this)-[:LINK]-(o:Other) RETURN o as result"; // <.>
		var userStatement = CypherParser.parse(userProvidedCypher); // <.>

		var node = Cypher.node("Node").named("node");
		var result = Cypher.name("result");
		var cypher = Cypher // <.>
			.match(node)
			.call(// <.>
				userStatement,
				node.as("this")
			)
			.returning(result.project("foo", "bar"))
			.build()
			.getCypher();

		assertThat(cypher).isEqualTo(
			"MATCH (node:`Node`) "
			+ "CALL {"
			+ "WITH node "
			+ "WITH node AS this " // <.>
			+ "MATCH (this)-[:`LINK`]-(o:`Other`) RETURN o AS result" // <.>
			+ "} "
			+ "RETURN result{.foo, .bar}");
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
					if(segment instanceof NodeLabel) {
						labelsSeen.add(((NodeLabel) segment).getValue());
					}
				});
				return op;
			}
		}

		var labelsAdded = new LabelCollector();
		var labelsRemoved = new LabelCollector();

		var options = Options.newOptions()
			.withCallback(ExpressionCreatedEventType.ON_SET_LABELS,  Operation.class, labelsAdded)
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
		var options = Options.newOptions()
			.withLabelFilter((labelParsedEventType, strings) -> {

				// The event type could be used to distinguish the call site at which labels where needed
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
			})
			.build();

		assertThatNoException().isThrownBy(() -> CypherParser.parse(cypher, options));
		assertThat(labelsSeen).containsExactlyInAnyOrder("Bazbar", "Foo", "Bar", "Person");
	}

	@Test // GH-299
	void extractLabelsInMatchOrCreate() {

		var cypher = "OPTIONAL MATCH (m:Bazbar) SET m:Foo REMOVE m:Bar WITH m CREATE (m:Person) SET n.name = 'John' RETURN n, m";

		class LabelCollector implements UnaryOperator<PatternElement> {

			Set<String> labelsSeen = new HashSet<>();

			@Override
			public PatternElement apply(PatternElement patternElement) {
				if(patternElement instanceof Node) {
					((Node) patternElement).getLabels().forEach(l -> labelsSeen.add(l.getValue()));
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

	@Test // GH-299
	void allLabelsSeen() {

		var cypher = "OPTIONAL MATCH (m:Bazbar) SET m:Foo REMOVE m:Bar WITH m CREATE (m:Person) SET n.name = 'John' RETURN n, m";

		Set<String> labelsSeen = new HashSet<>();

		class LabelsInsideExpressions implements Function<Expression, Operation> {

			@Override
			public Operation apply(Expression expression) {
				Operation op = (Operation) expression;
				op.accept(segment -> {
					if(segment instanceof NodeLabel) {
						labelsSeen.add(((NodeLabel) segment).getValue());
					}
				});
				return op;
			}
		}

		class LabelsInsidePatterns implements UnaryOperator<PatternElement> {

			@Override
			public PatternElement apply(PatternElement patternElement) {
				if(patternElement instanceof Node) {
					((Node) patternElement).getLabels().forEach(l -> labelsSeen.add(l.getValue()));
				}
				return patternElement;
			}
		}

		var labelsInsideExpressions = new LabelsInsideExpressions();
		var labelsInsidePatterns = new LabelsInsidePatterns();

		var options = Options.newOptions()
			.withCallback(ExpressionCreatedEventType.ON_SET_LABELS,  Operation.class, labelsInsideExpressions)
			.withCallback(ExpressionCreatedEventType.ON_REMOVE_LABELS, Operation.class, labelsInsideExpressions)
			.withCallback(PatternElementCreatedEventType.ON_CREATE, labelsInsidePatterns)
			.withCallback(PatternElementCreatedEventType.ON_MATCH, labelsInsidePatterns)
			.build();

		assertThatNoException().isThrownBy(() -> CypherParser.parse(cypher, options));
		assertThat(labelsSeen).containsExactlyInAnyOrder("Foo", "Bar", "Bazbar", "Person");
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
				ExpressionCreatedEventType.ON_RETURN_ITEM,
				AliasedExpression.class,
				ensureAlias
			)
			.build();

		var userStatement = CypherParser.parse(userProvidedCypher, options); // <.>
		// end::example-required-alias[]

		var node = Cypher.node("Node").named("node");
		var result = Cypher.name("result");
		var cypher = Cypher
			.match(node)
			.call(userStatement, node.as("this")) // <.>
			.returning(result.project("foo", "bar"))
			.build()
			.getCypher();

		assertThat(cypher).isEqualTo(
			"MATCH (node:`Node`) CALL {WITH node WITH node AS this MATCH (this)-[:`LINK`]-(o:`Other`) RETURN o AS result} RETURN result{.foo, .bar}");
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
				ExpressionCreatedEventType.ON_REMOVE_PROPERTY,
				Expression.class,
				preventPropertyDeletion
			)
			.build();

		assertThatExceptionOfType(RuntimeException.class)
			.isThrownBy(() -> CypherParser.parse(userProvidedCypher, options)); // <.>
		// end::example-preventing-things[]
	}

	@Test
	void modifyReturnClause() {

		// tag::example-shape-the-return-clause[]
		var userProvidedCypher = "MATCH (this)-[:LINK]-(o:Other) RETURN distinct this, o LIMIT 23";

		Function<ReturnDefinition, Return> returnClauseFactory = d -> { // <.>
			var finalExpressionsReturned = d.getExpressions().stream()
				.filter(e -> e instanceof SymbolicName && "o".equals(((SymbolicName) e).getValue()))
				.map(e -> e.as("result"))
				.collect(Collectors.<Expression>toList());

			return Clauses.returning(
				false,
				finalExpressionsReturned,
				List.of(Cypher.name("o").property("x").descending()),
				d.getOptionalSkip(), d.getOptionalLimit()
			);
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
}
