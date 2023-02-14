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
package org.neo4j.cypherdsl.parser.tmp_dont_merge_better_dont_commit;

import org.junit.jupiter.api.Test;
import org.neo4j.cypherdsl.core.AliasedExpression;
import org.neo4j.cypherdsl.core.Clauses;
import org.neo4j.cypherdsl.core.Condition;
import org.neo4j.cypherdsl.core.Conditions;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.Match;
import org.neo4j.cypherdsl.core.Node;
import org.neo4j.cypherdsl.core.PatternElement;
import org.neo4j.cypherdsl.core.SymbolicName;
import org.neo4j.cypherdsl.core.Where;
import org.neo4j.cypherdsl.parser.CypherParser;
import org.neo4j.cypherdsl.parser.MatchDefinition;
import org.neo4j.cypherdsl.parser.Options;
import org.neo4j.cypherdsl.parser.PatternElementCreatedEventType;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.UnaryOperator;

public class QueryParsingTest {

    @Test
    void test_query_parsing_multiple_ways() {

        var query = """
                MATCH (n:Person {name: $name})
                RETURN n, COUNT { (n)--() } AS degree
                """;

        extractMatchedProperties(query);

        var query2 = """
                MATCH (n:Person) WHERE n.name = $name
                RETURN n, COUNT { (n)--() } AS degree
                """;

        extractMatchedProperties(query2);

        var query3 = """
                MATCH (n) WHERE id(n) = $id
                MATCH (n)-[:CHECKED_IN_EVENT]->(e)
                WHERE e.date CONTAINS "-"
                WITH n, e, date(e.date) AS date
                WITH n, e ORDER BY date
                WITH n, head(collect(e)) AS event
                RETURN datetime(event.date + 'T23:59:59Z') AS lastSeenDate, $id AS id
                """;

        extractMatchedProperties(query3);


        var query4 = """
                MATCH (n) WHERE id(n) = $id
                WITH n
                MATCH (n)-[:CHECKED_IN_EVENT]->(e)
                WHERE e.date CONTAINS "-"
                WITH n, e, date(e.date) AS date
                WITH n, e ORDER BY date
                WITH n, head(collect(e)) AS event
                RETURN datetime(event.date + 'T23:59:59Z') AS lastSeenDate, $id AS id
                """;
        extractMatchedProperties(query4);


        var query5 = """
                MATCH (n) WHERE n.name = $name
                AND n.lastSeenDate > datetime() - duration({hours: 12})
                RETURN n
                """;
        extractMatchedProperties(query5);


        var query6 = """
                MATCH (n)-[r:FOLLOWS]->(v)
                WHERE r.id < 25
                MATCH (v)-[r2]->(x)
                WHERE r2.computed = false
                RETURN count(*) AS matches
                """;
        // exception due to r2 having no type defined
        extractMatchedProperties(query6);

        var query7 = """
                MATCH (n)-[r:FOLLOWS]->(v)
                MATCH (v)-[r2:TRANSACTS_WITH]->(x)
                WHERE r2.computed = false
                RETURN count(*) AS matches
                """;
        extractMatchedProperties(query7);

        var query8 = """
                MATCH (n:Person {name: "John Doe"})
                WHERE n:Active
                RETURN n
                """;
        extractMatchedProperties(query8);

        var query9 = """
                MATCH (n:Person)
                WHERE 12 > n.id
                RETURN n
                """;
        extractMatchedProperties(query9);

        var query10 = """
                MATCH (n:Event)
                WHERE point.distance($point, n.position) < 1000
                RETURN n
                """;
        extractMatchedProperties(query10);


        var query11 = """
                MATCH (n:Event)
                WHERE point.withinBox(n.position, $lowerLeft, $upperRight)
                RETURN n
                """;
        // withinBox not supported ?
        extractMatchedProperties(query11);

        var query12 = """
                UNWIND $names AS name
                MATCH (n:Person {name: name})
                RETURN n
                """;
        extractMatchedProperties(query12);


        // fails to get multiple relTypes for r.isActive ?
        var query13 = """
                MATCH (a:Officer),(b:Officer)
                WHERE a.name CONTAINS 'MR. ISAAC ELBAZ'\s
                AND b.name CONTAINS 'Stephanie J. Bridges'
                MATCH (a)-[r:OFFICER_OF|INTERMEDIARY_OF|REGISTERED_ADDRESS*..10]-(b)
                WHERE r.isActive = $activeFlag
                RETURN p
                LIMIT 50
                """;
        extractMatchedProperties(query13);


    }

    private void extractMatchedProperties(String query) {

        System.out.println("Extracting information from query \n" + query);

        var labelsCollector = new LabelCollector();
        var getPropertiesMatchedCollector = new ConditionExtractingMatchingFactory() {
            @Override
            Match apply0(MatchDefinition matchDefinition) {
                var newConditions = Conditions.noCondition();
                for (Condition value : nodeConditions.values().stream().flatMap(Collection::stream).toList()) {
                    newConditions = newConditions.and(value);
                }
                return (Match) Clauses.match(
                        matchDefinition.optional(),
                        List.of(Cypher.node("Movie").named("m").relationshipFrom(Cypher.node("Person").named("p"), "ACTED_IN")),
                        Where.from(newConditions),
                        matchDefinition.optionalHints()
                );
            }
        };

        var statement = CypherParser.parse(query, Options.newOptions()
                .withCallback(PatternElementCreatedEventType.ON_MATCH, labelsCollector)
                .withMatchClauseFactory(getPropertiesMatchedCollector)
                .build());

        System.out.println("labels matched " + labelsCollector.labelsSeen);

        var matchedProperties = getPropertiesMatchedCollector.matchedProperties;
        matchedProperties.forEach((k, v) -> {
            var label = k.getLabels().isEmpty() ? "no label" : k.getLabels().get(0).getValue();
            v.forEach(p -> {
                var propertyKey = p.getName();
                System.out.println("Matched property `%s` on label `%s`".formatted(propertyKey, label));
            });
        });
        var elements = statement.getIdentifiableExpressions().stream().map(e -> {
            if (e instanceof AliasedExpression) {
                return ((AliasedExpression) e).getAlias();
            }
            return ((SymbolicName) e).getValue();
        }).toList();
        System.out.println("returned elements : " + elements);

        getPropertiesMatchedCollector.matchedRelationshipProperties.forEach((k, v) -> {
            try {
                var label = k.getDetails().getTypes() == null ? "no type" : String.join(", ", k.getDetails().getTypes().get(0));
                v.forEach(p -> {
                    var propertyKey = p.getName();
                    System.out.println("Matched property `%s` on relationship `%s`".formatted(propertyKey, label));
                });
            } catch (Exception e) {
                System.out.println(e.getMessage());
            }
        });

        System.out.println(statement.getParameters().values());

        System.out.println("-----------------------------------------------");

        // get access now via conditions in matching factory
        getPropertiesMatchedCollector.nodeConditions.forEach((k, v) -> {
            // here it would be nice to actually have access to the node, property and the condition itself ( operator + right side )
            // right side ideally
        });
    }

    static class LabelCollector implements UnaryOperator<PatternElement> {

        Set<String> labelsSeen = new HashSet<>();

        @Override
        public PatternElement apply(PatternElement patternElement) {
            if (patternElement instanceof Node) {
                ((Node) patternElement).getLabels().forEach(l -> labelsSeen.add(l.getValue()));
            }
            return patternElement;
        }
    }
}