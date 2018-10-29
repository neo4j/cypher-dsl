package org.neo4j.cypherdsl;

import org.junit.Test;

public class CypherQueryTest3 extends AbstractCypherTest {

    @Test
    public void unwindLiteralCollection() {
        assertQueryEquals(CYPHER + "UNWIND [1,2] as x RETURN x", new CypherQuery() {{
            unwinds(collection(literal(1), literal(2)), identifier("x")).returns(identifier("x"));
        }}.toString());
    }

    @Test
    public void unwindParameter() {
        assertQueryEquals(CYPHER + "UNWIND {numbers} as x RETURN x", new CypherQuery() {{
            unwinds(param("numbers"), identifier("x")).returns(identifier("x"));
        }}.toString());
    }

    @Test
    public void unwindWithCreate() {
        assertQueryEquals(CYPHER + "UNWIND {pairs} as pair MATCH (`pair.x`),(`pair.y`) CREATE (`pair.x`)-[:edge]->(`pair.y`)", new CypherQuery() {{
            unwinds(param("pairs"), identifier("pair"))
                    .match(node(identifier("pair.x")), node(identifier("pair.y")))
                    .create(node(identifier("pair.x")).out("edge").node(identifier("pair.y")));
        }}.toString());
    }

    @Test
    public void unwindMatch() {
        assertQueryEquals(CYPHER + "MATCH (n)-[r*1..2]-(m) UNWIND r as item RETURN DISTINCT item", new CypherQuery() {{
            matches(node(identifier("n")).both().hops(1,2).as(identifier("r")).node(identifier("m")))
                    .unwind(identifier("r"), identifier("item"))
            .returns(distinct(identifier("item")));
        }}.toString());
    }

    @Test
    public void unwindWhere() {
        assertQueryEquals(CYPHER + "MATCH (n)-[r*1..2]-(m) WHERE n.id=\"1\" UNWIND r as item RETURN DISTINCT item", new CypherQuery() {{
            matches(node(identifier("n")).both().hops(1,2).as(identifier("r")).node(identifier("m")))
                    .where(identifier("n").property("id").eq(literal("1")))
                    .unwind(identifier("r"), identifier("item"))
                    .returns(distinct(identifier("item")));
        }}.toString());
    }
}
