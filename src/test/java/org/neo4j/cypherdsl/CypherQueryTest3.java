package org.neo4j.cypherdsl;

import org.junit.Test;

public class CypherQueryTest3 extends AbstractCypherTest {

    @Test
    public void unwindLiteralCollection() {
        assertQueryEquals(CYPHER + "UNWIND[1,2] as x RETURN x", new CypherQuery() {{
            unwinds(collection(literal(1), literal(2)), identifier("x")).returns(identifier("x"));
        }}.toString());
    }

    @Test
    public void unwindParameter() {
        assertQueryEquals(CYPHER + "UNWIND{numbers} as x RETURN x", new CypherQuery() {{
            unwinds(param("numbers"), identifier("x")).returns(identifier("x"));
        }}.toString());
    }

    @Test
    public void unwindWithCreate() {
        assertQueryEquals(CYPHER + "UNWIND{pairs} as pair MATCH (`pair.x`),(`pair.y`) CREATE (`pair.x`)-[:edge]->(`pair.y`)", new CypherQuery() {{
            unwinds(param("pairs"), identifier("pair"))
                    .match(node(identifier("pair.x")), node(identifier("pair.y")))
                    .create(node(identifier("pair.x")).out("edge").node(identifier("pair.y")));
        }}.toString());
    }
}
