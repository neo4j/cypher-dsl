package org.neo4j.cypherdsl;

import org.junit.Test;
import org.neo4j.cypherdsl.querydsl.Attribute;

import static org.neo4j.cypherdsl.CypherQuery.*;


public class ValueTest extends AbstractCypherTest {

    @Test
    public void valueEnum() {
        assertQueryEquals(match(node(identifier("n")).values(value(Attribute.Id, Attribute.Type)))
                        .returns(identifier("n")).toString(),
                "CYPHER 3.3 MATCH (n {Id:\"Type\"}) RETURN n");
    }
}
