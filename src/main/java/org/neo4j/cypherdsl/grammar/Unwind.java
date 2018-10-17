package org.neo4j.cypherdsl.grammar;

import org.neo4j.cypherdsl.Identifier;
import org.neo4j.cypherdsl.expression.Expression;

public interface Unwind {

    UpdateNext unwind(Expression list, Identifier identifier);
}
