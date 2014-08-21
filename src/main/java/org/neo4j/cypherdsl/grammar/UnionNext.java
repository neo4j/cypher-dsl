package org.neo4j.cypherdsl.grammar;

import org.neo4j.cypherdsl.expression.PathExpression;

/**
 * This specifies what can come after a UNION clause
 */
public interface UnionNext
{
    Match match( PathExpression... expression );

    Match match( Iterable<PathExpression> expressions );

    UnionNext all();
}
