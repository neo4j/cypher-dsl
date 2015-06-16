package org.neo4j.cypherdsl.grammar;

import org.neo4j.cypherdsl.expression.SetExpression;

/**
 * Represents the ON MATCH clause
 */
public interface OnMatch
{
    UpdateNext onMatch( SetExpression... setExpressions );

    UpdateNext onMatch( Iterable<SetExpression> setExpressions );
}
