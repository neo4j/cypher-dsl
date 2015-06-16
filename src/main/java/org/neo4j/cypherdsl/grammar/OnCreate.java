package org.neo4j.cypherdsl.grammar;

import org.neo4j.cypherdsl.expression.SetExpression;

/**
 * Represents the ON CREATE clause
 */
public interface OnCreate
{
    UpdateNext onCreate( SetExpression... setExpressions );

    UpdateNext onCreate( Iterable<SetExpression> setExpressions );
}
