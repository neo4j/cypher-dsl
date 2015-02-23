package org.neo4j.cypherdsl.grammar;

import org.neo4j.cypherdsl.SetProperty;

/**
 * Represents the ON MATCH clause
 */
public interface OnMatch
{
    UpdateNext onMatch( SetProperty... propertyValues );

    UpdateNext onMatch( Iterable<SetProperty> propertyValues );
}
