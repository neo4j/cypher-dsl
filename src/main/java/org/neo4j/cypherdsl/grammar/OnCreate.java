package org.neo4j.cypherdsl.grammar;

import org.neo4j.cypherdsl.SetProperty;

/**
 * Represents the ON CREATE clause
 */
public interface OnCreate
{
    UpdateNext onCreate( SetProperty... propertyValues );

    UpdateNext onCreate( Iterable<SetProperty> propertyValues );
}
