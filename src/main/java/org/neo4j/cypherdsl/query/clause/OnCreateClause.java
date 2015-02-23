package org.neo4j.cypherdsl.query.clause;

import org.neo4j.cypherdsl.SetProperty;

import java.util.ArrayList;

/**
 * ON CREATE clause
 */
public class OnCreateClause
        extends Clause
{
    private final ArrayList<SetProperty> expressions = new ArrayList<SetProperty>();

    public OnCreateClause( Iterable<SetProperty> expressions )
    {
        for ( SetProperty expression : expressions )
        {
            this.expressions.add( expression );
        }
    }

    @Override
    public void asString( StringBuilder builder )
    {
        String name = " ON CREATE";
        builder.append(name);
        clauseAsString( builder, "SET", expressions, "," );
    }
}

