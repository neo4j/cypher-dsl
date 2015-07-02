package org.neo4j.cypherdsl.query.clause;

import org.neo4j.cypherdsl.expression.SetExpression;

import java.util.ArrayList;

/**
 * ON CREATE clause
 */
public class OnCreateClause
        extends Clause
{
    private final ArrayList<SetExpression> expressions = new ArrayList<>();

    public OnCreateClause( Iterable<SetExpression> expressions )
    {
        for ( SetExpression expression : expressions )
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

