package org.neo4j.cypherdsl.query.clause;

import org.neo4j.cypherdsl.expression.SetExpression;

import java.util.ArrayList;

/**
 * ON CREATE clause
 */
public class OnMatchClause
        extends Clause
{
    private final ArrayList<SetExpression> expressions = new ArrayList<>();

    public OnMatchClause( Iterable<SetExpression> expressions )
    {
        for ( SetExpression expression : expressions )
        {
            this.expressions.add( expression );
        }
    }

    @Override
    public void asString( StringBuilder builder )
    {
        String name = " ON MATCH";
        builder.append(name);
        clauseAsString( builder, "SET", expressions, "," );
    }
}
