package org.neo4j.cypherdsl.query.clause;

/**
 * UNION clause
 */
public class UnionClause extends Clause
{
    private boolean all;

    @Override
    public void asString(StringBuilder builder)
    {
        String name = " UNION" + (all ? " ALL" : "");
        builder.append( name );
    }

    public void all() {
        this.all = true;
    }
}
