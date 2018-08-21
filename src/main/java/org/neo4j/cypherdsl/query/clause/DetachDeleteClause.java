package org.neo4j.cypherdsl.query.clause;

import org.neo4j.cypherdsl.expression.ReferenceExpression;

import java.util.ArrayList;
import java.util.List;

public class DetachDeleteClause extends Clause {
    private final List<ReferenceExpression> expressions = new ArrayList<>();

    public DetachDeleteClause(Iterable<ReferenceExpression> expressions) {
        for ( ReferenceExpression expression : expressions )
        {
            this.expressions.add( expression );
        }
    }

    public void asString(StringBuilder builder) {
        this.clauseAsString(builder, "DETACH DELETE", this.expressions, ",");
    }

}
