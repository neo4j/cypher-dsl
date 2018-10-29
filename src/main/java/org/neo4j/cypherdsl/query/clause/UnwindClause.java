package org.neo4j.cypherdsl.query.clause;

import org.neo4j.cypherdsl.AsString;
import org.neo4j.cypherdsl.Identifier;
import org.neo4j.cypherdsl.expression.Expression;

public class UnwindClause extends Clause
        implements AsString {

    private final Identifier id;

    private final Expression list;

    public UnwindClause(Expression list, Identifier id) {
        this.id = id;
        this.list = list;
    }

    @Override
    public void asString(StringBuilder builder) {
        builder.append(" UNWIND ");
        list.asString(builder);
        builder.append(" as ");
        id.asString(builder);
    }
}
