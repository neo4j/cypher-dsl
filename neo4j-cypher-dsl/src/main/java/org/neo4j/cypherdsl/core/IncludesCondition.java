package org.neo4j.cypherdsl.core;

import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * An includes condition checking for the list property.
 *
 * @author AakashSorathiya
 */
public final class IncludesCondition implements Condition {

    private final Expression left;
    private final EscapeQuoteStringLiteral leftStringLiteral;
    private final EscapeQuoteStringLiteral middleStringLiteral;
    private final EscapeQuoteStringLiteral rightStringLiteral;
    private final Expression right;

    static IncludesCondition create(Expression lhs, Operator operator, Expression rhs) {
        Assertions.notNull(lhs, "Left expression must not be null.");
        Assertions.notNull(rhs, "Right expression must not be null.");

        String[] allLiterals = operator.getRepresentation().split("\\{0}|\\{1}");
        EscapeQuoteStringLiteral leftStringLiteral = new EscapeQuoteStringLiteral(allLiterals[0]);
        EscapeQuoteStringLiteral middleStringLiteral = new EscapeQuoteStringLiteral(allLiterals[1]);
        EscapeQuoteStringLiteral rightStringLiteral = new EscapeQuoteStringLiteral(allLiterals[2]);

        return new IncludesCondition(lhs, leftStringLiteral, middleStringLiteral, rightStringLiteral, rhs);
    }

    public IncludesCondition(Expression left, EscapeQuoteStringLiteral leftStringLiteral, EscapeQuoteStringLiteral middleStringLiteral, EscapeQuoteStringLiteral rightStringLiteral, Expression right) {
        this.left = left;
        this.leftStringLiteral = leftStringLiteral;
        this.middleStringLiteral = middleStringLiteral;
        this.rightStringLiteral = rightStringLiteral;
        this.right = right;
    }

    @Override
    public void accept(Visitor visitor) {
        visitor.enter(this);
        this.leftStringLiteral.accept(visitor);
        this.right.accept(visitor);
        this.middleStringLiteral.accept(visitor);
        this.left.accept(visitor);
        this.rightStringLiteral.accept(visitor);
        visitor.leave(this);
    }
}
