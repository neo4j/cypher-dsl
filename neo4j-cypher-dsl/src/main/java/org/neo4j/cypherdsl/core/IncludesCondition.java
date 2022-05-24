package org.neo4j.cypherdsl.core;

import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * A condition checking for the list property.
 *
 * @author AakashSorathiya
 */
public final class IncludesCondition implements Condition {

    private final Expression left;
    private final StringLiteral leftStringLiteral;
    private final StringLiteral middleStringLiteral;
    private final StringLiteral rightStringLiteral;
    private final Expression right;

    static IncludesCondition create(Expression lhs, Operator operator, Expression rhs) {
        Assertions.notNull(lhs, "Left expression must not be null.");
        Assertions.notNull(rhs, "Right expression must not be null.");

        String[] allLiterals = operator.getRepresentation().split("\\{0}|\\{1}");
        StringLiteral leftStringLiteral = new StringLiteral(allLiterals[0], true);
        StringLiteral middleStringLiteral = new StringLiteral(allLiterals[1], true);
        StringLiteral rightStringLiteral = new StringLiteral(allLiterals[2], true);

        return new IncludesCondition(lhs, leftStringLiteral, middleStringLiteral, rightStringLiteral, rhs);
    }

    public IncludesCondition(Expression left, StringLiteral leftStringLiteral, StringLiteral middleStringLiteral, StringLiteral rightStringLiteral, Expression right) {
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
