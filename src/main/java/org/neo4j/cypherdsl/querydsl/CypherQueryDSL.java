/**
 * Copyright (c) 2002-2015 "Neo Technology,"
 * Network Engine for Objects in Lund AB [http://neotechnology.com]
 *
 * This file is part of Neo4j.
 *
 * Neo4j is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.neo4j.cypherdsl.querydsl;

import javax.annotation.Nullable;

import com.querydsl.lucene3.LuceneSerializer;
import com.querydsl.core.types.Constant;
import com.querydsl.core.types.FactoryExpression;
import com.querydsl.core.types.Operation;
import com.querydsl.core.types.Ops;
import com.querydsl.core.types.ParamExpression;
import com.querydsl.core.types.Path;
import com.querydsl.core.types.Predicate;
import com.querydsl.core.types.SubQueryExpression;
import com.querydsl.core.types.TemplateExpression;
import com.querydsl.core.types.Visitor;
import org.neo4j.cypherdsl.CypherQuery;
import org.neo4j.cypherdsl.Identifier;
import org.neo4j.cypherdsl.Property;
import org.neo4j.cypherdsl.expression.BooleanExpression;
import org.neo4j.cypherdsl.expression.Expression;
import org.neo4j.cypherdsl.expression.NumericExpression;
import org.neo4j.cypherdsl.expression.StringExpression;
import org.neo4j.cypherdsl.query.Value;

/**
 * Methods here are used to integrate Cypher DSL with the QueryDSL library. Create QBeans with QueryDSL
 * and use those as parameters for these methods, which can then be used with the regular Cypher DSL methods.
 */
public class CypherQueryDSL
        extends CypherQuery
{
    private static final LuceneSerializer luceneSerializer = new LuceneSerializer( true, true );

    // Additional QueryDSL methods
    public static Identifier identifier( Path<?> entityPath )
    {
        return identifier( entityPath.getMetadata().getElement().toString() );
    }

    public static Property property( Path<?> entityPath )
    {
        return identifier( entityPath.getRoot().toString() ).property( entityPath.getMetadata().getElement()
                .toString() );
    }

    public static StringExpression string( Path<?> entityPath )
    {
        return identifier( entityPath.getRoot().toString() ).string( entityPath.getMetadata().getElement()
                .toString() );
    }

    public static NumericExpression number( Path<?> entityPath )
    {
        return identifier( entityPath.getRoot().toString() ).number( entityPath.getMetadata().getElement()
                .toString() );
    }

    public static String toQuery( Predicate query )
    {
        return luceneSerializer.toQuery( query, null ).toString();
    }

    public static BooleanExpression toBooleanExpression( Predicate predicate )
    {
        return predicate.accept( new Visitor<BooleanExpression, BooleanExpression>()
        {
            @Override
            public BooleanExpression visit( Constant<?> constant,
                                            @Nullable BooleanExpression booleanExpression
            )
            {
                return null;
            }

            @Override
            public BooleanExpression visit( FactoryExpression<?> factoryExpression,
                                            @Nullable BooleanExpression booleanExpression
            )
            {
                return null;
            }

            @Override
            public BooleanExpression visit( Operation<?> operation,
                                            @Nullable BooleanExpression booleanExpression
            )
            {
                String id = operation.getOperator().name();
                if ( id.equals( Ops.AND.name() ) )
                {
                    return and( operation.getArg( 0 ).accept( this, null ), operation.getArg( 1 )
                            .accept( this, null ) );
                }
                else if ( id.equals( Ops.OR.name() ) )
                {
                    return or( operation.getArg( 0 ).accept( this, null ), operation.getArg( 1 )
                            .accept( this, null ) );
                }
                else if ( id.equals( Ops.NOT.name() ) )
                {
                    return not( operation.getArg( 0 ).accept( this, null ) );
                }
                else if ( id.equals( Ops.EQ.name() ) )
                {
                    return arg( operation.getArg( 0 ) ).eq( (StringExpression) arg( operation.getArg( 1 ) ) );
                }
                else if ( id.equals( Ops.NE.name() ) )
                {
                    return arg( operation.getArg( 0 ) ).ne( (StringExpression) arg( operation.getArg( 1 ) ) );
                }
                else if ( id.equals( Ops.GT.name() ) )
                {
                    return arg( operation.getArg( 0 ) ).gt( (StringExpression) arg( operation.getArg( 1 ) ) );
                }
                else if ( id.equals( Ops.LT.name() ) )
                {
                    return arg( operation.getArg( 0 ) ).lt( (StringExpression) arg( operation.getArg( 1 ) ) );
                }
                else if ( id.equals( Ops.GOE.name() ) )
                {
                    return arg( operation.getArg( 0 ) ).gte( (StringExpression) arg( operation.getArg( 1 ) ) );
                }
                else if ( id.equals( Ops.LOE.name() ) )
                {
                    return arg( operation.getArg( 0 ) ).lte( (StringExpression) arg( operation.getArg( 1 ) ) );
                }
                else if ( id.equals( Ops.EXISTS.name() ) )
                {
                    return has( (Expression) arg( operation.getArg( 0 ) ) );
                }
                else if ( id.equals( Ops.IS_NULL.name() ) )
                {
                    return isNull( (Expression) arg( operation.getArg( 0 ) ) );
                }
                else if ( id.equals( Ops.IS_NOT_NULL.name() ) )
                {
                    return isNotNull( (Expression) arg( operation.getArg( 0 ) ) );
                }
                else if ( id.equals( Ops.LIKE.name() ) )
                {
                    return arg( operation.getArg( 0 ) ).regexp( arg( operation.getArg( 1 ) ) );
                }
                else
                {
                    throw new IllegalArgumentException( "Unknown operator:" + id + " in expression " + operation );
                }
            }

            @Override
            public BooleanExpression visit( ParamExpression<?> paramExpression,
                                            @Nullable BooleanExpression booleanExpression
            )
            {
                return null;
            }

            @Override
            public BooleanExpression visit( Path<?> path,
                                            @Nullable BooleanExpression booleanExpression
            )
            {
                return null;
            }

            @Override
            public BooleanExpression visit( SubQueryExpression<?> subQueryExpression,
                                            @Nullable BooleanExpression booleanExpression
            )
            {
                return null;
            }

            @Override
            public BooleanExpression visit( TemplateExpression<?> templateExpression,
                                            @Nullable BooleanExpression booleanExpression
            )
            {
                return null;
            }

            public Value arg( com.querydsl.core.types.Expression expression )
            {
                if ( expression instanceof Constant )
                {
                    return new Value( literal( ((Constant) expression).getConstant() ) );
                }
                else if ( expression instanceof ParamExpression )
                {
                    return new Value( param( ((ParamExpression) expression).getName() ) );
                }
                else if ( expression instanceof Path )
                {
                    Path path = (Path) expression;
                    return new Value( identifier( path.getRoot() ).string( path.getMetadata().getElement()
                            .toString() ) );
                }
                else
                {
                    throw new IllegalArgumentException( "Unknown argument type:" + expression );
                }
            }
        }, null );
    }
}
