/**
 * Licensed to Neo Technology under one or more contributor
 * license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright
 * ownership. Neo Technology licenses this file to you under
 * the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
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
                    return exists( (Expression) arg( operation.getArg( 0 ) ) );
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
