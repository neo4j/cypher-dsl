/**
 * Copyright (c) 2002-2012 "Neo Technology,"
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

import com.mysema.query.lucene.LuceneSerializer;
import com.mysema.query.types.Constant;
import com.mysema.query.types.FactoryExpression;
import com.mysema.query.types.Operation;
import com.mysema.query.types.Ops;
import com.mysema.query.types.ParamExpression;
import com.mysema.query.types.Path;
import com.mysema.query.types.Predicate;
import com.mysema.query.types.SubQueryExpression;
import com.mysema.query.types.TemplateExpression;
import com.mysema.query.types.Visitor;
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
        return identifier( entityPath.getMetadata().getExpression().toString() );
    }

    public static Property property( Path<?> entityPath )
    {
        return identifier( entityPath.getRoot().toString() ).property( entityPath.getMetadata().getExpression()
                .toString() );
    }

    public static StringExpression string( Path<?> entityPath )
    {
        return identifier( entityPath.getRoot().toString() ).string( entityPath.getMetadata().getExpression()
                .toString() );
    }

    public static NumericExpression number( Path<?> entityPath )
    {
        return identifier( entityPath.getRoot().toString() ).number( entityPath.getMetadata().getExpression()
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
                String id = operation.getOperator().getId();
                if ( id.equals( Ops.AND.getId() ) )
                {
                    return and( operation.getArg( 0 ).accept( this, null ), operation.getArg( 1 )
                            .accept( this, null ) );
                }
                else if ( id.equals( Ops.OR.getId() ) )
                {
                    return or( operation.getArg( 0 ).accept( this, null ), operation.getArg( 1 )
                            .accept( this, null ) );
                }
                else if ( id.equals( Ops.NOT.getId() ) )
                {
                    return not( operation.getArg( 0 ).accept( this, null ) );
                }
                else if ( id.equals( Ops.EQ_PRIMITIVE.getId() ) || id.equals( Ops.EQ_OBJECT.getId() ) )
                {
                    return arg( operation.getArg( 0 ) ).eq( (StringExpression) arg( operation.getArg( 1 ) ) );
                }
                else if ( id.equals( Ops.NE_PRIMITIVE.getId() ) || id.equals( Ops.NE_OBJECT.getId() ) )
                {
                    return arg( operation.getArg( 0 ) ).ne( (StringExpression) arg( operation.getArg( 1 ) ) );
                }
                else if ( id.equals( Ops.GT.getId() ) )
                {
                    return arg( operation.getArg( 0 ) ).gt( (StringExpression) arg( operation.getArg( 1 ) ) );
                }
                else if ( id.equals( Ops.LT.getId() ) )
                {
                    return arg( operation.getArg( 0 ) ).lt( (StringExpression) arg( operation.getArg( 1 ) ) );
                }
                else if ( id.equals( Ops.GOE.getId() ) )
                {
                    return arg( operation.getArg( 0 ) ).gte( (StringExpression) arg( operation.getArg( 1 ) ) );
                }
                else if ( id.equals( Ops.LOE.getId() ) )
                {
                    return arg( operation.getArg( 0 ) ).lte( (StringExpression) arg( operation.getArg( 1 ) ) );
                }
                else if ( id.equals( Ops.EXISTS.getId() ) )
                {
                    return has( (Property) arg( operation.getArg( 0 ) ) );
                }
                else if ( id.equals( Ops.IS_NULL.getId() ) )
                {
                    return isNull( (Expression) arg( operation.getArg( 0 ) ) );
                }
                else if ( id.equals( Ops.IS_NOT_NULL.getId() ) )
                {
                    return isNotNull( (Expression) arg( operation.getArg( 0 ) ) );
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

            public Value arg( com.mysema.query.types.Expression expression )
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
                    return new Value( identifier( path.getRoot() ).string( path.getMetadata().getExpression()
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
