/**
 * Copyright (c) 2002-2011 "Neo Technology,"
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
import javax.annotation.Nullable;
import org.neo4j.cypherdsl.CypherQuery;
import org.neo4j.cypherdsl.Return;
import org.neo4j.cypherdsl.query.MatchExpression;
import org.neo4j.cypherdsl.query.StartExpression;
import org.neo4j.cypherdsl.query.WhereExpression;

/**
 * TODO
 */
public class CypherQueryDSL
    extends CypherQuery
{
    public static QueryDSLMatch start( StartExpression... startExpression )
    {
        CypherQueryDSL query = new CypherQueryDSL();
        return query.starts( startExpression );
    }

    public QueryDSLMatch starts( StartExpression... startExpression )
    {
        for( StartExpression expression : startExpression )
        {
            query.startExpressions.add( expression );
        }

        return new QueryDSLGrammar();
    }

    private class QueryDSLGrammar
        extends Grammar
        implements QueryDSLMatch, QueryDSLWhere
    {
        @Override
        public QueryDSLWhere match( MatchExpression... expression )
        {
            return (QueryDSLWhere) super.match( expression );
        }

        @Override
        public Return where( Predicate predicate )
        {
            // Parse predicate
            query.whereExpression = predicate.accept( new Visitor<WhereExpression.BooleanExpression, WhereExpression.BooleanExpression>()
            {
                @Override
                public WhereExpression.BooleanExpression visit( Constant<?> constant,
                                                                @Nullable WhereExpression.BooleanExpression booleanExpression
                )
                {
                    return null;
                }

                @Override
                public WhereExpression.BooleanExpression visit( FactoryExpression<?> factoryExpression,
                                                                @Nullable WhereExpression.BooleanExpression booleanExpression
                )
                {
                    return null;
                }

                @Override
                public WhereExpression.BooleanExpression visit( Operation<?> operation,
                                                                @Nullable WhereExpression.BooleanExpression booleanExpression
                )
                {
                    if (operation.getOperator().getId().equals( Ops.AND.getId() ))
                    {
                        return WhereExpression.and( operation.getArg( 0 ).accept( this, null ), operation.getArg( 1 ).accept( this, null ) );
                    } else if (operation.getOperator().getId().equals( Ops.EQ_PRIMITIVE.getId() ))
                    {
                        return WhereExpression.eq( operation.getArg( 0 ).toString(), ((Constant)operation.getArg( 1 )).getConstant() );
                    } else if (operation.getOperator().getId().equals( Ops.GT.getId() ))
                    {
                        return WhereExpression.gt( operation.getArg( 0 ).toString(), ((Constant)operation.getArg( 1 )).getConstant() );
                    } else
                        throw new IllegalArgumentException( "Unknown operator:"+operation.getOperator().getId()+" in expression "+operation );
                }

                @Override
                public WhereExpression.BooleanExpression visit( ParamExpression<?> paramExpression,
                                                                @Nullable WhereExpression.BooleanExpression booleanExpression
                )
                {
                    return null;
                }

                @Override
                public WhereExpression.BooleanExpression visit( Path<?> path,
                                                                @Nullable WhereExpression.BooleanExpression booleanExpression
                )
                {
                    return null;
                }

                @Override
                public WhereExpression.BooleanExpression visit( SubQueryExpression<?> subQueryExpression,
                                                                @Nullable WhereExpression.BooleanExpression booleanExpression
                )
                {
                    return null;
                }

                @Override
                public WhereExpression.BooleanExpression visit( TemplateExpression<?> templateExpression,
                                                                @Nullable WhereExpression.BooleanExpression booleanExpression
                )
                {
                    return null;
                }
            }, null );

            return this;
        }
    }
}
