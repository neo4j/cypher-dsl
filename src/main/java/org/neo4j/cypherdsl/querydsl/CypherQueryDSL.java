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

import com.mysema.query.types.*;

import javax.annotation.Nullable;
import org.neo4j.cypherdsl.CypherQuery;
import org.neo4j.cypherdsl.Match;
import org.neo4j.cypherdsl.Return;
import org.neo4j.cypherdsl.query.*;
import org.neo4j.cypherdsl.query.Expression;

import java.util.Collections;

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
        Collections.addAll(query.startExpressions, startExpression);

        return new QueryDSLGrammar();
    }

    public QueryDSLMatch starts( Iterable<StartExpression> startExpression )
    {
        for( StartExpression expression : startExpression )
        {
            query.startExpressions.add( expression );
        }

        return new QueryDSLGrammar();
    }

    // Additional QueryDSL methods
    protected StartExpression.StartNodes node( Path<?> entity, long... id )
    {
        return StartExpression.node(entity.toString(), id);
    }

    protected StartExpression.StartNodes node( Path<?> entity, String parameter )
    {
        return StartExpression.node(entity.toString(), parameter);
    }

    protected StartExpression.StartNodesLookup lookup( Path<?> entity, String indexName, Path<?> key, String value )
    {
        return StartExpression.lookup(entity.toString(), indexName, key.getMetadata().getExpression().toString(), value);
    }

    protected StartExpression.StartNodesLookup lookup( Path<?> entity, String indexName, Expression.Identifier key, Expression.Value value )
    {
        return StartExpression.lookup(entity.toString(), indexName, key, value);
    }

    protected StartExpression.StartNodesQuery query( Path<?> entity, String indexName, Predicate query )
    {
        return query(entity.toString(), indexName, query);
    }

    protected StartExpression.StartNodesQuery query( String name, String indexName, Predicate query )
    {
        return QueryDSLStartExpression.query(name, indexName, query);
    }

    protected StartExpression.StartNodesQuery query( EntityPath<?> entity, String indexName, Predicate query )
    {
        return QueryDSLStartExpression.query(entity, indexName, query);
    }

    protected QueryDSLMatchExpression.QueryDSLPath path()
    {
        return QueryDSLMatchExpression.path();
    }

    protected QueryDSLMatchExpression.QueryDSLPath path(String name)
    {
        return QueryDSLMatchExpression.path(name);
    }

    /**
     * Use this to invoke the shortestPath function
     *
     * @param name
     * @return
     */
    protected QueryDSLMatchExpression.QueryDSLFunctionPath shortestPath( String name )
    {
        return QueryDSLMatchExpression.shortestPath(name);
    }

    protected OrderByExpression property( Path<?> path )
    {
        return OrderByExpression.property(path.toString());
    }

    protected OrderByExpression property( Path<?> path, OrderByExpression.Order order )
    {
        return OrderByExpression.property(path.toString(), order);
    }

    protected ReturnExpression.ReturnNodes nodes( Path<?>... entityPaths )
    {
        return QueryDSLReturnExpression.nodes(entityPaths);
    }

    protected ReturnExpression.ReturnProperties properties( Path<?>... paths )
    {
        return QueryDSLReturnExpression.properties(paths);
    }

    private class QueryDSLGrammar
        extends Grammar
        implements QueryDSLMatch, QueryDSLWhere
    {
        @Override
        public QueryDSLMatch match( MatchExpression... expression )
        {
            return (QueryDSLMatch) super.match( expression );
        }

        @Override
        public QueryDSLMatch match(Iterable<MatchExpression> expressions)
        {
            return (QueryDSLMatch) super.match(expressions);
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
                    String id = operation.getOperator().getId();
                    if ( id.equals( Ops.AND.getId() ))
                    {
                        return WhereExpression.and(operation.getArg(0).accept(this, null), operation.getArg(1)
                                .accept(this, null));
                    } else if ( id.equals( Ops.OR.getId() ))
                    {
                        return WhereExpression.or(operation.getArg(0).accept(this, null), operation.getArg(1)
                                .accept(this, null));
                    } else if ( id.equals( Ops.NOT.getId() ))
                    {
                        return WhereExpression.not((WhereExpression.PredicateExpression)  operation.getArg(0).accept(this, null));
                    } else if ( id.equals( Ops.EQ_PRIMITIVE.getId() ) || id.equals( Ops.EQ_OBJECT.getId() ))
                    {
                        return WhereExpression.eq(operation.getArg(0).toString(), arg(operation.getArg(1)));
                    } else if ( id.equals( Ops.NE_PRIMITIVE.getId() ) || id.equals( Ops.NE_OBJECT.getId() ))
                    {
                        return WhereExpression.ne(operation.getArg(0).toString(), arg(operation.getArg(1)));
                    } else if ( id.equals( Ops.GT.getId() ))
                    {
                        return WhereExpression.gt(operation.getArg(0).toString(), arg(operation.getArg(1)));
                    } else if ( id.equals( Ops.LT.getId() ))
                    {
                        return WhereExpression.lt(operation.getArg(0)
                                .toString(), ((Constant) operation.getArg(1)).getConstant());
                    } else if ( id.equals( Ops.GOE.getId() ))
                    {
                        return WhereExpression.gte( operation.getArg( 0 )
                                                       .toString(), arg(operation.getArg(1)) );
                    } else if ( id.equals( Ops.LOE.getId() ))
                    {
                        return WhereExpression.lte( operation.getArg( 0 )
                                                       .toString(), arg(operation.getArg(1)) );
                    } else if ( id.equals( Ops.EXISTS.getId() ))
                    {
                        return WhereExpression.exists( operation.getArg( 0 )
                                                       .toString() );
                    } else if ( id.equals( Ops.IS_NULL.getId() ))
                    {
                        return WhereExpression.isNull( operation.getArg( 0 )
                                                       .toString() );
                    } else if ( id.equals( Ops.IS_NOT_NULL.getId() ))
                    {
                        return WhereExpression.isNotNull( operation.getArg( 0 )
                                                       .toString() );
                    } else
                        throw new IllegalArgumentException( "Unknown operator:"+ id +" in expression "+operation );
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
                
                public Object arg(com.mysema.query.types.Expression expression)
                {
                    if (expression instanceof Constant)
                        return ((Constant)expression).getConstant();
                    else if (expression instanceof ParamExpression)
                        return Expression.param(((ParamExpression)expression).getName());
                    else
                        throw new IllegalArgumentException("Unknown argument type:"+expression);
                }
            }, null );

            return this;
        }
    }
}
