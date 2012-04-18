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

import com.mysema.query.lucene.LuceneSerializer;
import com.mysema.query.types.*;

import javax.annotation.Nullable;
import org.neo4j.cypherdsl.CypherQuery;
import org.neo4j.cypherdsl.Return;
import org.neo4j.cypherdsl.query.*;
import org.neo4j.cypherdsl.query.Expression;

import java.util.Collections;
import org.neo4j.cypherdsl.query.Order;

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
    public static Identifier identifier( Path<?> entityPath )
    {
        return identifier(entityPath.toString());
    }

    public static Property property( Path<?> entityPath )
    {
        return identifier(entityPath.getRoot().toString()).property( entityPath.getMetadata().getExpression().toString() );
    }

    public static StringProperty string( Path<?> entityPath )
    {
        return identifier(entityPath.getRoot().toString()).string( entityPath.getMetadata().getExpression().toString() );
    }

    public static NumberProperty number( Path<?> entityPath )
    {
        return identifier(entityPath.getRoot().toString()).number( entityPath.getMetadata().getExpression().toString() );
    }

    // Start
    private static final LuceneSerializer luceneSerializer = new LuceneSerializer(true, true);

    public static StartExpression.StartNodesQuery query( Path<?> entity, String indexName, Predicate query )
    {
        return query(entity.toString(), indexName, query);
    }

    public static StartExpression.StartNodesQuery query( String name, String indexName, Predicate query )
    {
        return query( name, indexName, luceneSerializer.toQuery( query, null ).toString() );
    }

    public static StartExpression.StartNodes node( Path<?> entity, long... id )
    {
        return CypherQuery.node(entity.toString(), id);
    }

    public static StartExpression.StartNodes node( Path<?> entity, String parameter )
    {
        return CypherQuery.node( entity.toString(), parameter );
    }

    public static StartExpression.StartNodesLookup lookup( Path<?> entity, String indexName, Path<?> key, String value )
    {
        return CypherQuery.lookup(entity.toString(), indexName, key.getMetadata().getExpression().toString(), value);
    }

    public static StartExpression.StartNodesLookup lookup( Path<?> entity, String indexName, Identifier key, Expression value )
    {
        return CypherQuery.lookup( entity.toString(), indexName, key, value );
    }

    // Match
    public static QueryDSLMatchExpression.QueryDSLPath path()
    {
        return new QueryDSLMatchExpression.QueryDSLPath();
    }

    public static QueryDSLMatchExpression.QueryDSLPath path(String name)
    {
        QueryDSLMatchExpression.QueryDSLPath path = new QueryDSLMatchExpression.QueryDSLPath();
        path.pathName = name;
        return path;
    }

    /**
     * Use this to invoke the shortestPath function
     *
     * @param name
     * @return
     */
    public static QueryDSLMatchExpression.QueryDSLFunctionPath shortestPath( String name )
    {
        Query.checkNull( name, "Name" );

        QueryDSLMatchExpression.QueryDSLFunctionPath functionPath = new QueryDSLMatchExpression.QueryDSLFunctionPath();
        functionPath.function = "shortestPath";
        functionPath.pathName = name;
        return functionPath;
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
            query.whereExpression = predicate.accept( new Visitor<PredicateExpression, BooleanExpression>()
            {
                @Override
                public PredicateExpression visit( Constant<?> constant,
                                                                @Nullable BooleanExpression booleanExpression
                )
                {
                    return null;
                }

                @Override
                public PredicateExpression visit( FactoryExpression<?> factoryExpression,
                                                                @Nullable BooleanExpression booleanExpression
                )
                {
                    return null;
                }

                @Override
                public PredicateExpression visit( Operation<?> operation,
                                                                @Nullable BooleanExpression booleanExpression
                )
                {
                    String id = operation.getOperator().getId();
                    if ( id.equals( Ops.AND.getId() ))
                    {
                        return and( operation.getArg( 0 ).accept( this, null ), operation.getArg( 1 )
                            .accept( this, null ) );
                    } else if ( id.equals( Ops.OR.getId() ))
                    {
                        return or( operation.getArg( 0 ).accept( this, null ), operation.getArg( 1 )
                            .accept( this, null ) );
                    } else if ( id.equals( Ops.NOT.getId() ))
                    {
                        return not( operation.getArg( 0 ).accept( this, null ) );
                    } else if ( id.equals( Ops.EQ_PRIMITIVE.getId() ) || id.equals( Ops.EQ_OBJECT.getId() ))
                    {
                        return eq( arg( operation.getArg( 0 ) ), arg( operation.getArg( 1 ) ));
                    } else if ( id.equals( Ops.NE_PRIMITIVE.getId() ) || id.equals( Ops.NE_OBJECT.getId() ))
                    {
                        return ne( arg( operation.getArg( 0 ) ), arg( operation.getArg( 1 ) ));
                    } else if ( id.equals( Ops.GT.getId() ))
                    {
                        return gt( arg( operation.getArg( 0 ) ), arg( operation.getArg( 1 ) ));
                    } else if ( id.equals( Ops.LT.getId() ))
                    {
                        return lt( arg( operation.getArg( 0 ) ), arg( operation.getArg( 1 ) ));
                    } else if ( id.equals( Ops.GOE.getId() ))
                    {
                        return gte( arg( operation.getArg( 0 ) ), arg( operation.getArg( 1 ) ));
                    } else if ( id.equals( Ops.LOE.getId() ))
                    {
                        return lte( arg( operation.getArg( 0 ) ), arg( operation.getArg( 1 ) ));
                    } else if ( id.equals( Ops.EXISTS.getId() ))
                    {
                        return has( (Expression) arg( operation.getArg( 0 )));
                    } else if ( id.equals( Ops.IS_NULL.getId() ))
                    {
                        return isNull( (Expression) arg( operation.getArg( 0 )));
                    } else if ( id.equals( Ops.IS_NOT_NULL.getId() ))
                    {
                        return isNotNull( (Expression) arg( operation.getArg( 0 )));
                    } else
                        throw new IllegalArgumentException( "Unknown operator:"+ id +" in expression "+operation );
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
                
                public Object arg(com.mysema.query.types.Expression expression)
                {
                    if (expression instanceof Constant)
                        return ((Constant)expression).getConstant();
                    else if (expression instanceof ParamExpression)
                        return param( ( (ParamExpression) expression ).getName() );
                    else if (expression instanceof Path)
                    {
                        Path path = (Path) expression;
                        return identifier( path.getRoot()).string( path.getMetadata().getExpression().toString() );
                    }
                    else
                        throw new IllegalArgumentException("Unknown argument type:"+expression);
                }
            }, null );

            return this;
        }
    }
}
