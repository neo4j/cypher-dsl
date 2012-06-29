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
package org.neo4j.cypherdsl;

import static org.neo4j.cypherdsl.query.Query.checkEmpty;
import static org.neo4j.cypherdsl.query.Query.checkNull;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.neo4j.cypherdsl.expression.All;
import org.neo4j.cypherdsl.expression.BooleanExpression;
import org.neo4j.cypherdsl.expression.CollectionExpression;
import org.neo4j.cypherdsl.expression.Expression;
import org.neo4j.cypherdsl.expression.NumericExpression;
import org.neo4j.cypherdsl.expression.PathExpression;
import org.neo4j.cypherdsl.expression.PropertyContainerExpression;
import org.neo4j.cypherdsl.expression.ReferenceExpression;
import org.neo4j.cypherdsl.expression.RelationshipExpression;
import org.neo4j.cypherdsl.expression.ScalarExpression;
import org.neo4j.cypherdsl.expression.StartExpression;
import org.neo4j.cypherdsl.expression.StringExpression;
import org.neo4j.cypherdsl.grammar.Create;
import org.neo4j.cypherdsl.grammar.Delete;
import org.neo4j.cypherdsl.grammar.Execute;
import org.neo4j.cypherdsl.grammar.ExecuteWithParameters;
import org.neo4j.cypherdsl.grammar.ForEachStatement;
import org.neo4j.cypherdsl.grammar.ForEachStatements;
import org.neo4j.cypherdsl.grammar.Limit;
import org.neo4j.cypherdsl.grammar.Match;
import org.neo4j.cypherdsl.grammar.OrderBy;
import org.neo4j.cypherdsl.grammar.Relate;
import org.neo4j.cypherdsl.grammar.ReturnNext;
import org.neo4j.cypherdsl.grammar.Set;
import org.neo4j.cypherdsl.grammar.Skip;
import org.neo4j.cypherdsl.grammar.StartNext;
import org.neo4j.cypherdsl.grammar.UpdateNext;
import org.neo4j.cypherdsl.grammar.Where;
import org.neo4j.cypherdsl.grammar.With;
import org.neo4j.cypherdsl.grammar.WithNext;
import org.neo4j.cypherdsl.query.AbstractExpression;
import org.neo4j.cypherdsl.query.ExpressionCollection;
import org.neo4j.cypherdsl.query.Expressions;
import org.neo4j.cypherdsl.query.Extract;
import org.neo4j.cypherdsl.query.Filter;
import org.neo4j.cypherdsl.query.FunctionExpression;
import org.neo4j.cypherdsl.query.IterablePredicateExpression;
import org.neo4j.cypherdsl.query.NamedPath;
import org.neo4j.cypherdsl.query.Operator;
import org.neo4j.cypherdsl.query.OrderByExpression;
import org.neo4j.cypherdsl.query.PropertyValue;
import org.neo4j.cypherdsl.query.Query;
import org.neo4j.cypherdsl.query.SuffixFunctionExpression;
import org.neo4j.cypherdsl.query.Value;
import org.neo4j.cypherdsl.query.clause.CreateClause;
import org.neo4j.cypherdsl.query.clause.DeleteClause;
import org.neo4j.cypherdsl.query.clause.ForEachClause;
import org.neo4j.cypherdsl.query.clause.LimitClause;
import org.neo4j.cypherdsl.query.clause.MatchClause;
import org.neo4j.cypherdsl.query.clause.OrderByClause;
import org.neo4j.cypherdsl.query.clause.RelateClause;
import org.neo4j.cypherdsl.query.clause.ReturnClause;
import org.neo4j.cypherdsl.query.clause.SetClause;
import org.neo4j.cypherdsl.query.clause.SkipClause;
import org.neo4j.cypherdsl.query.clause.StartClause;
import org.neo4j.cypherdsl.query.clause.WhereClause;
import org.neo4j.cypherdsl.query.clause.WithClause;

/**
 * DSL for creating Cypher queries. Once created you can serialize to a string,
 * or retrieve the internal Query model for further processing.
 * <p/>
 * It is possible to iteratively construct Cypher queries by calling toQuery()
 * and then use the Query as continuation point. When a new CypherQuery is created
 * by using the newQuery() method the Query is cloned, so that the original Query
 * is not modified. This can be used effectively to create a base query which can then
 * be used many times. Typical examples would be to create a query which is reused many times
 * for paging purposes, and also to provide a base query using START and MATCH which is then
 * expanded using WHERE and RETURN clauses.
 */
public class CypherQuery
{
    /**
     * Start building a new Cypher query, starting with a START clause
     *
     * @param startExpressions list of start expressions
     * @return Grammar for Match clause
     */
    public static StartNext start( StartExpression... startExpressions )
    {
        CypherQuery query = new CypherQuery();
        return query.starts( startExpressions );
    }

    /**
     * Start building a new Cypher query, starting with a CREATE clause
     *
     * @param paths
     * @return
     */
    public static UpdateNext create( PathExpression... paths )
    {
        CypherQuery query = new CypherQuery();
        return query.creates( paths );
    }

    /**
     * Continue building on existing Query object
     *
     * @param query a previously created query object
     * @return CypherQuery DSL that can be used to continue building the query
     */
    public static CypherQuery newQuery( Query query )
    {
        return new CypherQuery( query );
    }

    // The internal query object. Methods in the DSL work on this
    protected Query query;

    /**
     * Use this constructor if you want to use the instantiation block style
     * of using the DSL.
     * <p/>
     * Example:
     * <pre>
     *     new CypherQuery()
     *     {{
     *         starts(node("n",1)).returns(identifier("n"));
     *     }}.toString()
     * </pre>
     */
    public CypherQuery()
    {
        query = new Query();
    }

    private CypherQuery( Query query )
    {
        try
        {
            this.query = (Query) query.clone();
        }
        catch ( CloneNotSupportedException e )
        {
            throw new IllegalStateException( "Query was not cloneable" );
        }
    }

    // Common -------------------------------------------------------

    /**
     * Declare a Cypher query parameter.
     * This will be replaced with {name}.
     *
     * @param name of the parameter
     * @return Parameter instance
     */
    public static Parameter param( String name )
    {
        checkEmpty( name, "Name" );
        return new Parameter( name );
    }

    /**
     * Declare a literal string value, such as "Foo".
     *
     * @param value literal value
     * @return Literal instance
     */
    public static StringExpression literal( String value )
    {
        checkNull( value, "Value" );
        return new Literal( value );
    }

    /**
     * Declare a literal numeric value, such 3 or 4.5.
     *
     * @param value literal value
     * @return Literal instance
     */
    public static NumericExpression literal( Number value )
    {
        checkNull( value, "Value" );
        return new Literal( value );
    }

    /**
     * Declare a literal boolean value, such as true or false.
     *
     * @param value literal value
     * @return Literal instance
     */
    public static BooleanExpression literal( boolean value )
    {
        return new Literal( value );
    }

    /**
     * Declare a literal value using an untyped object.
     * <p/>
     * If a string is passed in, then output will
     * be quoted appropriately.
     *
     * @param value literal value
     * @return Literal instance
     */
    public static ScalarExpression literal( Object value )
    {
        checkNull( value, "Value" );
        return new Literal( value );
    }

    /**
     * Declare an identifier. This is used to
     * refer to names declared elsewhere in the query.
     * <p/>
     * If you want to refer to properties, then create
     * this first, and then call e.g. id.property("propname")
     *
     * @param name
     * @return
     */
    public static Identifier identifier( String name )
    {
        checkEmpty( name, "Identifier" );
        return new Identifier( name );
    }

    /**
     * Declare a collection of expressions. Values may be Expressions or literal values
     * that are converted to Literal expressions by this method.
     * <p/>
     * Corresponds to:
     * <pre>
     * [value1,value2,value3]
     * </pre>
     *
     * @param values
     * @return
     */
    public static CollectionExpression collection( Object... values )
    {
        Expression[] expressions = new Expression[values.length];
        for ( int i = 0; i < values.length; i++ )
        {
            Object value = values[i];
            expressions[i] = value instanceof Expression ? (Expression) value : literal( value );
        }
        return new Value( new ExpressionCollection( new Expressions( expressions ) ) );
    }

    /**
     * Declare a list of identifiers.
     *
     * @param values
     * @return
     */
    public static Identifier[] identifiers( String... values )
    {
        Identifier[] identifiers = new Identifier[values.length];
        for ( int i = 0; i < values.length; i++ )
        {
            String value = values[i];
            identifiers[i] = identifier( value );
        }
        return identifiers;
    }

    /**
     * Declare a list of parameters.
     *
     * @param names
     * @return
     */
    public static Parameter[] parameters( String... names )
    {
        Parameter[] parameters = new Parameter[names.length];
        for ( int i = 0; i < names.length; i++ )
        {
            String value = names[i];
            parameters[i] = param( value );
        }
        return parameters;
    }

    /**
     * Declare a list of literals using longs. This can be handy
     * for the nodesById method.
     *
     * @param values
     * @return
     */
    public static NumericExpression[] literals( long... values )
    {
        NumericExpression[] literals = new NumericExpression[values.length];
        for ( int i = 0; i < values.length; i++ )
        {
            long value = values[i];
            literals[i] = literal( value );
        }
        return literals;
    }

    /**
     * Declare a value, which can be used for setting or matching
     * properties in the CREATE or RELATE clauses.
     *
     * @param id
     * @param value
     * @return
     */
    public static PropertyValue value( String id, Object value )
    {
        return new PropertyValue( identifier( id ), literal( value ) );
    }

    /**
     * Declare a value, which can be used for setting or matching
     * properties in the CREATE or RELATE clauses.
     *
     * @param id
     * @param value
     * @return
     */
    public static PropertyValue value( String id, Expression value )
    {
        return new PropertyValue( identifier( id ), value );
    }

    /**
     * Declare a value, which can be used for setting or matching
     * properties in the CREATE or RELATE clauses.
     *
     * @param id
     * @param value
     * @return
     */
    public static PropertyValue value( Identifier id, Expression value )
    {
        return new PropertyValue( id, value );
    }

    /**
     * "and" a series of expressions together.
     *
     * @param expressions
     * @return
     */
    public static BooleanExpression and( BooleanExpression... expressions )
    {
        Query.checkNull( expressions, "Expressions" );
        return new And( expressions );
    }

    /**
     * "or" a series of expressions together.
     *
     * @param expressions
     * @return
     */
    public static BooleanExpression or( BooleanExpression... expressions )
    {
        Query.checkNull( expressions, "Expressions" );
        return new Or( expressions );
    }

    /**
     * Invert the boolean value of a predicate.
     * <p/>
     * Corresponds to:
     * <pre>
     * not(expression)
     * </pre>
     *
     * @param expression
     * @return
     */
    public static BooleanExpression not( BooleanExpression expression )
    {
        Query.checkNull( expression, "Expression" );

        return new Value( new FunctionExpression( "not", expression ) );
    }

    /**
     * Corresponds to:
     * <pre>
     * has(property)
     * </pre>
     *
     * @param property
     * @return
     */
    public static BooleanExpression has( Property property )
    {
        return new Value( new FunctionExpression( "has", property ) );
    }

    /**
     * Corresponds to:
     * <pre>
     * expression is null
     * </pre>
     *
     * @param expression
     * @return
     */
    public static BooleanExpression isNull( Expression expression )
    {
        return new Value( new SuffixFunctionExpression( " is null", expression ) );
    }

    /**
     * Corresponds to:
     * <pre>
     * expression is not null
     * </pre>
     *
     * @param expression
     * @return
     */
    public static BooleanExpression isNotNull( Expression expression )
    {
        return new Value( new SuffixFunctionExpression( " is not null", expression ) );
    }

    // Start --------------------------------------------------------

    /**
     * START clause. Use this with Java initialization block style.
     *
     * @param startExpressions
     * @return
     */
    protected StartNext starts( StartExpression... startExpressions )
    {
        query.add( new StartClause( Arrays.asList( startExpressions ) ) );

        return new Grammar();
    }

    /**
     * START clause. Use this with Java initialization block style.
     *
     * @param startExpressions
     * @return
     */
    protected StartNext starts( Iterable<StartExpression> startExpressions )
    {
        query.add( new StartClause( startExpressions ) );

        return new Grammar();
    }

    /**
     * CREATE clause. Use this with Java initialization block style.
     *
     * @param paths
     * @return
     */
    protected UpdateNext creates( PathExpression... paths )
    {
        query.add( new CreateClause( Arrays.asList( paths ) ) );

        return new Grammar();
    }

    /**
     * Declare start nodes. Corresponds to:
     * <pre>
     * name=node(id1,id2,id3)
     * </pre>
     *
     * @param name
     * @param id
     * @return
     */
    public static StartExpression.StartNodes nodesById( String name, long... id )
    {
        return nodesById( identifier( name ), id );
    }

    /**
     * Declare start nodes. Corresponds to:
     * <pre>
     * name=node(id1,id2,id3)
     * </pre>
     *
     * @param name
     * @param id
     * @return
     */
    public static StartExpression.StartNodes nodesById( Identifier name, long... id )
    {
        checkNull( name, "Name" );

        for ( long i : id )
        {
            if ( i < 0 )
            {
                throw new IllegalArgumentException( "Id may not be below zero" );
            }
        }

        StartExpression.StartNodes startNodes = new StartExpression.StartNodes();
        startNodes.name = name;
        startNodes.nodes = literals( id );
        return startNodes;
    }

    /**
     * Declare start nodes. Corresponds to:
     * <pre>
     * name=node({parameter})
     * </pre>
     *
     * @param name
     * @param parameter
     * @return
     */
    public static StartExpression.StartNodes nodeByParameter( String name, String parameter )
    {
        return nodeByparameter( identifier( name ), parameter );
    }

    /**
     * Declare start nodes. Corresponds to:
     * <pre>
     * name=node({parameter})
     * </pre>
     *
     * @param name
     * @param parameter
     * @return
     */
    public static StartExpression.StartNodes nodeByparameter( Identifier name, String parameter )
    {
        checkEmpty( name, "Name" );
        checkEmpty( parameter, "Parameters" );

        StartExpression.StartNodes startNodes = new StartExpression.StartNodes();
        startNodes.name = name;
        startNodes.nodes = parameters( parameter );
        return startNodes;
    }

    /**
     * Declare start nodes. Corresponds to:
     * <pre>
     * name=node(*)
     * </pre>
     *
     * @param name
     * @return
     */
    public static StartExpression.StartNodes allNodes( String name )
    {
        return allNodes( identifier( name ) );
    }

    /**
     * Declare start nodes. Corresponds to:
     * <pre>
     * name=node(*)
     * </pre>
     *
     * @param name
     * @return
     */
    public static StartExpression.StartNodes allNodes( Identifier name )
    {
        checkNull( name, "Name" );

        StartExpression.StartNodes startNodes = new StartExpression.StartNodes();
        startNodes.name = name;
        startNodes.nodes = new Expression[]{new StartExpression.AllNodes()};
        return startNodes;
    }

    /**
     * Declare start nodes. Corresponds to:
     * <pre>
     * name=node:indexName(key="value")
     * </pre>
     *
     * @param name
     * @param indexName
     * @param key
     * @param value
     * @return
     */
    public static StartExpression.StartNodesLookup lookup( String name, String indexName, String key, String value )
    {
        return lookup( identifier( name ), identifier( indexName ), identifier( key ), literal( value ) );
    }

    /**
     * Declare start nodes. Corresponds to:
     * <pre>
     * name=node:indexName(key="value")
     * </pre>
     *
     * @param name
     * @param indexName
     * @param key
     * @param value
     * @return
     */
    public static StartExpression.StartNodesLookup lookup( Identifier name, Identifier indexName,
                                                           ReferenceExpression key, StringExpression value )
    {
        checkEmpty( name, "Name" );
        checkEmpty( indexName, "Index" );

        StartExpression.StartNodesLookup startNodesLookup = new StartExpression.StartNodesLookup();
        startNodesLookup.name = name;
        startNodesLookup.index = indexName;
        startNodesLookup.key = key;
        startNodesLookup.value = value;
        return startNodesLookup;
    }

    /**
     * Declare start nodes. Corresponds to:
     * <pre>
     * name=node:indexName("query")
     * </pre>
     *
     * @param name
     * @param indexName
     * @param query
     * @return
     */
    public static StartExpression.StartNodesQuery query( String name, String indexName, String query )
    {
        return query( identifier( name ), identifier( indexName ), query );
    }

    /**
     * Declare start nodes. Corresponds to:
     * <pre>
     * name=node:indexName("query")
     * </pre>
     *
     * @param name
     * @param indexName
     * @param query
     * @return
     */
    public static StartExpression.StartNodesQuery query( Identifier name, Identifier indexName, String query )
    {
        checkNull( name, "Name" );
        checkNull( indexName, "Index" );
        checkEmpty( query, "Query" );

        StartExpression.StartNodesQuery startNodesQuery = new StartExpression.StartNodesQuery();
        startNodesQuery.name = name;
        startNodesQuery.index = indexName;
        startNodesQuery.query = query;
        return startNodesQuery;
    }

    /**
     * Declare start relationships. Corresponds to:
     * <pre>
     * name=relationship(id1,id2,id3)
     * </pre>
     *
     * @param name
     * @param id
     * @return
     */
    public static StartExpression.StartRelationships relationshipsById( String name, long... id )
    {
        return relationshipsById( identifier( name ), id );
    }

    /**
     * Declare start relationships. Corresponds to:
     * <pre>
     * name=relationship(id1,id2,id3)
     * </pre>
     *
     * @param name
     * @param id
     * @return
     */
    public static StartExpression.StartRelationships relationshipsById( Identifier name, long... id )
    {
        checkNull( name, "Name" );

        for ( long i : id )
        {
            if ( i < 0 )
            {
                throw new IllegalArgumentException( "Id may not be below zero" );
            }
        }

        StartExpression.StartRelationships startRelationships = new StartExpression.StartRelationships();
        startRelationships.name = name;
        startRelationships.relationships = literals( id );
        return startRelationships;
    }

    /**
     * Declare start relationships. Corresponds to:
     * <pre>
     * name=relationship({parameter})
     * </pre>
     *
     * @param name
     * @param parameter
     * @return
     */
    public static StartExpression.StartRelationshipsParameters relationshipsByParameter( String name, String parameter )
    {
        return relationshipsByParameter( identifier( name ), parameter );
    }

    /**
     * Declare start relationships. Corresponds to:
     * <pre>
     * name=relationship({parameter})
     * </pre>
     *
     * @param name
     * @param parameter
     * @return
     */
    public static StartExpression.StartRelationshipsParameters relationshipsByParameter( Identifier name,
                                                                                         String parameter
    )
    {
        checkNull( name, "Name" );
        checkEmpty( parameter, "Parameter" );

        StartExpression.StartRelationshipsParameters startRelationships = new StartExpression
                .StartRelationshipsParameters();
        startRelationships.name = name;
        startRelationships.parameter = parameter;
        return startRelationships;
    }

    /**
     * Declare start relationships. Corresponds to:
     * <pre>
     * name=relationship:indexName(key="value")
     * </pre>
     *
     * @param name
     * @param indexName
     * @param key
     * @param value
     * @return
     */
    public static StartExpression.StartRelationshipsIndex relationshipLookup( String name, String indexName,
                                                                              String key, String value )
    {
        return relationshipLookup( identifier( name ), identifier( indexName ), identifier( key ), literal( value ) );
    }

    /**
     * Declare start relationships. Corresponds to:
     * <pre>
     * name=relationship:indexName(key="value")
     * </pre>
     *
     * @param name
     * @param indexName
     * @param key
     * @param value
     * @return
     */
    public static StartExpression.StartRelationshipsIndex relationshipLookup( Identifier name, Identifier indexName,
                                                                              Identifier key, StringExpression value )
    {
        checkNull( name, "Name" );
        checkNull( indexName, "Index" );
        checkNull( key, "Key" );
        checkNull( value, "Value" );

        StartExpression.StartRelationshipsIndex startRelationshipsIndex = new StartExpression.StartRelationshipsIndex();
        startRelationshipsIndex.name = name;
        startRelationshipsIndex.index = indexName;
        startRelationshipsIndex.key = key;
        startRelationshipsIndex.value = value;
        return startRelationshipsIndex;
    }

    // Match --------------------------------------------------------

    /**
     * Start declaring a path for CREATE, RELATE, MATCH or WHERE clauses.
     * <p/>
     * Corresponds to:
     * <pre>
     * ()
     * </pre>
     *
     * @return
     */
    public static Path node()
    {
        return new Path( null, null );
    }

    /**
     * Start declaring a path for CREATE, RELATE, MATCH or WHERE clauses.
     * <p/>
     * Corresponds to:
     * <pre>
     * (id)
     * </pre>
     *
     * @param id
     * @return
     */
    public static Path node( String id )
    {
        return node( identifier( id ) );
    }

    /**
     * Start declaring a path for CREATE, RELATE, MATCH or WHERE clauses.
     * <p/>
     * Corresponds to:
     * <pre>
     * (expression)
     * </pre>
     *
     * @param expression
     * @return
     */
    public static Path node( Expression expression )
    {
        return new Path( expression, null );
    }

    /**
     * Declare a named path for MATCH clauses
     * <p/>
     * Corresponds to:
     * <pre>
     * name=path
     * </pre>
     *
     * @param name
     * @return
     */
    public static PathExpression path( String name, PathExpression path )
    {
        return path( identifier( name ), path );
    }

    /**
     * Declare a named path for MATCH clauses
     * <p/>
     * Corresponds to:
     * <pre>
     * name=path
     * </pre>
     *
     * @param name
     * @return
     */
    public static PathExpression path( Identifier name, PathExpression path )
    {
        checkNull( name, "Name" );
        return new NamedPath( name, path );
    }

    /**
     * Use this to declare a shortestPath.
     * <p/>
     * Corresponds to:
     * <pre>
     * shortestPath(path)
     * </pre>
     *
     * @param path
     * @return
     */
    public static PathExpression shortestPath( PathExpression path )
    {
        Query.checkNull( path, "Path" );
        return new Value( new FunctionExpression( "shortestPath", path ) );
    }

    /**
     * Use this to declare a allShortestPaths
     * <p/>
     * Corresponds to:
     * <pre>
     * allShortestPaths(path)
     * </pre>
     *
     * @param path
     * @return
     */
    public static PathExpression allShortestPaths( PathExpression path )
    {
        Query.checkNull( path, "Path" );

        return new Value( new FunctionExpression( "allShortestPaths", path ) );
    }

    // Return -------------------------------------------------------

    /**
     * Use this to rename identifiers for RETURN or WITH
     * <p/>
     * Corresponds to:
     * <pre>
     * expression AS name
     * </pre>
     *
     * @param expression
     * @param name
     * @return
     */
    public static Expression as( Expression expression, String name )
    {
        return new Value( new Operator( expression, " AS " ), identifier( name ) );
    }

    /**
     * Use this to rename identifiers for RETURN or WITH
     * <p/>
     * Corresponds to:
     * <pre>
     * expression AS name
     * </pre>
     *
     * @param expression
     * @param name
     * @return
     */
    public static Expression as( Expression expression, Identifier name )
    {
        return new Value( new Operator( expression, " AS " ), name );
    }

    /**
     * Use this to declare DISTINCT
     * <p/>
     * Corresponds to:
     * <pre>
     * DISTINCT expression
     * </pre>
     *
     * @param expression
     * @return
     */
    public static Expression distinct( Expression expression )
    {
        return new Value( new Operator( "DISTINCT " ), expression );
    }

    /**
     * Declare a count(*) RETURN expression
     *
     * @return
     */
    public static NumericExpression count()
    {
        return new Value( new FunctionExpression( "count", new AbstractExpression()
        {
            @Override
            public void asString( StringBuilder builder )
            {
                builder.append( '*' );
            }
        } ) );
    }

    /**
     * Declare a count(expression) RETURN expression
     *
     * @return
     */
    public static NumericExpression count( Expression expression )
    {
        checkNull( expression, "Expression" );

        return new Value( new FunctionExpression( "count", expression ) );
    }

    /**
     * Declare a * RETURN expression
     */
    public static All all()
    {
        return new All();
    }

    /**
     * Declare a sum(expression) RETURN expression
     *
     * @return
     */
    public static NumericExpression sum( NumericExpression expression )
    {
        checkNull( expression, "Expression" );
        return new Value( new FunctionExpression( "sum", expression ) );
    }

    /**
     * Declare a avg(expression) RETURN expression
     *
     * @return
     */
    public static NumericExpression avg( Expression expression )
    {
        checkNull( expression, "Expression" );
        return new Value( new FunctionExpression( "avg", expression ) );
    }

    /**
     * Declare a max(expression) RETURN expression
     *
     * @return
     */
    public static NumericExpression max( NumericExpression expression )
    {
        checkNull( expression, "Expression" );
        return new Value( new FunctionExpression( "max", expression ) );
    }

    /**
     * Declare a min(expression) RETURN expression
     *
     * @return
     */
    public static NumericExpression min( NumericExpression expression )
    {
        checkNull( expression, "Expression" );
        return new Value( new FunctionExpression( "min", expression ) );
    }

    /**
     * Declare a collect(expression) RETURN expression
     *
     * @return
     */
    public static CollectionExpression collect( ScalarExpression expression )
    {
        checkNull( expression, "Expression" );
        return new Value( new FunctionExpression( "collect", expression ) );
    }

    // Order by -----------------------------------------------------

    /**
     * Declare an ORDER clause expression. Typically used with identifier("n").property("property") as
     * parameter.
     *
     * @param expression
     * @return
     */
    public static OrderByExpression order( Expression expression )
    {
        Query.checkNull( expression, "Expression" );
        OrderByExpression orderBy = new OrderByExpression();
        orderBy.expression = expression;
        return orderBy;
    }

    /**
     * Declare an ORDER clause expression, with either ASCENDING or DESCENDING order
     * explicitly set. Typically used with identifier("n").property("property") as
     * parameter.
     *
     * @param expression
     * @param order
     * @return
     */
    public static OrderByExpression order( Expression expression, Order order )
    {
        Query.checkNull( expression, "Name" );
        Query.checkNull( order, "Order" );
        OrderByExpression orderBy = new OrderByExpression();
        orderBy.expression = expression;
        orderBy.order = order;
        return orderBy;
    }

    // For each -----------------------------------------------------

    /**
     * Use this to create expressions for use with the FOR EACH clause. Use
     * the fluent API of ForEachStatements to create the statements to be evaluated
     * in the FOR EACH clause.
     *
     * @param id
     * @param in
     * @return
     */
    public static ForEachStatements in( String id, Expression in )
    {
        return new ForEachClause( identifier( id ), in );
    }

    /**
     * Use this to create expressions for use with the FOR EACH clause. Use
     * the fluent API of ForEachStatements to create the statements to be evaluated
     * in the FOR EACH clause.
     *
     * @param id
     * @param in
     * @return
     */
    public static ForEachStatements in( Identifier id, Expression in )
    {
        return new ForEachClause( id, in );
    }

    // Set ----------------------------------------------------------

    /**
     * Use this to set properties in the SET clause.
     * <p/>
     * Corresponds to:
     * <pre>
     * property=value
     * </pre>
     *
     * @param property
     * @param value
     * @return
     */
    public static SetProperty property( Property property, Expression value )
    {
        return new SetProperty( property, value );
    }

    // Functions ----------------------------------------------------

    /**
     * Declare an ALL expression. Corresponds to:
     * <pre>
     * ALL(name IN iterable WHERE expression)
     * </pre>
     *
     * @param name
     * @param iterable
     * @param predicateExpression
     * @return
     */
    public static BooleanExpression all( String name, CollectionExpression iterable,
                                         BooleanExpression predicateExpression )
    {
        return all( identifier( name ), iterable, predicateExpression );
    }

    /**
     * Declare an ALL expression. Corresponds to:
     * <pre>
     * ALL(name IN iterable WHERE expression)
     * </pre>
     *
     * @param name
     * @param iterable
     * @param predicateExpression
     * @return
     */
    public static BooleanExpression all( Identifier name, CollectionExpression iterable,
                                         BooleanExpression predicateExpression )
    {
        Query.checkNull( name, "Name" );
        Query.checkNull( iterable, "Iterable" );
        Query.checkNull( predicateExpression, "Predicate" );

        IterablePredicateExpression expression = new IterablePredicateExpression();
        expression.function = "all";
        expression.name = name;
        expression.iterable = iterable;
        expression.predicate = predicateExpression;
        return new Value( expression );
    }

    /**
     * Declare an ANY expression. Corresponds to:
     * <pre>
     * ANY(name IN iterable WHERE expression)
     * </pre>
     *
     * @param name
     * @param iterable
     * @param predicateExpression
     * @return
     */
    public static BooleanExpression any( String name, CollectionExpression iterable,
                                         BooleanExpression predicateExpression )
    {
        return any( identifier( name ), iterable, predicateExpression );
    }

    /**
     * Declare an ANY expression. Corresponds to:
     * <pre>
     * ANY(name IN iterable WHERE expression)
     * </pre>
     *
     * @param name
     * @param iterable
     * @param predicateExpression
     * @return
     */
    public static BooleanExpression any( Identifier name, CollectionExpression iterable,
                                         BooleanExpression predicateExpression )
    {
        Query.checkNull( name, "Name" );
        Query.checkNull( iterable, "Iterable" );
        Query.checkNull( predicateExpression, "Predicate" );

        IterablePredicateExpression expression = new IterablePredicateExpression();
        expression.function = "any";
        expression.name = name;
        expression.iterable = iterable;
        expression.predicate = predicateExpression;
        return new Value( expression );
    }

    /**
     * Declare a NONE expression. Corresponds to:
     * <pre>
     * NONE(name IN iterable WHERE expression)
     * </pre>
     *
     * @param name
     * @param iterable
     * @param predicateExpression
     * @return
     */
    public static BooleanExpression none( String name, CollectionExpression iterable,
                                          BooleanExpression predicateExpression )
    {
        return none( identifier( name ), iterable, predicateExpression );
    }

    /**
     * Declare a NONE expression. Corresponds to:
     * <pre>
     * NONE(name IN iterable WHERE expression)
     * </pre>
     *
     * @param name
     * @param iterable
     * @param predicateExpression
     * @return
     */
    public static BooleanExpression none( Identifier name, CollectionExpression iterable,
                                          BooleanExpression predicateExpression )
    {
        Query.checkNull( name, "Name" );
        Query.checkNull( iterable, "Iterable" );
        Query.checkNull( predicateExpression, "Predicate" );

        IterablePredicateExpression expression = new IterablePredicateExpression();
        expression.function = "none";
        expression.name = name;
        expression.iterable = iterable;
        expression.predicate = predicateExpression;
        return new Value( expression );
    }

    /**
     * Declare a SINGLE expression. Corresponds to:
     * <pre>
     * SINGLE(name IN iterable WHERE expression)
     * </pre>
     *
     * @param name
     * @param iterable
     * @param predicateExpression
     * @return
     */
    public static BooleanExpression single( String name, CollectionExpression iterable,
                                            BooleanExpression predicateExpression )
    {
        return single( identifier( name ), iterable, predicateExpression );
    }

    /**
     * Declare a SINGLE expression. Corresponds to:
     * <pre>
     * SINGLE(name IN iterable WHERE expression)
     * </pre>
     *
     * @param name
     * @param iterable
     * @param predicateExpression
     * @return
     */
    public static BooleanExpression single( Identifier name, CollectionExpression iterable,
                                            BooleanExpression predicateExpression )
    {
        Query.checkNull( name, "Name" );
        Query.checkNull( iterable, "Iterable" );
        Query.checkNull( predicateExpression, "Predicate" );

        IterablePredicateExpression expression = new IterablePredicateExpression();
        expression.function = "single";
        expression.name = name;
        expression.iterable = iterable;
        expression.predicate = predicateExpression;
        return new Value( expression );
    }

    // Scalar expressions

    /**
     * Declare a length expression. Corresponds to:
     * <pre>
     * length(iterable)
     * </pre>
     *
     * @param expression
     * @return
     */
    public static NumericExpression length( CollectionExpression expression )
    {
        checkNull( expression, "Expression" );
        return new Value( new FunctionExpression( "length", expression ) );
    }

    /**
     * Declare a type expression. Corresponds to:
     * <pre>
     * type(relationship)
     * </pre>
     *
     * @param relationshipExpression
     * @return
     */
    public static StringExpression type( RelationshipExpression relationshipExpression )
    {
        checkNull( relationshipExpression, "Expression" );
        return new Value( new FunctionExpression( "type", relationshipExpression ) );
    }

    /**
     * Declare an id expression. Corresponds to:
     * <pre>
     * id(name)
     * </pre>
     *
     * @param name
     * @return
     */
    public static NumericExpression id( String name )
    {
        checkNull( name, "Name" );
        return new Value( new FunctionExpression( "id", identifier( name ) ) );
    }

    /**
     * Declare an id expression. Corresponds to:
     * <pre>
     * id(propertyContainer)
     * </pre>
     *
     * @param propertyContainerExpression
     * @return
     */
    public static NumericExpression id( PropertyContainerExpression propertyContainerExpression )
    {
        checkNull( propertyContainerExpression, "Expression" );
        return new Value( new FunctionExpression( "id", propertyContainerExpression ) );
    }

    /**
     * Declare a coalesce expression. Corresponds to:
     * <pre>
     * coalesce(expression1,expression2,expression3)
     * </pre>
     *
     * @param expressions
     * @return
     */
    public static Value coalesce( Expression... expressions )
    {
        if ( expressions.length < 1 )
        {
            throw new IllegalArgumentException( "At least one expression must be provided to coalesce function" );
        }

        return new Value( new FunctionExpression( "coalesce", new Expressions( expressions ) ) );
    }

    /**
     * Declare a head expression. Corresponds to:
     * <pre>
     * head(collection)
     * </pre>
     *
     * @param collectionExpression
     * @return
     */
    public static Expression head( CollectionExpression collectionExpression )
    {
        checkNull( collectionExpression, "Expression" );
        return new Value( new FunctionExpression( "head", collectionExpression ) );
    }

    /**
     * Declare a last expression. Corresponds to:
     * <pre>
     * last(collection)
     * </pre>
     *
     * @param collectionExpression
     * @return
     */
    public static Expression last( CollectionExpression collectionExpression )
    {
        checkNull( collectionExpression, "Expression" );
        return new Value( new FunctionExpression( "last", collectionExpression ) );
    }

    // Iterable expressions

    /**
     * Declare a nodes expression. Corresponds to:
     * <pre>
     * nodes(path)
     * </pre>
     *
     * @param pathExpression
     * @return
     */
    public static CollectionExpression nodes( PathExpression pathExpression )
    {
        checkNull( pathExpression, "Expression" );

        return new Value( new FunctionExpression( "nodes", pathExpression ) );
    }

    /**
     * Declare a relationships expression. Corresponds to:
     * <pre>
     * relationships(path)
     * </pre>
     *
     * @param pathExpression
     * @return
     */
    public static CollectionExpression relationships( PathExpression pathExpression )
    {
        checkNull( pathExpression, "Expression" );

        return new Value( new FunctionExpression( "relationships", pathExpression ) );
    }

    /**
     * Declare an extract expression. Corresponds to:
     * <pre>
     * extract(name IN iterable : expression)
     * </pre>
     *
     * @param iterable
     * @param expression
     * @return
     */
    public static CollectionExpression extract( String name, CollectionExpression iterable,
                                                ScalarExpression expression )
    {
        return extract( identifier( name ), iterable, expression );
    }

    /**
     * Declare an extract expression. Corresponds to:
     * <pre>
     * extract(name IN iterable : expression)
     * </pre>
     *
     * @param iterable
     * @param expression
     * @return
     */
    public static CollectionExpression extract( Identifier name, CollectionExpression iterable,
                                                ScalarExpression expression )
    {
        Query.checkNull( name, "Name" );
        Query.checkNull( iterable, "Iterable" );
        Query.checkNull( expression, "Expression" );

        Extract extract = new Extract();
        extract.name = name;
        extract.iterable = iterable;
        extract.expression = expression;
        return new Value( extract );
    }

    /**
     * Declare a filter expression. Corresponds to:
     * <pre>
     * filter(name IN iterable : predicate)
     * </pre>
     *
     * @param iterable
     * @param predicateExpression
     * @return
     */
    public static CollectionExpression filter( String name, CollectionExpression iterable,
                                               BooleanExpression predicateExpression )
    {
        return filter( identifier( name ), iterable, predicateExpression );
    }

    /**
     * Declare a filter expression. Corresponds to:
     * <pre>
     * filter(name IN iterable : predicate)
     * </pre>
     *
     * @param iterable
     * @param predicateExpression
     * @return
     */
    public static CollectionExpression filter( Identifier name, CollectionExpression iterable,
                                               BooleanExpression predicateExpression )
    {
        Query.checkNull( name, "Name" );
        Query.checkNull( iterable, "Iterable" );
        Query.checkNull( predicateExpression, "Predicate" );

        Filter filter = new Filter();
        filter.name = name;
        filter.iterable = iterable;
        filter.predicate = predicateExpression;
        return new Value( filter );
    }

    /**
     * Declare a tail expression. Corresponds to:
     * <pre>
     * tail(collectionExpression)
     * </pre>
     *
     * @param collectionExpression
     * @return
     */
    public static CollectionExpression tail( CollectionExpression collectionExpression )
    {
        checkNull( collectionExpression, "Expression" );
        return new Value( new FunctionExpression( "tail", collectionExpression ) );
    }

    /**
     * Declare a range expression. Corresponds to:
     * <pre>
     * range(start,end)
     * </pre>
     *
     * @param start
     * @param end
     * @return
     */
    public static CollectionExpression range( Number start, Number end )
    {
        return range( literal( start ), literal( end ), null );
    }

    /**
     * Declare a range expression. Corresponds to:
     * <pre>
     * range(start,end,step)
     * </pre>
     *
     * @param start
     * @param end
     * @param step
     * @return
     */
    public static CollectionExpression range( Number start, Number end, Number step )
    {
        return range( literal( start ), literal( end ), literal( step ) );
    }

    /**
     * Declare a range expression. Corresponds to:
     * <pre>
     * range(start,end)
     * </pre>
     *
     * @param start
     * @param end
     * @return
     */
    public static CollectionExpression range( NumericExpression start, NumericExpression end )
    {
        return range( start, end, null );
    }

    /**
     * Declare a range expression. Corresponds to:
     * <pre>
     * range(start,end,step)
     * </pre>
     *
     * @param start
     * @param end
     * @param step
     * @return
     */
    public static CollectionExpression range( NumericExpression start, NumericExpression end, NumericExpression step )
    {
        if ( step == null )
        {
            return new Value( new FunctionExpression( "range", new Expressions( new Expression[]{start, end} ) ) );
        }
        else
        {
            return new Value( new FunctionExpression( "range", new Expressions( new Expression[]{start, end,
                    step} ) ) );
        }
    }

    // Mathematical expressions

    /**
     * Declare extra parentheses. This can be useful to ensure that operators are performed in the order you desire.
     * <p/>
     * Corresponds to:
     * <pre>
     * (numericExpression)
     * </pre>
     */
    public static NumericExpression p( NumericExpression numericExpression )
    {
        return new Value( new FunctionExpression( "", numericExpression ) );
    }

    /**
     * Declare an abs expression. Corresponds to:
     * <pre>
     * abs(expression)
     * </pre>
     *
     * @param numericalExpression
     * @return
     */
    public static NumericExpression abs( Number numericalExpression )
    {
        return abs( literal( numericalExpression ) );
    }

    /**
     * Declare an abs expression. Corresponds to:
     * <pre>
     * abs(expression)
     * </pre>
     *
     * @param numericalExpression
     * @return
     */
    public static NumericExpression abs( NumericExpression numericalExpression )
    {
        return new Value( new FunctionExpression( "abs", numericalExpression ) );
    }

    /**
     * Declare a round function. Corresponds to:
     * <pre>
     * round(expression)
     * </pre>
     *
     * @param numericalExpression
     * @return
     */
    public static NumericExpression round( Number numericalExpression )
    {
        return round( literal( numericalExpression ) );
    }

    /**
     * Declare a round function. Corresponds to:
     * <pre>
     * round(expression)
     * </pre>
     *
     * @param numericalExpression
     * @return
     */
    public static NumericExpression round( NumericExpression numericalExpression )
    {
        return new Value( new FunctionExpression( "round", numericalExpression ) );
    }

    /**
     * Declare a sqrt expression. Corresponds to:
     * <pre>
     * sqrt(expression)
     * </pre>
     *
     * @param numericalExpression
     * @return
     */
    public static NumericExpression sqrt( Number numericalExpression )
    {
        return sqrt( literal( numericalExpression ) );
    }

    /**
     * Declare a sqrt expression. Corresponds to:
     * <pre>
     * sqrt(expression)
     * </pre>
     *
     * @param numericalExpression
     * @return
     */
    public static NumericExpression sqrt( NumericExpression numericalExpression )
    {
        return new Value( new FunctionExpression( "sqrt", numericalExpression ) );
    }

    /**
     * Declare a sign expression. Corresponds to:
     * <pre>
     * sign(expression)
     * </pre>
     *
     * @param numericalExpression
     * @return
     */
    public static NumericExpression sign( Number numericalExpression )
    {
        return sign( literal( numericalExpression ) );
    }

    /**
     * Declare a sign expression. Corresponds to:
     * <pre>
     * sign(expression)
     * </pre>
     *
     * @param numericalExpression
     * @return
     */
    public static NumericExpression sign( NumericExpression numericalExpression )
    {
        return new Value( new FunctionExpression( "sign", numericalExpression ) );
    }

    @Override
    public String toString()
    {
        return query.toString();
    }

    // Grammar
    protected class Grammar
            implements StartNext, With, WithNext, Create, Set, Delete, Relate, UpdateNext, Match, ReturnNext, OrderBy,
            Skip, Limit, Execute
    {
        // With ---------------------------------------------------------
        public WithNext with( Expression... withExpressions )
        {
            query.add( new WithClause( Arrays.asList( withExpressions ) ) );

            return this;
        }

        @Override
        public WithNext with( Iterable<Expression> withExpressions )
        {
            query.add( new WithClause( withExpressions ) );

            return this;
        }

        // Create -------------------------------------------------------
        @Override
        public UpdateNext create( PathExpression... paths )
        {
            query.add( new CreateClause( Arrays.asList( paths ) ) );

            return this;
        }

        @Override
        public UpdateNext create( Iterable<PathExpression> paths )
        {
            query.add( new CreateClause( paths ) );

            return this;
        }


        // Set ----------------------------------------------------------
        @Override
        public UpdateNext set( SetProperty... setProperties )
        {
            query.add( new SetClause( Arrays.asList( setProperties ) ) );

            return this;
        }

        @Override
        public UpdateNext set( Iterable<SetProperty> setProperties )
        {
            query.add( new SetClause( setProperties ) );

            return this;
        }

        // Delete -------------------------------------------------------
        @Override
        public UpdateNext delete( ReferenceExpression... expressions )
        {
            query.add( new DeleteClause( Arrays.asList( expressions ) ) );

            return this;
        }

        @Override
        public UpdateNext delete( Iterable<ReferenceExpression> expressions )
        {
            query.add( new DeleteClause( expressions ) );

            return this;
        }

        // Relate -------------------------------------------------------
        @Override
        public UpdateNext relate( PathExpression... expressions )
        {
            query.add( new RelateClause( Arrays.asList( expressions ) ) );

            return this;
        }

        @Override
        public UpdateNext relate( Iterable<PathExpression> expressions )
        {
            query.add( new RelateClause( expressions ) );

            return this;
        }

        // For each -----------------------------------------------------

        @Override
        public UpdateNext forEach( ForEachStatement statement )
        {
            query.add( statement.getClause() );
            return this;
        }

        // Start --------------------------------------------------------
        @Override
        public StartNext starts( StartExpression... startExpression )
        {
            query.add( new StartClause( Arrays.asList( startExpression ) ) );
            return this;
        }

        @Override
        public StartNext starts( Iterable<StartExpression> startExpression )
        {
            query.add( new StartClause( startExpression ) );
            return this;
        }

        // Match --------------------------------------------------------
        @Override
        public Match match( PathExpression... expressions )
        {
            query.add( new MatchClause( Arrays.asList( expressions ) ) );
            return this;
        }

        @Override
        public Match match( Iterable<PathExpression> expressions )
        {
            query.add( new MatchClause( expressions ) );
            return this;
        }

        // Where --------------------------------------------------------
        @Override
        public Where where( BooleanExpression expression )
        {
            Query.checkNull( expression, "Expression" );
            query.add( new WhereClause( expression ) );
            return this;
        }

        // Return -------------------------------------------------------
        @Override
        public ReturnNext returns( Expression... returnExpressions )
        {
            query.add( new ReturnClause( Arrays.asList( returnExpressions ) ) );
            return this;
        }

        @Override
        public ReturnNext returns( Iterable<Expression> returnExpressions )
        {
            query.add( new ReturnClause( returnExpressions ) );
            return this;
        }

        // OrderBy ------------------------------------------------------
        @Override
        public OrderBy orderBy( Expression... orderByExpressions )
        {
            query.add( new OrderByClause( Arrays.asList( orderByExpressions ) ) );
            return this;
        }

        @Override
        public OrderBy orderBy( Iterable<Expression> orderByExpressions )
        {
            query.add( new OrderByClause( orderByExpressions ) );
            return this;
        }

        // Skip ---------------------------------------------------------
        @Override
        public Limit skip( int skip )
        {
            if ( skip < 0 )
            {
                throw new IllegalArgumentException( "Skip may not be below zero" );
            }

            query.add( new SkipClause( skip ) );
            return this;
        }

        // Limit --------------------------------------------------------
        @Override
        public Execute limit( int limit )
        {
            if ( limit < 0 )
            {
                throw new IllegalArgumentException( "Limit may not be below zero" );
            }

            query.add( new LimitClause( limit ) );
            return this;
        }

        // Execute ------------------------------------------------------
        @Override
        public void asString( StringBuilder builder )
        {
            query.asString( builder );
        }

        @Override
        public Query toQuery()
        {
            return query;
        }

        @Override
        public ExecuteWithParameters parameter( String name, Object value )
        {
            ExecuteWithParams withParams = new ExecuteWithParams( query );
            return withParams.parameter( name, value );
        }

        @Override
        public ExecuteWithParameters parameters( Map<String, Object> parameters )
        {
            ExecuteWithParams withParams = new ExecuteWithParams( query );
            withParams.getParameters().putAll( parameters );
            return withParams;
        }

        @Override
        public String toString()
        {
            return CypherQuery.this.toString();
        }
    }

    protected class ExecuteWithParams
            implements ExecuteWithParameters
    {
        private Query query;
        private Map<String, Object> parameters = new HashMap<String, Object>();

        public ExecuteWithParams( Query query )
        {
            this.query = query;
        }

        @Override
        public Query toQuery()
        {
            return query;
        }

        public Map<String, Object> getParameters()
        {
            return parameters;
        }

        @Override
        public ExecuteWithParameters parameter( String name, Object value )
        {
            parameters.put( name, value );
            return this;
        }

        @Override
        public ExecuteWithParameters parameters( Map<String, Object> parameters )
        {
            parameters.putAll( parameters );
            return this;
        }

        @Override
        public void asString( StringBuilder builder )
        {
            query.asString( builder );
        }

        @Override
        public String toString()
        {
            return query.toString();
        }
    }


    public static class And
            extends Value
    {
        public And( BooleanExpression[] value )
        {
            super( new Expressions( value ) );
        }

        @Override
        public void asString( StringBuilder builder )
        {
            Expressions expressions = (Expressions) value;

            for ( int i = 0; i < expressions.expressions.length; i++ )
            {
                Expression expression = expressions.expressions[i];
                if ( i > 0 )
                {
                    builder.append( " and " );
                }
                if ( expression instanceof And || expression instanceof Or )
                {
                    builder.append( '(' );
                    expression.asString( builder );
                    builder.append( ')' );
                }
                else
                {
                    expression.asString( builder );
                }
            }
        }
    }

    public static class Or
            extends Value
    {
        public Or( BooleanExpression[] value )
        {
            super( new Expressions( value ) );
        }

        @Override
        public void asString( StringBuilder builder )
        {
            Expressions expressions = (Expressions) value;

            for ( int i = 0; i < expressions.expressions.length; i++ )
            {
                Expression expression = expressions.expressions[i];
                if ( i > 0 )
                {
                    builder.append( " or " );
                }
                if ( expression instanceof And )
                {
                    builder.append( '(' );
                    expression.asString( builder );
                    builder.append( ')' );
                }
                else
                {
                    expression.asString( builder );
                }
            }
        }
    }
}
