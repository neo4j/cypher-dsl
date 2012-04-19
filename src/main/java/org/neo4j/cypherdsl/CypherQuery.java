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


import org.neo4j.cypherdsl.query.BinaryPredicateExpression;
import org.neo4j.cypherdsl.query.BooleanExpression;
import org.neo4j.cypherdsl.query.Expression;
import org.neo4j.cypherdsl.query.FunctionExpression;
import org.neo4j.cypherdsl.query.Has;
import org.neo4j.cypherdsl.query.Identifier;
import org.neo4j.cypherdsl.query.IsNotNull;
import org.neo4j.cypherdsl.query.IsNull;
import org.neo4j.cypherdsl.query.IterablePredicateExpression;
import org.neo4j.cypherdsl.query.Literal;
import org.neo4j.cypherdsl.query.MatchExpression;
import org.neo4j.cypherdsl.query.NumberProperty;
import org.neo4j.cypherdsl.query.Order;
import org.neo4j.cypherdsl.query.OrderByExpression;
import org.neo4j.cypherdsl.query.Parameter;
import org.neo4j.cypherdsl.query.PredicateExpression;
import org.neo4j.cypherdsl.query.Query;
import org.neo4j.cypherdsl.query.ReturnExpression;
import org.neo4j.cypherdsl.query.StartExpression;
import org.neo4j.cypherdsl.query.StringProperty;
import org.neo4j.cypherdsl.query.WhereExpression;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import static org.neo4j.cypherdsl.query.FunctionExpression.Extract;
import static org.neo4j.cypherdsl.query.Query.*;

import org.neo4j.cypherdsl.query.Regexp;

/**
 * DSL for creating Cypher queries. Once created you can serialize to a string,
 * or retrieve the internal Query model for further processing.
 *
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
     * Start building a new Cypher query
     *
     * @param startExpressions list of start expressions
     * @return Grammar for Match clause
     */
    public static Match start( StartExpression... startExpressions )
    {
        CypherQuery query = new CypherQuery();
        return query.starts( startExpressions );
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
     *
     * Example:
     * <pre>
     *     new CypherQuery()
     *     {{
     *         starts(node("n",1)).returns(identifier("n"));
     *     }}.toString()
     * </pre>
     *
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
        catch( CloneNotSupportedException e )
        {
            throw new IllegalStateException( "Query was not cloneable" );
        }
    }

    // Common -------------------------------------------------------

    /**
     *
     * Declare a Cypher query parameter.
     * This will be replaced with {name}.
     *
     * @param name of the parameter
     * @return Parameter instance
     */
    public static Parameter param(String name)
    {
        checkEmpty( name, "Name" );
        Parameter parameter = new Parameter();
        parameter.name = name;
        return parameter;
    }

    /**
     * Declare a literal value, such "Foo" or 3.
     * If a string is passed in, then output will
     * be quoted appropriately.
     *
     * @param value literal value
     * @return Literal instance
     */
    public static Literal literal( Object value )
    {
        checkNull( value, "Value" );
        Literal literal = new Literal();
        literal.value = value;
        return literal;
    }

    /**
     * Declare an identifier. This is used to
     * refer to names declared elsewhere in the query.
     *
     * If you want to refer to properties, then create
     * this first, and then call e.g. .property("propname")
     *
     * @param name
     * @return
     */
    public static Identifier identifier( String name )
    {
        checkEmpty( name, "Identifier" );
        Identifier identifier = new Identifier();
        identifier.name = name;
        return identifier;
    }

    /**
     * Declare a string literal that you want to
     * perform operations on.
     *
     * @param name
     * @return
     */
    public static StringProperty string( String name )
    {
        checkEmpty( name, "Name" );
        StringProperty property = new StringProperty();
        property.name = identifier( name);
        return property;
    }

    /**
     * Declare a number literal that you want to
     * perform operations on.
     *
     * @param name
     * @return
     */
    public static NumberProperty number( String name )
    {
        checkEmpty( name, "Name" );
        NumberProperty property = new NumberProperty();
        property.name = identifier( name);
        return property;
    }

    /**
     * Declare a list of literal values.
     *
     * @param values
     * @return
     */
    public static Literal[] literals( Object... values )
    {
        Literal[] literals = new Literal[values.length];
        for( int i = 0; i < values.length; i++ )
        {
            Object value = values[ i ];
            literals[i] = literal(value);
        }
        return literals;
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
        for( int i = 0; i < values.length; i++ )
        {
            String value = values[ i ];
            identifiers[i] = identifier(value);
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
        for( int i = 0; i < names.length; i++ )
        {
            String value = names[ i ];
            parameters[i] = param( value );
        }
        return parameters;
    }

    /**
     * Declare a list of literals using node values
     *
     * @param values
     * @return
     */
    public static Literal[] literals( long... values )
    {
        Literal[] literals = new Literal[values.length];
        for( int i = 0; i < values.length; i++ )
        {
            Object value = values[ i ];
            literals[i] = literal(value);
        }
        return literals;
    }

    /**
     * "and" a series of expressions together.
     *
     * @param expressions
     * @return
     */
    public static BooleanExpression.And and(PredicateExpression... expressions)
    {
        Query.checkNull( expressions, "Expressions" );

        BooleanExpression.And and = new BooleanExpression.And();
        and.expressions = expressions;
        return and;
    }

    /**
     * "or" a series of expressions together.
     *
     * @param expressions
     * @return
     */
    public static BooleanExpression.Or or(PredicateExpression... expressions)
    {
        Query.checkNull( expressions, "Expressions" );

        BooleanExpression.Or or = new BooleanExpression.Or();
        or.expressions = expressions;
        return or;
    }

    /**
     * Invert the boolean value of a predicate.
     *
     * @param expression
     * @return
     */
    public static BooleanExpression.Not not(PredicateExpression expression)
    {
        Query.checkNull( expression, "Expression" );

        BooleanExpression.Not not = new BooleanExpression.Not();
        not.expression = expression;
        return not;
    }

    /**
     * "=" operation
     *
     * @param left expression
     * @param right expression
     * @return predicate expression
     */
    public static BinaryPredicateExpression eq(Object left, Object right)
    {
        return binaryPredicate( "=", left, right );
    }

    /**
     * ">" operation
     *
     * @param left expression
     * @param right expression
     * @return predicate expression
     */
    public static BinaryPredicateExpression gt(Object left, Object right)
    {
        return binaryPredicate( ">", left, right );
    }

    /**
     * "<" operation
     *
     * @param left expression
     * @param right expression
     * @return predicate expression
     */
    public static BinaryPredicateExpression lt(Object left, Object right)
    {
        return binaryPredicate( "<", left, right );
    }

    /**
     * ">=" operation
     *
     * @param left expression
     * @param right expression
     * @return predicate expression
     */
    public static BinaryPredicateExpression gte(Object left, Object right)
    {
        return binaryPredicate( ">=", left, right );
    }

    /**
     * ">=" operation
     *
     * @param left expression
     * @param right expression
     * @return predicate expression
     */
    public static BinaryPredicateExpression lte(Object left, Object right)
    {
        return binaryPredicate( ">=", left, right );
    }

    /**
     * "<>" operation
     *
     * @param left expression
     * @param right expression
     * @return predicate expression
     */
    public static BinaryPredicateExpression ne(Object left, Object right)
    {
        return binaryPredicate( "<>", left, right );
    }

    private static BinaryPredicateExpression binaryPredicate( String operator, Object left, Object right )
    {
        Query.checkNull( left, "Left expression" );
        Query.checkNull( right, "Right expression" );

        BinaryPredicateExpression binaryPredicateExpression = new BinaryPredicateExpression();
        binaryPredicateExpression.operator = operator;
        binaryPredicateExpression.left = left instanceof Expression ? (Expression) left : CypherQuery.literal( left );
        binaryPredicateExpression.right = right instanceof Expression ? (Expression) right : CypherQuery.literal( right );
        return binaryPredicateExpression;
    }

    /**
     * Create a case-sensitive regular expression. Corresponds to:
     * <pre>
     *     property ~=/regex/
     * </pre>
     *
     * @param property
     * @param regexp
     * @return
     */
    public static Regexp regexp( Expression property, Expression regexp )
    {
        return regexp( property, regexp, true );
    }

    /**
     * Create a regular expression. Corresponds to:
     * <pre>
     *     property ~=/regex/
     * </pre>
     *
     * @param property
     * @param regexp
     * @param caseSensitive
     * @return
     */
    public static Regexp regexp( Expression property, Expression regexp, boolean caseSensitive )
    {
        Regexp regularExpression = new Regexp();
        regularExpression.caseSensitive = caseSensitive;
        regularExpression.left = property;
        regularExpression.regexp = regexp;
        return regularExpression;
    }

    /**
     * has(expression)
     *
     * @param expression
     * @return
     */
    public Has has(Expression expression)
    {
        Has has = new Has();
        has.expression = expression;
        return has;
    }

    /**
     * expression is null
     *
     * @param expression
     * @return
     */
    public IsNull isNull(Expression expression)
    {
        IsNull isNull = new IsNull();
        isNull.expression = expression;
        return isNull;
    }

    /**
     * expression is not null
     *
     * @param expression
     * @return
     */
    public IsNotNull isNotNull(Expression expression)
    {
        IsNotNull isNotNull = new IsNotNull();
        isNotNull.expression = expression;
        return isNotNull;
    }

    // Start --------------------------------------------------------
    /**
     * START clause. Use this with Java initialization block style.
     *
     * @param startExpression
     * @return
     */
    protected Match starts( StartExpression... startExpression )
    {
        Collections.addAll(query.startExpressions, startExpression);

        return new Grammar();
    }

    /**
     * START clause. Use this with Java initialization block style.
     *
     * @param startExpression
     * @return
     */
    protected Match starts( Iterable<StartExpression> startExpression )
    {
        for (StartExpression expression : startExpression)
        {
            query.startExpressions.add(expression);
        }

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
    public static StartExpression.StartNodes node( String name, long... id )
    {
        return node( identifier( name ), id );
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
    public static StartExpression.StartNodes node( Identifier name, long... id )
    {
        checkNull( name, "Name" );

        for( long i : id )
        {
            if (i < 0)
                throw new IllegalArgumentException( "Id may not be below zero" );
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
    public static StartExpression.StartNodes node( String name, String parameter )
    {
        return node( identifier( name), parameter );
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
    public static StartExpression.StartNodes node( Identifier name, String parameter )
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
        return allNodes( identifier( name ));
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
        return lookup( identifier( name ), identifier(indexName), identifier( key ), literal( value ) );
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
    public static StartExpression.StartNodesLookup lookup( Identifier name, Identifier indexName, Identifier key, Literal value )
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
        return query(identifier( name ), identifier( indexName ), query );
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
    public static StartExpression.StartRelationships relationship( String name, long... id )
    {
        return relationship( identifier( name ), id );
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
    public static StartExpression.StartRelationships relationship( Identifier name, long... id )
    {
        checkNull( name, "Name" );

        for( long i : id )
        {
            if (i < 0)
                throw new IllegalArgumentException( "Id may not be below zero" );
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
    public static StartExpression.StartRelationshipsParameters relationship( String name, String parameter )
    {
        return relationship( identifier( name ), parameter);
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
    public static StartExpression.StartRelationshipsParameters relationship( Identifier name, String parameter )
    {
        checkNull( name, "Name" );
        checkEmpty( parameter, "Parameter" );

        StartExpression.StartRelationshipsParameters startRelationships = new StartExpression.StartRelationshipsParameters();
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
    public static StartExpression.StartRelationshipsIndex relationshipLookup( String name, String indexName, String key, String value )
    {
        return relationshipLookup( identifier( name ), identifier( indexName ), identifier( key ), literal( value));
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
    public static StartExpression.StartRelationshipsIndex relationshipLookup( Identifier name, Identifier indexName, Identifier key, Literal value )
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
     * Declare a path for MATCH clauses
     *
     * @return
     */
    public static MatchExpression.Path path()
    {
        return new MatchExpression.Path();
    }

    /**
     * Declare a named path for MATCH clauses
     *
     * @param name
     * @return
     */
    public static MatchExpression.Path path(String name)
    {
        return path(identifier( name ));
    }

    /**
     * Declare a named path for MATCH clauses
     *
     * @param name
     * @return
     */
    public static MatchExpression.Path path(Identifier name)
    {
        checkNull( name, "Name" );
        MatchExpression.Path path = new MatchExpression.Path();
        path.pathName = name;
        return path;
    }

    /**
     * Use this to invoke the shortestPath function
     *
     * @param name
     * @return
     */
    public static MatchExpression.FunctionPath shortestPath( String name )
    {
        return shortestPath( identifier( name ) );
    }

    /**
     * Use this to invoke the shortestPath function
     *
     * @param name
     * @return
     */
    public static MatchExpression.FunctionPath shortestPath( Identifier name )
    {
        Query.checkNull( name, "Name" );

        MatchExpression.FunctionPath functionPath = new MatchExpression.FunctionPath();
        functionPath.function = "shortestPath";
        functionPath.pathName = name;
        return functionPath;
    }

    /**
     * Use this to invoke the allShortestPaths function
     *
     * @param name
     * @return
     */
    public static MatchExpression.FunctionPath allShortestPaths( String name )
    {
        return allShortestPaths( identifier( name ) );
    }

    /**
     * Use this to invoke the allShortestPaths function
     *
     * @param name
     * @return
     */
    public static MatchExpression.FunctionPath allShortestPaths( Identifier name )
    {
        Query.checkNull( name, "Name" );

        MatchExpression.FunctionPath functionPath = new MatchExpression.FunctionPath();
        functionPath.function = "allShortestPaths";
        functionPath.pathName = name;
        return functionPath;
    }

    // Where --------------------------------------------------------
    /**
     * Filter on relationships in WHERE clause
     *
     * @return
     */
    public static WhereExpression.WhereRelationship relationship()
    {
        return new WhereExpression.WhereRelationship();
    }

    /**
     * Declare an IN operator. Corresponds to:
     * <pre>
     *     expression IN [element1,element2,element3]
     * </pre>
     *
     * @param expression
     * @param elements
     * @return
     */
    public static WhereExpression.WhereIn in(Expression expression, Expression... elements)
    {
        Query.checkNull( expression, "Expression" );

        WhereExpression.WhereIn in = new WhereExpression.WhereIn();
        in.expression = expression;
        in.elements = elements;
        return in;
    }


    // Return -------------------------------------------------------

    /**
     * If you want to specify AS or DISTINCT on an expression in the RETURN clause,
     * then use this.
     *
     * @param expression
     * @return
     */
    public static ReturnExpression<ReturnExpression> exp(Expression expression)
    {
        ReturnExpression<ReturnExpression> returnExpression = new ReturnExpression<ReturnExpression>();
        returnExpression.expression = expression;
        return returnExpression;
    }

    /**
     * Declare a count(*) RETURN expression
     *
     * @return
     */
    public static ReturnExpression.ReturnAggregate count()
    {
        ReturnExpression.ReturnAggregate returnAggregate = new ReturnExpression.ReturnAggregate();
        returnAggregate.function = "count";
        return returnAggregate;
    }

    /**
     * Declare a count(expression) RETURN expression
     *
     * @return
     */
    public static FunctionExpression count(Expression expression)
    {
        checkNull( expression, "Expression" );

        FunctionExpression returnAggregate = new FunctionExpression();
        returnAggregate.name = "count";
        returnAggregate.expression = expression;
        return returnAggregate;
    }

    /**
     * Declare a sum(expression) RETURN expression
     *
     * @return
     */
    public static FunctionExpression sum(Expression expression)
    {
        checkNull( expression, "Expression" );

        FunctionExpression returnAggregate = new FunctionExpression();
        returnAggregate.name = "sum";
        returnAggregate.expression = expression;
        return returnAggregate;
    }

    /**
     * Declare a avg(expression) RETURN expression
     *
     * @return
     */
    public static FunctionExpression avg(Expression expression)
    {
        checkNull( expression, "Expression" );

        FunctionExpression returnAggregate = new FunctionExpression();
        returnAggregate.name = "avg";
        returnAggregate.expression = expression;
        return returnAggregate;
    }

    /**
     * Declare a max(expression) RETURN expression
     *
     * @return
     */
    public static FunctionExpression max(Expression expression)
    {
        checkNull( expression, "Expression" );

        FunctionExpression returnAggregate = new FunctionExpression();
        returnAggregate.name = "max";
        returnAggregate.expression = expression;
        return returnAggregate;
    }

    /**
     * Declare a min(expression) RETURN expression
     *
     * @return
     */
    public static FunctionExpression min(Expression expression)
    {
        checkNull( expression, "Expression" );

        FunctionExpression returnAggregate = new FunctionExpression();
        returnAggregate.name = "min";
        returnAggregate.expression = expression;
        return returnAggregate;
    }

    /**
     * Declare a collect(expression) RETURN expression
     *
     * @return
     */
    public static FunctionExpression collect(Expression expression)
    {
        checkNull( expression, "Expression" );

        FunctionExpression returnAggregate = new FunctionExpression();
        returnAggregate.name = "collect";
        returnAggregate.expression = expression;
        return returnAggregate;
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


    // Functions ----------------------------------------------------
    /**
     * Declare an ALL expression. Corresponds to:
     * <pre>
     *     ALL(name IN iterable WHERE expression)
     * </pre>
     *
     * @param name
     * @param iterable
     * @param predicateExpression
     * @return
     */
    public static IterablePredicateExpression all( String name, Expression iterable, PredicateExpression predicateExpression )
    {
        return all( identifier( name ), iterable, predicateExpression );
    }

    /**
     * Declare an ALL expression. Corresponds to:
     * <pre>
     *     ALL(name IN iterable WHERE expression)
     * </pre>
     *
     * @param name
     * @param iterable
     * @param predicateExpression
     * @return
     */
    public static IterablePredicateExpression all( Identifier name, Expression iterable, PredicateExpression predicateExpression )
    {
        Query.checkNull( name, "Name" );
        Query.checkNull( iterable, "Iterable" );
        Query.checkNull( predicateExpression, "Predicate" );

        IterablePredicateExpression expression = new IterablePredicateExpression();
        expression.function = "all";
        expression.name = name;
        expression.iterable = iterable;
        expression.predicate = predicateExpression;
        return expression;
    }

    /**
     * Declare an ANY expression. Corresponds to:
     * <pre>
     *     ANY(name IN iterable WHERE expression)
     * </pre>
     *
     * @param name
     * @param iterable
     * @param predicateExpression
     * @return
     */
    public static IterablePredicateExpression any( String name,
                                                   Expression iterable,
                                                   PredicateExpression predicateExpression
    )
    {
        return any( identifier( name ), iterable, predicateExpression );
    }

    /**
     * Declare an ANY expression. Corresponds to:
     * <pre>
     *     ANY(name IN iterable WHERE expression)
     * </pre>
     *
     * @param name
     * @param iterable
     * @param predicateExpression
     * @return
     */
    public static IterablePredicateExpression any( Identifier name,
                                                   Expression iterable,
                                                   PredicateExpression predicateExpression
    )
    {
        Query.checkNull( name, "Name" );
        Query.checkNull( iterable, "Iterable" );
        Query.checkNull( predicateExpression, "Predicate" );

        IterablePredicateExpression expression = new IterablePredicateExpression();
        expression.function = "any";
        expression.name = name;
        expression.iterable = iterable;
        expression.predicate = predicateExpression;
        return expression;
    }

    /**
     * Declare a NONE expression. Corresponds to:
     * <pre>
     *     NONE(name IN iterable WHERE expression)
     * </pre>
     *
     * @param name
     * @param iterable
     * @param predicateExpression
     * @return
     */
    public static IterablePredicateExpression none( String name,
                                                    Expression iterable,
                                                   PredicateExpression predicateExpression
    )
    {
        return none( identifier( name ), iterable, predicateExpression );
    }

    /**
     * Declare a NONE expression. Corresponds to:
     * <pre>
     *     NONE(name IN iterable WHERE expression)
     * </pre>
     *
     * @param name
     * @param iterable
     * @param predicateExpression
     * @return
     */
    public static IterablePredicateExpression none( Identifier name,
                                                    Expression iterable,
                                                   PredicateExpression predicateExpression
    )
    {
        Query.checkNull( name, "Name" );
        Query.checkNull( iterable, "Iterable" );
        Query.checkNull( predicateExpression, "Predicate" );

        IterablePredicateExpression expression = new IterablePredicateExpression();
        expression.function = "none";
        expression.name = name;
        expression.iterable = iterable;
        expression.predicate = predicateExpression;
        return expression;
    }

    /**
     * Declare a SINGLE expression. Corresponds to:
     * <pre>
     *     SINGLE(name IN iterable WHERE expression)
     * </pre>
     *
     * @param name
     * @param iterable
     * @param predicateExpression
     * @return
     */
    public static IterablePredicateExpression single( String name,
                                                      Expression iterable,
                                                      PredicateExpression predicateExpression
    )
    {
        return single( identifier( name ), iterable, predicateExpression );
    }

    /**
     * Declare a SINGLE expression. Corresponds to:
     * <pre>
     *     SINGLE(name IN iterable WHERE expression)
     * </pre>
     *
     * @param name
     * @param iterable
     * @param predicateExpression
     * @return
     */
    public static IterablePredicateExpression single( Identifier name,
                                                      Expression iterable,
                                                      PredicateExpression predicateExpression
    )
    {
        Query.checkNull( name, "Name" );
        Query.checkNull( iterable, "Iterable" );
        Query.checkNull( predicateExpression, "Predicate" );

        IterablePredicateExpression expression = new IterablePredicateExpression();
        expression.function = "single";
        expression.name = name;
        expression.iterable = iterable;
        expression.predicate = predicateExpression;
        return expression;
    }

    // Scalar functions
    /**
     * Declare a length function. Corresponds to:
     * <pre>
     *     length(iterable)
     * </pre>
     *
     * @param iterableExpression
     * @return
     */
    public static FunctionExpression length(Expression iterableExpression)
    {
        checkNull( iterableExpression, "Expression" );

        FunctionExpression function = new FunctionExpression();
        function.name = "length";
        function.expression = iterableExpression;
        return function;
    }

    /**
     * Declare a type function. Corresponds to:
     * <pre>
     *     type(relationship)
     * </pre>
     *
     * @param relationshipExpression
     * @return
     */
    public static FunctionExpression type(Expression relationshipExpression)
    {
        checkNull( relationshipExpression, "Expression" );

        FunctionExpression function = new FunctionExpression();
        function.name = "type";
        function.expression = relationshipExpression;
        return function;
    }

    /**
     * Declare an id function. Corresponds to:
     * <pre>
     *     id(propertyContainer)
     * </pre>
     *
     * @param propertyContainerExpression
     * @return
     */
    public static FunctionExpression id(Expression propertyContainerExpression)
    {
        checkNull( propertyContainerExpression, "Expression" );

        FunctionExpression function = new FunctionExpression();
        function.name = "id";
        function.expression = propertyContainerExpression;
        return function;
    }

    /**
     * Declare a coalesce function. Corresponds to:
     * <pre>
     *     coalesce(expression1,expression2,expression3)
     * </pre>
     *
     * @param expressions
     * @return
     */
    public static FunctionExpression coalesce(Expression... expressions)
    {
        if (expressions.length < 1)
            throw new IllegalArgumentException("At least one expression must be provided to coalesce function");

        FunctionExpression coalesce = new FunctionExpression();
        coalesce.name = "coalesce";
        FunctionExpression.Expressions expressions1 = new FunctionExpression.Expressions();
        expressions1.expressions = expressions;
        coalesce.expression = expressions1;
        return coalesce;
    }

    /**
     * Declare a head function. Corresponds to:
     * <pre>
     *     head(collection)
     * </pre>
     *
     * @param collectionExpression
     * @return
     */
    public static FunctionExpression head(Expression collectionExpression)
    {
        checkNull( collectionExpression, "Expression" );

        FunctionExpression head = new FunctionExpression();
        head.name = "head";
        head.expression = collectionExpression;
        return head;
    }

    /**
     * Declare a head function. Corresponds to:
     * <pre>
     *     head(collection)
     * </pre>
     *
     * @param collectionExpression
     * @return
     */
    public static FunctionExpression last(Expression collectionExpression)
    {
        checkNull( collectionExpression, "Expression" );

        FunctionExpression last = new FunctionExpression();
        last.name = "last";
        last.expression = collectionExpression;
        return last;
    }

    // Iterable functions

    /**
     * Declare a nodes function. Corresponds to:
     * <pre>
     *     nodes(path)
     * </pre>
     * @param pathExpression
     * @return
     */
    public static FunctionExpression nodes(Expression pathExpression)
    {
        checkNull( pathExpression, "Expression" );

        FunctionExpression function = new FunctionExpression();
        function.name = "nodes";
        function.expression = pathExpression;
        return function;
    }

    /**
     * Declare a relationships function. Corresponds to:
     * <pre>
     *     relationships(path)
     * </pre>
     * @param pathExpression
     * @return
     */
    public static FunctionExpression relationships(Expression pathExpression)
    {
        checkNull( pathExpression, "Expression" );

        FunctionExpression function = new FunctionExpression();
        function.name = "relationships";
        function.expression = pathExpression;
        return function;
    }

    /**
     * Declare a tail function. Corresponds to:
     * <pre>
     *     tail(path)
     * </pre>
     * @param pathExpression
     * @return
     */
    public static FunctionExpression tail(Expression pathExpression)
    {
        checkNull( pathExpression, "Expression" );

        FunctionExpression function = new FunctionExpression();
        function.name = "tail";
        function.expression = pathExpression;
        return function;
    }

    /**
     * Declare an extract function. Corresponds to:
     * <pre>
     *     extract(name IN iterable : expression)
     * </pre>
     * @param iterable
     * @param expression
     * @return
     */
    public static Extract extract( String name, Expression iterable, Expression expression )
    {
        return extract( identifier( name ), iterable, expression );
    }

    /**
     * Declare an extract function. Corresponds to:
     * <pre>
     *     extract(name IN iterable : expression)
     * </pre>
     * @param iterable
     * @param expression
     * @return
     */
    public static Extract extract( Identifier name, Expression iterable, Expression expression )
    {
        Query.checkNull( name, "Name" );
        Query.checkNull( iterable, "Iterable" );
        Query.checkNull( expression, "Expression" );

        Extract extract = new Extract();
        extract.name = name;
        extract.iterable = iterable;
        extract.expression = expression;
        return extract;
    }

    /**
     * Declare a filter function. Corresponds to:
     * <pre>
     *     filter(name IN iterable : predicate)
     * </pre>
     * @param iterable
     * @param predicateExpression
     * @return
     */
    public static FunctionExpression.Filter filter( String name, Expression iterable, PredicateExpression predicateExpression )
    {
        return filter( identifier( name ), iterable, predicateExpression );
    }

    /**
     * Declare a filter function. Corresponds to:
     * <pre>
     *     filter(name IN iterable : predicate)
     * </pre>
     * @param iterable
     * @param predicateExpression
     * @return
     */
    public static FunctionExpression.Filter filter( Identifier name, Expression iterable, PredicateExpression predicateExpression )
    {
        Query.checkNull( name, "Name" );
        Query.checkNull( iterable, "Iterable" );
        Query.checkNull( predicateExpression, "Predicate" );

        FunctionExpression.Filter filter = new FunctionExpression.Filter();
        filter.name = name;
        filter.iterable = iterable;
        filter.predicate = predicateExpression;
        return filter;
    }

    // Mathematical functions
    /**
     * Declare an abs function. Corresponds to:
     * <pre>
     *     abs(expression)
     * </pre>
     *
     * @param numericalExpression
     * @return
     */
    public static FunctionExpression abs(Expression numericalExpression)
    {
        FunctionExpression function = new FunctionExpression();
        function.name = "abs";
        function.expression = numericalExpression;
        return function;
    }

    /**
     * Declare a round function. Corresponds to:
     * <pre>
     *     round(expression)
     * </pre>
     *
     * @param numericalExpression
     * @return
     */
    public static FunctionExpression round(Expression numericalExpression)
    {
        FunctionExpression function = new FunctionExpression();
        function.name = "round";
        function.expression = numericalExpression;
        return function;
    }

    /**
     * Declare a sqrt function. Corresponds to:
     * <pre>
     *     sqrt(expression)
     * </pre>
     *
     * @param numericalExpression
     * @return
     */
    public static FunctionExpression sqrt(Expression numericalExpression)
    {
        FunctionExpression function = new FunctionExpression();
        function.name = "sqrt";
        function.expression = numericalExpression;
        return function;
    }

    /**
     * Declare a sign function. Corresponds to:
     * <pre>
     *     sign(expression)
     * </pre>
     *
     * @param numericalExpression
     * @return
     */
    public static FunctionExpression sign(Expression numericalExpression)
    {
        FunctionExpression function = new FunctionExpression();
        function.name = "sign";
        function.expression = numericalExpression;
        return function;
    }

    @Override
    public String toString()
    {
        return query.toString();
    }

    // Grammar
    protected class Grammar
        implements Match, ReturnNext, OrderBy, Skip, Limit, Execute
    {
        // Match --------------------------------------------------------
        @Override
        public Match match( MatchExpression... expression )
        {
            Collections.addAll(query.matchExpressions, expression);
            return this;
        }

        @Override
        public Match match(Iterable<MatchExpression> expressions)
        {
            for (MatchExpression expression : expressions)
            {
                query.matchExpressions.add(expression);
            }
            return this;
        }

        // Where --------------------------------------------------------
        @Override
        public Where where( PredicateExpression expression )
        {
            Query.checkNull( expression, "Expression" );
            query.whereExpressions.add( expression );
            return this;
        }

        // Return -------------------------------------------------------
        @Override
        public ReturnNext returns( Expression... returnExpressions )
        {
            for( Expression expression : returnExpressions )
            {
                if (expression instanceof ReturnExpression<?>)
                    query.returnExpressions.add( (ReturnExpression) expression);
                else
                    query.returnExpressions.add( exp( expression ) );
            }
            return this;
        }

        @Override
        public ReturnNext returns(Iterable<Expression> returnExpressions)
        {
            for( Expression expression : returnExpressions )
            {
                if (expression instanceof ReturnExpression<?>)
                    query.returnExpressions.add( (ReturnExpression) expression);
                else
                    query.returnExpressions.add( exp( expression ) );
            }
            return this;
        }

        // OrderBy ------------------------------------------------------
        @Override
        public OrderBy orderBy( Expression... orderByExpressions )
        {
            for( Expression byExpression : orderByExpressions )
            {
                if( byExpression instanceof OrderByExpression )
                {
                    query.orderByExpressions.add( (OrderByExpression) byExpression );
                }
                else
                {
                    query.orderByExpressions.add( order( byExpression ) );
                }
            }
            return this;
        }

        @Override
        public OrderBy orderBy(Iterable<Expression> orderByExpressions)
        {
            for( Expression byExpression : orderByExpressions )
            {
                if( byExpression instanceof OrderByExpression )
                {
                    query.orderByExpressions.add( (OrderByExpression) byExpression );
                }
                else
                {
                    query.orderByExpressions.add( order( byExpression ) );
                }
            }
            return this;
        }

        // Skip ---------------------------------------------------------
        @Override
        public Limit skip( int skip )
        {
            if( skip < 0 )
            {
                throw new IllegalArgumentException( "Skip may not be below zero" );
            }

            query.skip = skip;
            return this;
        }

        // Limit --------------------------------------------------------
        @Override
        public Execute limit( int limit )
        {
            if( limit < 0 )
            {
                throw new IllegalArgumentException( "Limit may not be below zero" );
            }

            query.limit = limit;
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
        public ExecuteWithParameters parameter(String name, Object value)
        {
            ExecuteWithParams withParams = new ExecuteWithParams(query);
            return withParams.parameter(name, value);
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

        public ExecuteWithParams(Query query)
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
        public ExecuteWithParameters parameter(String name, Object value)
        {
            parameters.put(name, value);
            return this;
        }

        @Override
        public void asString(StringBuilder builder)
        {
            query.asString(builder);
        }

        @Override
        public String toString()
        {
            return query.toString();
        }
    }
}
