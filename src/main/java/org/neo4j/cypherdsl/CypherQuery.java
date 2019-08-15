/*
 * Licensed to Neo Technology under one or more contributor
 * license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright
 * ownership. Neo Technology licenses this file to you under
 * the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * <p>
 * http://www.apache.org/licenses/LICENSE-2.0
 * <p>
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.neo4j.cypherdsl;

import org.neo4j.cypherdsl.expression.*;
import org.neo4j.cypherdsl.grammar.*;
import org.neo4j.cypherdsl.query.*;
import org.neo4j.cypherdsl.query.clause.*;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import static org.neo4j.cypherdsl.query.Query.checkEmpty;
import static org.neo4j.cypherdsl.query.Query.checkNull;

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
public class CypherQuery {
    /**
     * Start building a new Cypher query, starting with a START clause
     *
     * @param startExpressions list of start expressions
     * @return Grammar for Match clause
     */
    public static StartNext start(StartExpression... startExpressions) {
        CypherQuery query = new CypherQuery();
        return query.starts(startExpressions);
    }

    /**
     * Start building a new Cypher query, starting with a MATCH clause
     *
     * @param paths
     * @return
     */
    public static StartNext match(PathExpression... paths) {
        CypherQuery query = new CypherQuery();
        return query.matches(paths);
    }

    public static StartNext unwind(Expression values, Identifier id) {
        CypherQuery query = new CypherQuery();
        return query.unwinds(values, id);
    }

    /**
     * Start building a new Cypher query, starting with a CREATE clause
     *
     * @param paths
     * @return
     */
    public static UpdateNext create(PathExpression... paths) {
        CypherQuery query = new CypherQuery();
        return query.creates(paths);
    }

    /**
     * Start building a new Cypher query, starting with a MERGE clause
     *
     * @param paths
     * @return
     */
    public static UpdateNext merge(PathExpression... paths) {
        CypherQuery query = new CypherQuery();
        return query.merges(paths);
    }

    /**
     * Continue building on existing Query object
     *
     * @param query a previously created query object
     * @return CypherQuery DSL that can be used to continue building the query
     */
    public static <T> T continueQuery(Query query, Class<T> asClause)
            throws ClassCastException {
        try {
            return new CypherQuery((Query) query.clone()).continueQuery(asClause);
        } catch (CloneNotSupportedException e) {
            throw new RuntimeException(e);
        }
    }

    // The internal query object. Methods in the DSL work on this
    protected final Query query;

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
    public CypherQuery() {
        query = new Query();
    }

    private CypherQuery(Query query) {
        try {
            this.query = (Query) query.clone();
        } catch (CloneNotSupportedException e) {
            throw new IllegalStateException("Query was not cloneable");
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
    public static Parameter param(String name) {
        checkEmpty(name, "Name");
        return new Parameter(name);
    }

    /**
     * Declare a label.
     *
     * @param label literal value
     * @return Label instance
     */
    public static LabelValue label(String label) {
        return new LabelValue(identifier(label));
    }

    public static LabelValue label(Enum label) {
        return label(label.name());
    }

    /**
     * Declare a label.
     *
     * @param label literal value
     * @return Label instance
     */
    public static LabelValue label(Identifier label) {
        return new LabelValue(label);
    }

    /**
     * Declare a literal string value, such as "Foo".
     *
     * @param value literal value
     * @return Literal instance
     */
    public static StringExpression literal(String value) {
        checkNull(value, "Value");
        return new Literal(value);
    }

    /**
     * Declare a literal numeric value, such 3 or 4.5.
     *
     * @param value literal value
     * @return Literal instance
     */
    public static NumericExpression literal(Number value) {
        checkNull(value, "Value");
        return new Literal(value);
    }

    /**
     * Declare a literal boolean value, such as true or false.
     *
     * @param value literal value
     * @return Literal instance
     */
    public static BooleanExpression literal(boolean value) {
        return new Literal(value);
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
    public static ScalarExpression literal(Object value) {
        checkNull(value, "Value");
        return new Literal(value);
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
    public static Identifier identifier(String name) {
        checkEmpty(name, "Identifier");
        return new Identifier(name);
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
    public static CollectionExpression collection(Object... values) {
        Expression[] expressions = new Expression[values.length];
        for (int i = 0; i < values.length; i++) {
            Object value = values[i];
            expressions[i] = value instanceof Expression ? (Expression) value : literal(value);
        }
        return new Value(new ExpressionCollection(new Expressions(expressions)));
    }

    /**
     * Declare a list of identifiers.
     *
     * @param values
     * @return
     */
    public static Identifier[] identifiers(String... values) {
        Identifier[] identifiers = new Identifier[values.length];
        for (int i = 0; i < values.length; i++) {
            String value = values[i];
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
    public static Parameter[] parameters(String... names) {
        Parameter[] parameters = new Parameter[names.length];
        for (int i = 0; i < names.length; i++) {
            String value = names[i];
            parameters[i] = param(value);
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
    public static NumericExpression[] literals(long... values) {
        NumericExpression[] literals = new NumericExpression[values.length];
        for (int i = 0; i < values.length; i++) {
            long value = values[i];
            literals[i] = literal(value);
        }
        return literals;
    }

    /**
     * Declare a value, which can be used for setting or matching
     * properties in the CREATE or CREATE UNIQUE clauses.
     *
     * @param id
     * @param value
     * @return
     */
    public static PropertyValue value(String id, Object value) {
        checkNull(value, "Value", id);
        return new PropertyValue(identifier(id), literal(value));
    }

    public static PropertyValue value(Enum id, Object value) {
        return value(id.name(), value);
    }

    public static PropertyValue value(Enum id, Enum value) {
        return value(id.name(), value.name());
    }

    /**
     * Declare a value, which can be used for setting or matching
     * properties in the CREATE or CREATE UNIQUE clauses.
     *
     * @param id
     * @param value
     * @return
     */
    public static PropertyValue value(String id, Expression value) {
        return new PropertyValue(identifier(id), value);
    }

    public static PropertyValue value(Enum id, Expression value) {
        return value(id.name(), value);
    }

    /**
     * Declare a value, which can be used for setting or matching
     * properties in the CREATE or CREATE UNIQUE clauses.
     *
     * @param id
     * @param value
     * @return
     */
    public static PropertyValue value(Identifier id, Expression value) {
        return new PropertyValue(id, value);
    }

    /**
     * "and" a series of expressions together.
     *
     * @param expressions
     * @return
     */
    public static BooleanExpression and(BooleanExpression... expressions) {
        Query.checkNull(expressions, "Expressions");
        return new And(expressions);
    }

    /**
     * "or" a series of expressions together.
     *
     * @param expressions
     * @return
     */
    public static BooleanExpression or(BooleanExpression... expressions) {
        Query.checkNull(expressions, "Expressions");
        return new Or(expressions);
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
    public static BooleanExpression not(BooleanExpression expression) {
        Query.checkNull(expression, "Expression");

        return new Value(new FunctionExpression("not", expression));
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
    public static BooleanExpression exists(Property property) {
        return new Value(new FunctionExpression("exists", property));
    }

    /**
     * Corresponds to:
     * <pre>
     * has(expression)
     * </pre>
     *
     * @param expression
     * @return
     */
    public static BooleanExpression exists(Expression expression) {
        return new Value(new FunctionExpression("exists", expression));
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
    public static BooleanExpression isNull(Expression expression) {
        return new Value(new SuffixFunctionExpression(" is null", expression));
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
    public static BooleanExpression isNotNull(Expression expression) {
        return new Value(new SuffixFunctionExpression(" is not null", expression));
    }

    // Start --------------------------------------------------------

    /**
     * START clause. Use this with Java initialization block style.
     *
     * @param startExpressions
     * @return
     */
    protected StartNext starts(StartExpression... startExpressions) {
        query.add(new StartClause(Arrays.asList(startExpressions)));

        return new Grammar();
    }

    /**
     * START clause. Use this with Java initialization block style.
     *
     * @param startExpressions
     * @return
     */
    protected StartNext starts(Iterable<StartExpression> startExpressions) {
        query.add(new StartClause(startExpressions));

        return new Grammar();
    }

    /**
     * CREATE clause. Use this with Java initialization block style.
     *
     * @param paths
     * @return
     */
    protected UpdateNext creates(PathExpression... paths) {
        query.add(new CreateClause(Arrays.asList(paths)));

        return new Grammar();
    }

    /**
     * MERGE clause. Use this with Java initialization block style.
     *
     * @param paths
     * @return
     */
    protected UpdateNext merges(PathExpression... paths) {
        query.add(new MergeClause(Arrays.asList(paths)));

        return new Grammar();
    }

    /**
     * MATCH clause. Use this with Java initialization block style.
     *
     * @param paths
     * @return
     */
    protected StartNext matches(PathExpression... paths) {
        query.add(new MatchClause(Arrays.asList(paths)));

        return new Grammar();
    }

    protected StartNext unwinds(Expression values, Identifier id) {
        query.add(new UnwindClause(values, id));

        return new Grammar();
    }

    protected <T> T continueQuery(Class<T> asClause)
            throws ClassCastException {
        return asClause.cast(new Grammar());
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
    public static StartExpression.StartNodes nodesById(String name, long... id) {
        return nodesById(identifier(name), id);
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
    public static StartExpression.StartNodes nodesById(Identifier name, long... id) {
        checkNull(name, "Name");

        for (long i : id) {
            if (i < 0) {
                throw new IllegalArgumentException("Id may not be below zero");
            }
        }

        return new StartExpression.StartNodes(name, literals(id));
    }

    @Deprecated
    public static StartExpression.StartNodes nodeByParameter(String name, String parameter) {
        return nodesByParameter(name, parameter);
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
    public static StartExpression.StartNodes nodesByParameter(String name, String parameter) {
        return nodesByParameter(identifier(name), parameter);
    }

    @Deprecated
    public static StartExpression.StartNodes nodeByparameter(Identifier name, String parameter) {
        return nodesByParameter(name, parameter);
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
    public static StartExpression.StartNodes nodesByParameter(Identifier name, String parameter) {
        checkEmpty(name, "Name");
        checkEmpty(parameter, "Parameters");

        return new StartExpression.StartNodes(name, parameters(parameter));
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
    public static StartExpression.StartNodes allNodes(String name) {
        return allNodes(identifier(name));
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
    public static StartExpression.StartNodes allNodes(Identifier name) {
        checkNull(name, "Name");

        return new StartExpression.StartNodes(name, new Expression[]{new StartExpression.AllNodes()});
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
    public static StartExpression.StartNodesLookup lookup(String name, String indexName, String key, String value) {
        return lookup(identifier(name), identifier(indexName), identifier(key), literal(value));
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
    public static StartExpression.StartNodesLookup lookup(Identifier name, Identifier indexName,
                                                          ReferenceExpression key, StringExpression value) {
        checkEmpty(name, "Name");
        checkEmpty(indexName, "Index");

        return new StartExpression.StartNodesLookup(name, indexName, key, value);
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
    public static StartExpression.StartNodesQuery query(String name, String indexName, String query) {
        return query(identifier(name), identifier(indexName), query);
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
    public static StartExpression.StartNodesQuery query(Identifier name, Identifier indexName, String query) {
        checkNull(name, "Name");
        checkNull(indexName, "Index");
        checkEmpty(query, "Query");

        return new StartExpression.StartNodesQuery(name, indexName, query);
    }

    /**
     * Declare start nodes. Corresponds to:
     * <pre>
     * name=node:indexName({param}")
     * </pre>
     *
     * @param name
     * @param indexName
     * @param param
     * @return
     */
    public static StartExpression.StartNodesQueryParam queryByParameter(String name, String indexName, String param) {
        return queryByParameter(identifier(name), identifier(indexName), param);
    }

    /**
     * Declare start nodes. Corresponds to:
     * <pre>
     * name=node:indexName({param})
     * </pre>
     *
     * @param name
     * @param indexName
     * @param param
     * @return
     */
    public static StartExpression.StartNodesQueryParam queryByParameter(Identifier name, Identifier indexName,
                                                                        String param) {
        checkNull(name, "Name");
        checkNull(indexName, "Index");
        checkEmpty(param, "Param");
        return new StartExpression.StartNodesQueryParam(name, indexName, param);
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
    public static StartExpression.StartRelationships relationshipsById(String name, long... id) {
        return relationshipsById(identifier(name), id);
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
    public static StartExpression.StartRelationships relationshipsById(Identifier name, long... id) {
        checkNull(name, "Name");

        for (long i : id) {
            if (i < 0) {
                throw new IllegalArgumentException("Id may not be below zero");
            }
        }

        return new StartExpression.StartRelationships(name, literals(id));
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
    public static StartExpression.StartRelationshipsParameters relationshipsByParameter(String name, String parameter) {
        return relationshipsByParameter(identifier(name), parameter);
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
    public static StartExpression.StartRelationshipsParameters relationshipsByParameter(Identifier name,
                                                                                        String parameter
    ) {
        checkNull(name, "Name");
        checkEmpty(parameter, "Parameter");

        return new StartExpression.StartRelationshipsParameters(name, parameter);
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
    public static StartExpression.StartRelationshipsIndex relationshipLookup(String name, String indexName,
                                                                             String key, String value) {
        return relationshipLookup(identifier(name), identifier(indexName), identifier(key), literal(value));
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
    public static StartExpression.StartRelationshipsIndex relationshipLookup(Identifier name, Identifier indexName,
                                                                             Identifier key, StringExpression value) {
        checkNull(name, "Name");
        checkNull(indexName, "Index");
        checkNull(key, "Key");
        checkNull(value, "Value");

        return new StartExpression.StartRelationshipsIndex(name, indexName, key, value);
    }

    // Match --------------------------------------------------------

    /**
     * Start declaring a path for CREATE, CREATE UNIQUE, MATCH or WHERE clauses.
     * <p/>
     * Corresponds to:
     * <pre>
     * ()
     * </pre>
     *
     * @return
     */
    public static Path node() {
        return new Path(null, null, null, null);
    }

    /**
     * Start declaring a path for CREATE, CREATE UNIQUE, MATCH or WHERE clauses.
     * <p/>
     * Corresponds to:
     * <pre>
     * (id)
     * </pre>
     *
     * @param id
     * @return
     */
    public static Path node(String id) {
        return node(identifier(id));
    }

    /**
     * Start declaring a path for CREATE, CREATE UNIQUE, MATCH or WHERE clauses.
     * <p/>
     * Corresponds to:
     * <pre>
     * (expression)
     * </pre>
     *
     * @param expression
     * @return
     */
    public static Path node(Expression expression) {
        return new Path(expression, null, null, null);
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
    public static PathExpression path(String name, PathExpression path) {
        return path(identifier(name), path);
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
    public static PathExpression path(Identifier name, PathExpression path) {
        checkNull(name, "Name");
        return new NamedPath(name, path);
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
    public static PathExpression shortestPath(PathExpression path) {
        Query.checkNull(path, "Path");
        return new Value(new FunctionExpression("shortestPath", path));
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
    public static PathExpression allShortestPaths(PathExpression path) {
        Query.checkNull(path, "Path");

        return new Value(new FunctionExpression("allShortestPaths", path));
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
    public static Expression as(Expression expression, String name) {
        return new Value(new Operator(expression, " AS "), identifier(name));
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
    public static Expression as(Expression expression, Identifier name) {
        return new Value(new Operator(expression, " AS "), name);
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
    public static Expression distinct(Expression expression) {
        return new Value(new Operator("DISTINCT "), expression);
    }

    /**
     * Declare a count(*) RETURN expression
     *
     * @return
     */
    public static NumericExpression count() {
        return new Value(new FunctionExpression("count", new AbstractExpression() {
            @Override
            public void asString(StringBuilder builder) {
                builder.append('*');
            }
        }));
    }

    /**
     * Declare a count(expression) RETURN expression
     *
     * @return
     */
    public static NumericExpression count(Expression expression) {
        checkNull(expression, "Expression");

        return new Value(new FunctionExpression("count", expression));
    }

    /**
     * Declare a * RETURN expression
     */
    public static All all() {
        return new All();
    }

    /**
     * Declare a sum(expression) RETURN expression
     *
     * @return
     */
    public static NumericExpression sum(NumericExpression expression) {
        checkNull(expression, "Expression");
        return new Value(new FunctionExpression("sum", expression));
    }

    /**
     * Declare a avg(expression) RETURN expression
     *
     * @return
     */
    public static NumericExpression avg(Expression expression) {
        checkNull(expression, "Expression");
        return new Value(new FunctionExpression("avg", expression));
    }

    /**
     * Declare a max(expression) RETURN expression
     *
     * @return
     */
    public static NumericExpression max(NumericExpression expression) {
        checkNull(expression, "Expression");
        return new Value(new FunctionExpression("max", expression));
    }

    /**
     * Declare a min(expression) RETURN expression
     *
     * @return
     */
    public static NumericExpression min(NumericExpression expression) {
        checkNull(expression, "Expression");
        return new Value(new FunctionExpression("min", expression));
    }

    /**
     * Declare a collect(expression) RETURN expression
     *
     * @return
     */
    public static CollectionExpression collect(ScalarExpression expression) {
        checkNull(expression, "Expression");
        return new Value(new FunctionExpression("collect", expression));
    }

    // Order by -----------------------------------------------------

    /**
     * Declare an ORDER clause expression. Typically used with identifier("n").property("property") as
     * parameter.
     *
     * @param expression
     * @return
     */
    public static OrderByExpression order(Expression expression) {
        Query.checkNull(expression, "Expression");
        return new OrderByExpression(expression, null);
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
    public static OrderByExpression order(Expression expression, Order order) {
        Query.checkNull(expression, "Name");
        Query.checkNull(order, "Order");
        return new OrderByExpression(expression, order);
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
    public static ForEachStatements in(String id, Expression in) {
        return new ForEachClause(identifier(id), in);
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
    public static ForEachStatements in(Identifier id, Expression in) {
        return new ForEachClause(id, in);
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
    public static SetProperty property(Property property, Expression value) {
        return new SetProperty(property, value);
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
    public static BooleanExpression all(String name, CollectionExpression iterable,
                                        BooleanExpression predicateExpression) {
        return all(identifier(name), iterable, predicateExpression);
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
    public static BooleanExpression all(Identifier name, CollectionExpression iterable,
                                        BooleanExpression predicateExpression) {
        Query.checkNull(name, "Name");
        Query.checkNull(iterable, "Iterable");
        Query.checkNull(predicateExpression, "Predicate");

        return new Value(new IterablePredicateExpression("all", name, iterable, predicateExpression));
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
    public static BooleanExpression any(String name, CollectionExpression iterable,
                                        BooleanExpression predicateExpression) {
        return any(identifier(name), iterable, predicateExpression);
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
    public static BooleanExpression any(Identifier name, CollectionExpression iterable,
                                        BooleanExpression predicateExpression) {
        Query.checkNull(name, "Name");
        Query.checkNull(iterable, "Iterable");
        Query.checkNull(predicateExpression, "Predicate");

        return new Value(new IterablePredicateExpression("any", name, iterable, predicateExpression));
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
    public static BooleanExpression none(String name, CollectionExpression iterable,
                                         BooleanExpression predicateExpression) {
        return none(identifier(name), iterable, predicateExpression);
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
    public static BooleanExpression none(Identifier name, CollectionExpression iterable,
                                         BooleanExpression predicateExpression) {
        Query.checkNull(name, "Name");
        Query.checkNull(iterable, "Iterable");
        Query.checkNull(predicateExpression, "Predicate");

        return new Value(new IterablePredicateExpression("none", name, iterable, predicateExpression));
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
    public static BooleanExpression single(String name, CollectionExpression iterable,
                                           BooleanExpression predicateExpression) {
        return single(identifier(name), iterable, predicateExpression);
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
    public static BooleanExpression single(Identifier name, CollectionExpression iterable,
                                           BooleanExpression predicateExpression) {
        Query.checkNull(name, "Name");
        Query.checkNull(iterable, "Iterable");
        Query.checkNull(predicateExpression, "Predicate");

        return new Value(new IterablePredicateExpression("single", name, iterable, predicateExpression));
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
    public static NumericExpression length(CollectionExpression expression) {
        checkNull(expression, "Expression");
        return new Value(new FunctionExpression("length", expression));
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
    public static StringExpression type(RelationshipExpression relationshipExpression) {
        checkNull(relationshipExpression, "Expression");
        return new Value(new FunctionExpression("type", relationshipExpression));
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
    public static NumericExpression id(String name) {
        checkNull(name, "Name");
        return new Value(new FunctionExpression("id", identifier(name)));
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
    public static NumericExpression id(PropertyContainerExpression propertyContainerExpression) {
        checkNull(propertyContainerExpression, "Expression");
        return new Value(new FunctionExpression("id", propertyContainerExpression));
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
    public static Value coalesce(Expression... expressions) {
        if (expressions.length < 1) {
            throw new IllegalArgumentException("At least one expression must be provided to coalesce function");
        }

        return new Value(new FunctionExpression("coalesce", new Expressions(expressions)));
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
    public static Expression head(CollectionExpression collectionExpression) {
        checkNull(collectionExpression, "Expression");
        return new Value(new FunctionExpression("head", collectionExpression));
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
    public static Expression last(CollectionExpression collectionExpression) {
        checkNull(collectionExpression, "Expression");
        return new Value(new FunctionExpression("last", collectionExpression));
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
    public static CollectionExpression nodes(PathExpression pathExpression) {
        checkNull(pathExpression, "Expression");

        return new Value(new FunctionExpression("nodes", pathExpression));
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
    public static CollectionExpression relationships(PathExpression pathExpression) {
        checkNull(pathExpression, "Expression");

        return new Value(new FunctionExpression("relationships", pathExpression));
    }

    /**
     * Declare a labels expression. Corresponds to:
     * <pre>
     * labels(node)
     * </pre>
     *
     * @param nodeExpression
     * @return
     */
    public static CollectionExpression labels(NodeExpression nodeExpression) {
        checkNull(nodeExpression, "Expression");

        return new Value(new FunctionExpression("labels", nodeExpression));
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
    public static CollectionExpression extract(String name, CollectionExpression iterable,
                                               ScalarExpression expression) {
        return extract(identifier(name), iterable, expression);
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
    public static CollectionExpression extract(Identifier name, CollectionExpression iterable,
                                               ScalarExpression expression) {
        Query.checkNull(name, "Name");
        Query.checkNull(iterable, "Iterable");
        Query.checkNull(expression, "Expression");

        return new Value(new Extract(name, iterable, expression));
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
    public static CollectionExpression filter(String name, CollectionExpression iterable,
                                              BooleanExpression predicateExpression) {
        return filter(identifier(name), iterable, predicateExpression);
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
    public static CollectionExpression filter(Identifier name, CollectionExpression iterable,
                                              BooleanExpression predicateExpression) {
        Query.checkNull(name, "Name");
        Query.checkNull(iterable, "Iterable");
        Query.checkNull(predicateExpression, "Predicate");

        return new Value(new Filter(name, iterable, predicateExpression));
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
    public static CollectionExpression tail(CollectionExpression collectionExpression) {
        checkNull(collectionExpression, "Expression");
        return new Value(new FunctionExpression("tail", collectionExpression));
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
    public static CollectionExpression range(Number start, Number end) {
        return range(literal(start), literal(end), null);
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
    public static CollectionExpression range(Number start, Number end, Number step) {
        return range(literal(start), literal(end), literal(step));
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
    public static CollectionExpression range(NumericExpression start, NumericExpression end) {
        return range(start, end, null);
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
    public static CollectionExpression range(NumericExpression start, NumericExpression end, NumericExpression step) {
        if (step == null) {
            return new Value(new FunctionExpression("range", new Expressions(new Expression[]{start, end})));
        } else {
            return new Value(new FunctionExpression("range", new Expressions(new Expression[]{start, end,
                    step})));
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
    public static NumericExpression p(NumericExpression numericExpression) {
        return new Value(new FunctionExpression("", numericExpression));
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
    public static NumericExpression abs(Number numericalExpression) {
        return abs(literal(numericalExpression));
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
    public static NumericExpression abs(NumericExpression numericalExpression) {
        return new Value(new FunctionExpression("abs", numericalExpression));
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
    public static NumericExpression round(Number numericalExpression) {
        return round(literal(numericalExpression));
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
    public static NumericExpression round(NumericExpression numericalExpression) {
        return new Value(new FunctionExpression("round", numericalExpression));
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
    public static NumericExpression sqrt(Number numericalExpression) {
        return sqrt(literal(numericalExpression));
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
    public static NumericExpression sqrt(NumericExpression numericalExpression) {
        return new Value(new FunctionExpression("sqrt", numericalExpression));
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
    public static NumericExpression sign(Number numericalExpression) {
        return sign(literal(numericalExpression));
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
    public static NumericExpression sign(NumericExpression numericalExpression) {
        return new Value(new FunctionExpression("sign", numericalExpression));
    }

    @Override
    public String toString() {
        return query.toString();
    }

    // Grammar
    protected class Grammar
            implements StartNext, With, WithNext, Create, Set, Delete, Remove, CreateUnique, Merge, UpdateNext, Match, ReturnNext,
            OrderBy, Unwind,
            Skip, Limit, Execute, Union, UnionNext {
        // With ---------------------------------------------------------
        public WithNext with(Expression... withExpressions) {
            query.add(new WithClause(Arrays.asList(withExpressions)));

            return this;
        }

        @Override
        public WithNext with(Iterable<Expression> withExpressions) {
            query.add(new WithClause(withExpressions));

            return this;
        }

        // Union ---------------------------------------------------------
        @Override
        public UnionNext union() {
            query.add(new UnionClause());

            return this;
        }

        @Override
        public UnionNext all() {
            UnionClause unionClause = query.lastClause(UnionClause.class);
            if (unionClause != null) {
                unionClause.all();
            }

            return this;
        }

        // Create -------------------------------------------------------
        @Override
        public UpdateNext create(PathExpression... paths) {
            query.add(new CreateClause(Arrays.asList(paths)));

            return this;
        }

        @Override
        public UpdateNext create(Iterable<PathExpression> paths) {
            query.add(new CreateClause(paths));

            return this;
        }


        // Set ----------------------------------------------------------
        @Override
        public UpdateNext set(SetExpression... setExpressions) {
            query.add(new SetClause(Arrays.asList(setExpressions)));

            return this;
        }

        @Override
        public UpdateNext set(Iterable<SetExpression> setExpressions) {
            query.add(new SetClause(setExpressions));

            return this;
        }


        // On Create ------------------------------------------------------
        @Override
        public UpdateNext onCreate(SetExpression... setExpressions) {
            Clause clause = query.lastClause(Clause.class);
            if (clause instanceof MergeClause || clause instanceof OnMatchClause) {
                query.add(new OnCreateClause(Arrays.asList(setExpressions)));
            } else {
                throw new IllegalArgumentException("ON CREATE must follow a MERGE or ON MATCH Clause");
            }


            return this;
        }

        @Override
        public UpdateNext onCreate(Iterable<SetExpression> setExpressions) {
            Clause clause = query.lastClause(Clause.class);
            if (clause instanceof MergeClause || clause instanceof OnMatchClause) {
                query.add(new OnCreateClause(setExpressions));
            } else {
                throw new IllegalArgumentException("ON CREATE must follow a MERGE or ON MATCH Clause");
            }


            return this;
        }

        // On Match ------------------------------------------------------
        @Override
        public UpdateNext onMatch(SetExpression... setExpressions) {
            Clause clause = query.lastClause(Clause.class);
            if (clause instanceof MergeClause || clause instanceof OnCreateClause) {
                query.add(new OnMatchClause(Arrays.asList(setExpressions)));
            } else {
                throw new IllegalArgumentException("ON MATCH must follow a MERGE or ON CREATE Clause");
            }


            return this;
        }

        @Override
        public UpdateNext onMatch(Iterable<SetExpression> setExpressions) {
            Clause clause = query.lastClause(Clause.class);
            if (clause instanceof MergeClause || clause instanceof OnCreateClause) {
                query.add(new OnMatchClause(setExpressions));
            } else {
                throw new IllegalArgumentException("ON MATCH must follow a MERGE or ON CREATE Clause");
            }


            return this;
        }

        // Delete -------------------------------------------------------
        @Override
        public UpdateNext delete(ReferenceExpression... expressions) {
            query.add(new DeleteClause(Arrays.asList(expressions)));

            return this;
        }

        @Override
        public UpdateNext delete(Iterable<ReferenceExpression> expressions) {
            query.add(new DeleteClause(expressions));

            return this;
        }

        @Override
        public UpdateNext detachDelete(ReferenceExpression... expressions) {
            query.add(new DetachDeleteClause(Arrays.asList(expressions)));

            return this;
        }

        @Override
        public UpdateNext detachDelete(Iterable<ReferenceExpression> expressions) {
            query.add(new DetachDeleteClause(expressions));

            return this;
        }

        // Remove -------------------------------------------------------
        @Override
        public UpdateNext remove(RemoveExpression... expressions) {
            query.add(new RemoveClause(Arrays.asList(expressions)));

            return this;
        }

        @Override
        public UpdateNext remove(Iterable<RemoveExpression> expressions) {
            query.add(new RemoveClause(expressions));

            return this;
        }

        // createUnique -------------------------------------------------------
        @Override
        public UpdateNext createUnique(PathExpression... expressions) {
            query.add(new CreateUniqueClause(Arrays.asList(expressions)));

            return this;
        }

        @Override
        public UpdateNext createUnique(Iterable<PathExpression> expressions) {
            query.add(new CreateUniqueClause(expressions));

            return this;
        }

        // merge -------------------------------------------------------
        @Override
        public UpdateNext merge(PathExpression... expressions) {
            query.add(new MergeClause(Arrays.asList(expressions)));

            return this;
        }

        @Override
        public UpdateNext merge(Iterable<PathExpression> expressions) {
            query.add(new MergeClause(expressions));

            return this;
        }

        // For each -----------------------------------------------------

        @Override
        public UpdateNext forEach(ForEachStatement statement) {
            query.add(statement.getClause());
            return this;
        }

        // Start --------------------------------------------------------
        @Override
        public StartNext starts(StartExpression... startExpression) {
            query.add(new StartClause(Arrays.asList(startExpression)));
            return this;
        }

        @Override
        public StartNext starts(Iterable<StartExpression> startExpression) {
            query.add(new StartClause(startExpression));
            return this;
        }

        // Match --------------------------------------------------------
        @Override
        public Match match(PathExpression... expressions) {
            query.add(new MatchClause(Arrays.asList(expressions)));
            return this;
        }

        @Override
        public Match match(Iterable<PathExpression> expressions) {
            query.add(new MatchClause(expressions));
            return this;
        }

        @Override
        public Match optional() {
            MatchClause matchClause = query.lastClause(MatchClause.class);
            if (matchClause != null) {
                matchClause.optional();
            }
            return this;
        }

        // Where --------------------------------------------------------
        @Override
        public Where where(BooleanExpression expression) {
            Query.checkNull(expression, "Expression");
            query.add(new WhereClause(expression));
            return this;
        }

        // Return -------------------------------------------------------
        @Override
        public ReturnNext returns(Expression... returnExpressions) {
            query.add(new ReturnClause(Arrays.asList(returnExpressions)));
            return this;
        }

        @Override
        public ReturnNext returns(Iterable<Expression> returnExpressions) {
            query.add(new ReturnClause(returnExpressions));
            return this;
        }

        // OrderBy ------------------------------------------------------
        @Override
        public OrderBy orderBy(Expression... orderByExpressions) {
            query.add(new OrderByClause(Arrays.asList(orderByExpressions)));
            return this;
        }

        @Override
        public OrderBy orderBy(Iterable<Expression> orderByExpressions) {
            query.add(new OrderByClause(orderByExpressions));
            return this;
        }

        // Skip ---------------------------------------------------------
        @Override
        public Limit skip(int skip) {
            if (skip < 0) {
                throw new IllegalArgumentException("Skip may not be below zero");
            }

            query.add(new SkipClause(skip));
            return this;
        }

        // Skip ---------------------------------------------------------
        @Override
        public Limit skip(String skip) {
            query.add(new SkipParameterClause(skip));
            return this;
        }

        // Limit --------------------------------------------------------
        @Override
        public Execute limit(int limit) {
            if (limit < 0) {
                throw new IllegalArgumentException("Limit may not be below zero");
            }

            query.add(new LimitClause(limit));
            return this;
        }


        // Limit --------------------------------------------------------
        @Override
        public Execute limit(String limit) {
            query.add(new LimitParameterClause(limit));
            return this;
        }

        // Execute ------------------------------------------------------
        @Override
        public void asString(StringBuilder builder) {
            query.asString(builder);
        }

        @Override
        public Query toQuery() {
            return query;
        }

        @Override
        public ExecuteWithParameters parameter(String name, Object value) {
            return new ExecuteWithParams(query).parameter(name, value);
        }

        @Override
        public ExecuteWithParameters parameters(Map<String, Object> parameters) {
            return new ExecuteWithParams(query).parameters(parameters);
        }

        @Override
        public String toString() {
            return CypherQuery.this.toString();
        }

        @Override
        public UpdateNext unwind(Expression values, Identifier identifier) {
            query.add(new UnwindClause(values, identifier));
            return this;
        }
    }

    protected class ExecuteWithParams
            implements ExecuteWithParameters {
        private final Query query;
        private final Map<String, Object> parameters = new HashMap<>();

        public ExecuteWithParams(Query query) {
            this.query = query;
        }

        @Override
        public Query toQuery() {
            return query;
        }

        public Map<String, Object> getParameters() {
            return parameters;
        }

        @Override
        public ExecuteWithParameters parameter(String name, Object value) {
            this.parameters.put(name, value);
            return this;
        }

        @Override
        public ExecuteWithParameters parameters(Map<String, Object> parameters) {
            this.parameters.putAll(parameters);
            return this;
        }

        @Override
        public void asString(StringBuilder builder) {
            query.asString(builder);
        }

        @Override
        public String toString() {
            return query.toString();
        }
    }


    public static class And
            extends Value {
        public And(BooleanExpression[] value) {
            super(new Expressions(value));
        }

        @Override
        public void asString(StringBuilder builder) {
            Expressions expressions = (Expressions) value;

            for (int i = 0; i < expressions.expressions.length; i++) {
                Expression expression = expressions.expressions[i];
                if (i > 0) {
                    builder.append(" and ");
                }
                if (expression instanceof And || expression instanceof Or) {
                    builder.append('(');
                    expression.asString(builder);
                    builder.append(')');
                } else {
                    expression.asString(builder);
                }
            }
        }
    }

    public static class Or
            extends Value {
        public Or(BooleanExpression[] value) {
            super(new Expressions(value));
        }

        @Override
        public void asString(StringBuilder builder) {
            Expressions expressions = (Expressions) value;

            for (int i = 0; i < expressions.expressions.length; i++) {
                Expression expression = expressions.expressions[i];
                if (i > 0) {
                    builder.append(" or ");
                }
                if (expression instanceof And) {
                    builder.append('(');
                    expression.asString(builder);
                    builder.append(')');
                } else {
                    expression.asString(builder);
                }
            }
        }
    }
}
