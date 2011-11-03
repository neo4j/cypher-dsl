/*
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

import com.mysema.query.BooleanBuilder;
import com.mysema.query.support.Expressions;
import com.mysema.query.types.Ops;
import com.mysema.query.types.Path;
import org.junit.Assert;
import org.junit.Test;
import org.neo4j.cypherdsl.CypherQuery;

import static com.mysema.query.support.Expressions.*;
import static org.neo4j.cypherdsl.query.ReturnExpression.*;
import static org.neo4j.cypherdsl.query.StartExpression.*;
import static org.neo4j.cypherdsl.query.WhereExpression.*;

/**
 * TODO
 */
public class QueryDSLTest
{
    @Test
    public void testQueryDSL()
    {
        Path<Person> person = Expressions.path(Person.class, "n");
        Path<String> personFirstName = Expressions.path(String.class, person, "firstName");
        Path<Integer> personAge = Expressions.path(Integer.class, person, "age");
        BooleanBuilder expr = new BooleanBuilder(predicate( Ops.EQ_PRIMITIVE, personFirstName, constant( "P" ) )).and( predicate( Ops.GT, personAge, constant( 25 ) ) );

        Assert.assertEquals( "START n=node(1,2,3) WHERE n.firstName=\"P\" and n.age>25 RETURN n",
                             CypherQueryDSL.start( node( "n", 1, 2, 3 ) )
                                 .where( expr )
                                 .returns( nodes( "n" ) )
                                 .toString() );

        Assert.assertEquals( "START n=node(1,2,3) WHERE n.firstName=\"P\" and n.age>25 RETURN n",
                             CypherQuery.start( node( "n", 1, 2, 3 ) )
                                 .where( string( "n.firstName" ).eq( "P" ).and( number("n.age").gt( 25 ) ) )
                                 .returns( nodes( "n" ) )
                                 .toString() );


    }

    class Person
    {
        String firstName;
        int age;
    }
}
