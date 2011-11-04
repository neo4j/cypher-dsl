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

import com.mysema.query.BooleanBuilder;
import com.mysema.query.support.Expressions;
import com.mysema.query.types.Ops;
import com.mysema.query.types.Path;
import org.junit.Assert;
import org.junit.Test;
import org.neo4j.cypherdsl.CypherQuery;

import static com.mysema.query.alias.Alias.*;
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
        }

        {
            Person person = alias( Person.class, "n" );
            Assert.assertEquals( "START n=node(1,2,3) WHERE n.firstName=\"P\" and n.age>25 RETURN n",
                                 CypherQueryDSL.start( node( "n", 1, 2, 3 ) )
                                     .where( $( person.getFirstName() ).eq( "P" ).and( $( person.getAge() ).gt( 25 ) ) )
                                     .returns( nodes( "n" ) )
                                     .toString() );
        }

        {
            QPerson person = QPerson.person;
            Assert.assertEquals( "START person=node(1,2,3) WHERE person.firstName=\"P\" and person.age>25 RETURN person",
                                 CypherQueryDSL.start( node( "person", 1, 2, 3 ) )
                                     .where( person.firstName.eq( "P" ).and( person.age.gt( 25 ) ) )
                                     .returns( nodes( "person" ) )
                                     .toString() );
        }



        Assert.assertEquals( "START n=node(1,2,3) WHERE n.firstName=\"P\" and n.age>25 RETURN n",
                             CypherQuery.start( node( "n", 1, 2, 3 ) )
                                 .where( prop( "n.firstName" ).eq( "P" ).and( prop( "n.age" ).gt( 25 ) ) )
                                 .returns( nodes( "n" ) )
                                 .toString() );


    }
}
