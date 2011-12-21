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
import com.mysema.query.types.ParamExpression;
import com.mysema.query.types.Path;
import com.mysema.query.types.expr.Param;
import org.junit.Assert;
import org.junit.Test;
import org.neo4j.cypherdsl.query.OrderByExpression;
import org.neo4j.cypherdsl.query.ReturnExpression;
import org.neo4j.cypherdsl.query.StartExpression;

import static com.mysema.query.alias.Alias.$;
import static com.mysema.query.alias.Alias.alias;
import static com.mysema.query.support.Expressions.constant;
import static com.mysema.query.support.Expressions.predicate;
import static org.junit.Assert.assertEquals;
import static org.neo4j.cypherdsl.query.OrderByExpression.property;
import static org.neo4j.cypherdsl.query.OrderByExpression.Order.DESCENDING;
import static org.neo4j.cypherdsl.query.ReturnExpression.count;
import static org.neo4j.cypherdsl.querydsl.CypherQueryDSL.start;
import static org.neo4j.cypherdsl.querydsl.QueryDSLMatchExpression.path;
import static org.neo4j.cypherdsl.querydsl.QueryDSLOrderByExpression.property;
import static org.neo4j.cypherdsl.querydsl.QueryDSLReturnExpression.properties;
import static org.neo4j.cypherdsl.querydsl.QueryDSLStartExpression.*;

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
            BooleanBuilder expr = new BooleanBuilder(predicate(Ops.EQ_PRIMITIVE, personFirstName, constant("P"))).and(predicate(Ops.GT, personAge, constant(25)));

            Assert.assertEquals("START n=node(1,2,3) WHERE n.firstName=\"P\" and n.age>25 RETURN n",
                    start(node(person, 1, 2, 3))
                            .where(expr)
                            .returns(QueryDSLReturnExpression.nodes(person))
                            .toString());
        }

        {
            Person person = alias(Person.class, "n");
            Assert.assertEquals("START n=node(1,2,3) WHERE n.firstName=\"P\" and n.age>25 RETURN n",
                    start(StartExpression.node(person.toString(), 1, 2, 3))
                            .where($(person.getFirstName()).eq("P").and($(person.getAge()).gt(25)))
                            .returns(ReturnExpression.nodes(person.toString()))
                            .toString());
        }

        {
            QPerson person = QPerson.person;
            Assert.assertEquals("START person=node(1,2,3) WHERE person.firstName=\"P\" and person.age>25 RETURN person",
                    start(node(person, 1, 2, 3))
                            .where(person.firstName.eq("P").and(person.age.gt(25)))
                            .returns(QueryDSLReturnExpression.nodes(person))
                            .toString());
        }

        {
            QPerson person = QPerson.person;
            Assert.assertEquals("START person=node:node_auto_index(\"firstName:rickard\") RETURN person.firstName ORDER BY person.firstName DESCENDING",
                    start(query(person, "node_auto_index", person.firstName.eq("Rickard")))
                            .returns(properties(person.firstName))
                            .orderBy(property(person.firstName, OrderByExpression.Order.DESCENDING))
                            .toString());
        }

        {
            Assert.assertEquals("START person=node:node_auto_index(\"firstName:rickard\") RETURN person.firstName ORDER BY person.firstName DESCENDING",
                    new CypherQueryDSL()
                    {{
                            QPerson person = QPerson.person;
                            starts(query(person, "node_auto_index", person.firstName.eq("Rickard")))
                                    .returns(properties(person.firstName))
                                    .orderBy(property(person.firstName, OrderByExpression.Order.DESCENDING));
                        }}
                            .toString());
        }

        {
            QPerson n = new QPerson("n");
            Assert.assertEquals("START n=node(1,2,3) WHERE n.firstName=\"P\" and n.age>25 RETURN n",
                    start(node(n, 1, 2, 3))
                            .where(n.firstName.eq("P").and(n.age.gt(25)))
                            .returns(QueryDSLReturnExpression.nodes(n))
                            .toString());
        }

        {
            QPerson n = new QPerson("n");
            Assert.assertEquals("START n=node(1,2,3) WHERE n.firstName={name} and n.age>{age} RETURN n",
                    start(node(n, 1, 2, 3))
                            .where(n.firstName.eq(new Param<String>(String.class, "name")).and(n.age.gt(new Param<Integer>(Integer.class, "age"))))
                            .returns(QueryDSLReturnExpression.nodes(n))
                            .toString());
        }
    }

    @Test
    public void testCookbookExample()
    {
        // Static method style
        QStuff stuff = QStuff.stuff;
        QPlace place = QPlace.place;
        QPerson person = QPerson.person;
        assertEquals("START place=node:node_auto_index(name=\"CoffeShop1\") MATCH (place)<-[:favorite]-(person)-[:favorite]->(stuff) RETURN stuff.name,count(*) ORDER BY count(*) DESCENDING,stuff.name",
                start(lookup(place, "node_auto_index", place.name, "CoffeShop1")).
                        match(path().from(place).in("favorite").to(person)
                                .link().out("favorite").to(stuff)).
                        returns(properties(stuff.name), count()).
                        orderBy(property("count(*)", DESCENDING), property(stuff.name)).
                        toString());

        // Java instance initialization block style
        assertEquals("START place=node:node_auto_index(name=\"CoffeShop1\") MATCH (place)<-[:favorite]-(person)-[:favorite]->(stuff) RETURN stuff.name,count(*) ORDER BY count(*) DESCENDING,stuff.name",
                new CypherQueryDSL()
                {{
                    QStuff stuff = QStuff.stuff;
                    QPlace place = QPlace.place;
                    QPerson person = QPerson.person;

                    starts(lookup(place, "node_auto_index", place.name, "CoffeShop1")).
                    match(path().from(place).in("favorite").to(person)
                            .link().out("favorite").to(stuff)).
                    returns(properties(stuff.name), count()).
                    orderBy(property("count(*)", DESCENDING), property(stuff.name));
                }}.toString());

    }

}
