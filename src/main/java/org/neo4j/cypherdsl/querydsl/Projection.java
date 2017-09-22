/**
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

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.querydsl.core.types.Expression;
import com.querydsl.core.types.Path;
import com.querydsl.core.types.Projections;
import com.querydsl.core.types.QBean; 
import com.querydsl.core.types.dsl.PathBuilder;
import org.neo4j.graphdb.Result;

/**
 * Projection is responsible for converting the results of a query into an iterable of instances
 * of a given class.
 */
public class Projection<T>
{
    private final QBean<T> bean;

    public Projection( Class<T> targetClass )
    {
        PathBuilder<T> entity = new PathBuilder<T>( targetClass, "entity" );
        Field[] fields = targetClass.getFields();
        Expression[] fieldExpressions = new Expression[fields.length];
        for ( int i = 0; i < fields.length; i++ )
        {
            fieldExpressions[i] = entity.getString( fields[i].getName() );
        }

        bean = Projections.fields( targetClass, fieldExpressions );
    }


    public Iterable<T> iterable( Iterator<Map<String, Object>> result )
    {
        List<T> entities = new ArrayList<T>();

        while (result.hasNext())
        {
            Map<String, Object> row = result.next();
            Object[] args = new Object[row.size()];
            int idx = 0;
            for ( Expression<?> expression : bean.getArgs() )
            {
                args[idx++] = row.get( ((Path) expression).getMetadata().getElement().toString() );
            }

            entities.add( bean.newInstance( args ) );
        }

        return entities;
    }
}
