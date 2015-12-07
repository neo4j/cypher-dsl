/**
 * Copyright (c) 2002-2015 "Neo Technology,"
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

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.querydsl.core.types.Expression;
import com.querydsl.core.types.Path;
import com.querydsl.core.types.Projections;
import com.querydsl.core.types.QBean; 
import com.querydsl.core.types.dsl.PathBuilder;

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


    public Iterable<T> iterable( Iterable<Map<String, Object>> result )
    {
        List<T> entities = new ArrayList<T>();

        for ( Map<String, Object> stringObjectMap : result )
        {
            Object[] args = new Object[stringObjectMap.size()];
            int idx = 0;
            for ( Expression<?> expression : bean.getArgs() )
            {
                args[idx++] = stringObjectMap.get( ((Path) expression).getMetadata().getElement().toString() );
            }

            entities.add( bean.newInstance( args ) );
        }

        return entities;
    }
}
