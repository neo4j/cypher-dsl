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

import com.mysema.query.types.Expression;
import com.mysema.query.types.Projections;
import com.mysema.query.types.QBean;
import com.mysema.query.types.path.PathBuilder;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.neo4j.cypher.javacompat.ExecutionEngine;
import org.neo4j.cypher.javacompat.ExecutionResult;
import org.neo4j.cypherdsl.Execute;

/**
 * TODO
 */
public class Projection
{
    private ExecutionEngine engine;

    public Projection( ExecutionEngine engine )
    {
        this.engine = engine;
    }

    public <T> Iterable<T> iterable(Execute query, Class<T> targetClass)
    {
        return iterable( query, Collections.<String,Object>emptyMap(), targetClass );
    }

    public <T> Iterable<T> iterable(Execute query, Map<String, Object> parameters, Class<T> targetClass)
    {
        PathBuilder<T> friend = new PathBuilder<T>( targetClass, "entity" );
        Field[] fields = targetClass.getFields();
        Expression[] fieldExpressions = new Expression[fields.length];
        for( int i = 0; i < fields.length; i++ )
        {
            fieldExpressions[i] = friend.getString( fields[ i ].getName() );
        }

        QBean<T> bean = Projections.fields( targetClass, fieldExpressions );
        ExecutionResult result = engine.execute( query.toString() , parameters );
        List<T> friends = new ArrayList<T>(  );
        for( Map<String, Object> stringObjectMap : result )
        {
            Object[] args = new Object[stringObjectMap.size()];
            int idx = 0;
            for( Object object : stringObjectMap.values() )
            {
                args[idx++] = object;
            }
            friends.add( bean.newInstance( args ) );
        }

        return friends;
    }
}
