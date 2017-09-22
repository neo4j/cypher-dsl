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
package org.neo4j.cypherdsl;

import static org.neo4j.cypherdsl.CypherQuery.identifier;
import static org.neo4j.cypherdsl.query.Query.checkNull;

import java.util.Arrays;
import java.util.regex.Pattern;

import org.neo4j.cypherdsl.expression.NumericExpression;
import org.neo4j.cypherdsl.expression.ReferenceExpression;
import org.neo4j.cypherdsl.expression.StringExpression;
import org.neo4j.cypherdsl.query.AbstractExpression;
import org.neo4j.cypherdsl.query.Value;

/**
 * Represents an identifier. If the identifier is a node or relationship,
 * then you can use the property() method to get a representation of individual properties.
 */
public class Identifier
        extends Value
        implements ReferenceExpression
{
    private static final Pattern simpleName = Pattern.compile( "\\p{Alpha}\\w*" );

    Identifier( String name )
    {
        super( new IdentifierExpression( name ) );
    }

    Identifier ( Iterable<String> names, String prefix )
    {
        super( new IdentifierExpression(names, prefix) );
    }

    /**
     * If this identifier represents a node or relationship,
     * then you can use this method to denote a property.
     * <p/>
     * Corresponds to:
     * <pre>
     * id.name
     * </pre>
     *
     * @param name
     * @return
     */
    public Property property( String name )
    {
        return property( identifier( name ) );
    }

    /**
     * If this identifier represents a node or relationship,
     * then you can use this method to denote a property.
     * <p/>
     * Corresponds to:
     * <pre>
     * id.name
     * </pre>
     *
     * @param name
     * @return
     */
    public Property property( Identifier name )
    {
        checkNull( name, "Name" );
        return new Property( this, name);
    }

    /**
     * If this identifier represents a node,
     * then you can use this method to denote a label.
     * <p/>
     * Corresponds to:
     * <pre>
     * id:label
     * </pre>
     *
     * @param label
     * @return
     */
    public LabelReference label( String label )
    {
        return label(identifier(label));
    }

    /**
     * If this identifier represents a node,
     * then you can use this method to denote labels.
     * <p/>
     * Corresponds to:
     * <pre>
     * id:label1:label2
     * </pre>
     *
     * @param labels
     * @return
     */
    public LabelReference labels( String... labels )
    {
        checkNull( labels, "Labels" );
        return label(new Identifier( Arrays.asList(labels), ":"));
    }

    /**
     * If this identifier represents a node,
     * then you can use this method to denote labels.
     * <p/>
     * Corresponds to:
     * <pre>
     * id:label1:label2
     * </pre>
     *
     * @param labels
     * @return
     */
    public LabelReference labels( Iterable<String> labels )
    {
        checkNull( labels, "Labels" );
        return label(new Identifier( labels, ":"));
    }

    /**
     * If this identifier represents a node,
     * then you can use this method to denote a label.
     * <p/>
     * Corresponds to:
     * <pre>
     * id:label
     * </pre>
     *
     * @param label
     * @return
     */
    public LabelReference label( Identifier label )
    {
        checkNull( label, "Label" );
        return new LabelReference( this, label );
    }

    /**
     * If this identifier represents a node or relationship,
     * then you can use this method to denote a string property.
     * <p/>
     * Corresponds to:
     * <pre>
     * id.name
     * </pre>
     *
     * @param name
     * @return
     */
    public StringExpression string( String name )
    {
        return string( identifier( name ) );
    }

    /**
     * If this identifier represents a node or relationship,
     * then you can use this method to denote a string property.
     * <p/>
     * Corresponds to:
     * <pre>
     * id.name
     * </pre>
     *
     * @param name
     * @return
     */
    public StringExpression string( Identifier name )
    {
        checkNull( name, "Name" );
        return new Property( this, name);
    }

    /**
     * If this identifier represents a node or relationship,
     * then you can use this method to denote a numeric property.
     * <p/>
     * Corresponds to:
     * <pre>
     * id.name
     * </pre>
     *
     * @param name
     * @return
     */
    public NumericExpression number( String name )
    {
        return number( identifier( name ) );
    }

    /**
     * If this identifier represents a node or relationship,
     * then you can use this method to denote a numeric property.
     * <p/>
     * Corresponds to:
     * <pre>
     * id.name
     * </pre>
     *
     * @param name
     * @return
     */
    public NumericExpression number( Identifier name )
    {
        checkNull( name, "Name" );
        return new Property( this, name);
    }

    private static class IdentifierExpression
            extends AbstractExpression
    {
        private final String name;
        private String prefix;

        private IdentifierExpression( String name )
        {
            this.name = name;
        }

        private IdentifierExpression( Iterable<String> names, String prefix )
        {
            StringBuilder nameBuilder = new StringBuilder();
            boolean first = true;
            for ( String name : names )
            {
                if( !first )
                {
                    nameBuilder.append( prefix );
                }

                if ( simpleName.matcher( name ).matches() )
                {
                    nameBuilder.append( name );
                }
                else
                {
                    nameBuilder.append( '`' ).append( name ).append( '`' );
                }
                first = false;
            }
            this.prefix = prefix;
            this.name = nameBuilder.toString();
        }

        public void asString( StringBuilder builder )
        {
            if ( prefix != null || simpleName.matcher( name ).matches() )
            {
                builder.append( name );
            }
            else
            {
                builder.append( '`' ).append( name ).append( '`' );
            }
        }
    }
}
