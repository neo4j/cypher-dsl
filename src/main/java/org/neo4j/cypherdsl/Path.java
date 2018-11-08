/**
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

import static java.util.Arrays.asList;
import static org.neo4j.cypherdsl.CypherQuery.identifier;
import static org.neo4j.cypherdsl.CypherQuery.identifiers;
import static org.neo4j.cypherdsl.query.Direction.BOTH;
import static org.neo4j.cypherdsl.query.Direction.IN;
import static org.neo4j.cypherdsl.query.Direction.OUT;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.neo4j.cypherdsl.expression.Expression;
import org.neo4j.cypherdsl.expression.PathExpression;
import org.neo4j.cypherdsl.query.AbstractExpression;
import org.neo4j.cypherdsl.query.LabelValue;
import org.neo4j.cypherdsl.query.LabelValues;
import org.neo4j.cypherdsl.query.PropertyValue;
import org.neo4j.cypherdsl.query.PropertyValues;

/**
 * Represents either a single node or a path from one node to another.
 */
public class Path
        extends AbstractExpression
        implements PathExpression {
    private final Expression node;
    private final Expression nodePropertyValues;
    private final Expression nodeLabels;
    private final PathRelationship relationship;

    Path(Expression node, PathRelationship relationship, Expression nodePropertyValues, Expression labels) {
        this.node = node;
        this.relationship = relationship;
        this.nodePropertyValues = nodePropertyValues;
        this.nodeLabels = labels;
    }

    public Path labels(LabelValue... labels) {
        return new Path(node, relationship, nodePropertyValues, new LabelValues(asList(labels)));
    }

    public Path labels(Iterable<LabelValue> labels) {
        return new Path(node, relationship, nodePropertyValues, new LabelValues(labels));
    }

    public Path label(String label) {
        return new Path(node, relationship, nodePropertyValues, new LabelValue(identifier(label)));
    }

    public Path label(Enum label) {
        return label(label.name());
    }

    public Path label(Identifier label) {
        return new Path(node, relationship, nodePropertyValues, new LabelValue(label));
    }

    /**
     * If this node is used in a CREATE or CREATE UNIQUE clause,
     * then you can use this method to specify property values.
     * Use e.g. {@link CypherQuery.value( String,Object )} to create
     * the individual values to be passed in here.
     * <p/>
     * Corresponds to:
     * <pre>
     *     (n {prop1:value1,prop2:value2})
     * </pre>
     *
     * @param propertyValues
     * @return
     */
    public Path values(PropertyValue... propertyValues) {
        return new Path(node, relationship, new PropertyValues(asList(propertyValues)), nodeLabels);
    }

    /**
     * If this node is used in a CREATE or CREATE UNIQUE clause,
     * then you can use this method to specify property values.
     * Use e.g. {@link CypherQuery.value( String,Object )} to create
     * the individual values to be passed in here.
     * <p/>
     * Corresponds to:
     * <pre>
     *     (n {prop1:value1,prop2:value2})
     * </pre>
     *
     * @param propertyValues
     * @return
     */
    public Path values(Iterable<PropertyValue> propertyValues) {
        return new Path(node, relationship, new PropertyValues(propertyValues), nodeLabels);
    }

    /**
     * If this node is used in a CREATE or CREATE UNIQUE clause,
     * then you can use this method to specify property values which
     * should be taken from a map parameter.
     * <p/>
     * Corresponds to:
     * <pre>
     *     (n {propertyValues})
     * </pre>
     *
     * @param propertyValues
     * @return
     */
    public Path values(Parameter propertyValues) {
        return new Path(node, relationship, propertyValues, nodeLabels);
    }

    /**
     * Declare a new outgoing relationship from this node.
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)-->(m)
     * </pre>
     *
     * @return
     */
    public PathRelationship out() {
        return new PathRelationship(this, OUT, null, Collections.<Identifier>emptyList(), null, null, null);
    }

    /**
     * Declare a new outgoing relationship from this node.
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)-[:relationship1|relationship2]->(m)
     * </pre>
     *
     * @return
     */
    public PathRelationship out(String... relationships) {
        return new PathRelationship(this, OUT, null, asList(identifiers(relationships)), null, null, null);
    }

    /**
     * Declare a new outgoing relationship from this node.
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)-[:relationship1|relationship2]->(m)
     * </pre>
     *
     * @return
     */
    public PathRelationship out(Identifier... relationships) {
        return new PathRelationship(this, OUT, null, asList(relationships), null, null, null);
    }

    /**
     * Declare a new outgoing relationship from this node.
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)-[:relationship1|relationship2]->(m)
     * </pre>
     *
     * @return
     */
    public PathRelationship out(Enum<?>... relationships) {
        List<Identifier> relationshipNames = new ArrayList<Identifier>();
        for (Enum<?> relationship : relationships) {
            relationshipNames.add(identifier(relationship.name()));
        }

        return new PathRelationship(this, OUT, null, relationshipNames, null, null, null);
    }

    /**
     * Declare a new outgoing relationship from this node.
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)<--(m)
     * </pre>
     *
     * @return
     */
    public PathRelationship in() {
        return new PathRelationship(this, IN, null, Collections.<Identifier>emptyList(), null, null, null);
    }

    /**
     * Declare a new incoming relationship to this node.
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)<-[:relationship1|relationship2]-(m)
     * </pre>
     *
     * @return
     */
    public PathRelationship in(String... relationships) {
        return new PathRelationship(this, IN, null, asList(identifiers(relationships)), null, null, null);
    }

    /**
     * Declare a new incoming relationship to this node.
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)<-[:relationship1|relationship2]-(m)
     * </pre>
     *
     * @return
     */
    public PathRelationship in(Identifier... relationships) {
        return new PathRelationship(this, IN, null, asList(relationships), null, null, null);
    }

    /**
     * Declare a new incoming relationship to this node.
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)<-[:relationship1|relationship2]-(m)
     * </pre>
     *
     * @return
     */
    public PathRelationship in(Enum<?>... relationships) {
        List<Identifier> relationshipNames = new ArrayList<Identifier>();
        for (Enum<?> relationship : relationships) {
            relationshipNames.add(identifier(relationship.name()));
        }

        return new PathRelationship(this, IN, null, relationshipNames, null, null, null);
    }

    /**
     * Declare a new relationship on this node.
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)--(m)
     * </pre>
     *
     * @return
     */
    public PathRelationship both() {
        return new PathRelationship(this, BOTH, null, Collections.<Identifier>emptyList(), null, null, null);
    }

    /**
     * Declare a new relationship on this node.
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)-[:relationship1|relationship2]-(m)
     * </pre>
     *
     * @return
     */
    public PathRelationship both(String... relationships) {
        return new PathRelationship(this, BOTH, null, asList(identifiers(relationships)), null, null,
                null);
    }

    /**
     * Declare a new relationship on this node.
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)-[:relationship1|relationship2]-(m)
     * </pre>
     *
     * @return
     */
    public PathRelationship both(Identifier... relationships) {
        return new PathRelationship(this, BOTH, null, asList(relationships), null, null, null);
    }

    /**
     * Declare a new relationship on this node.
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)-[:relationship1|relationship2]-(m)
     * </pre>
     *
     * @return
     */
    public PathRelationship both(Enum<?>... relationships) {
        List<Identifier> relationshipNames = new ArrayList<Identifier>();
        for (Enum<?> relationship : relationships) {
            relationshipNames.add(identifier(relationship.name()));
        }

        return new PathRelationship(this, BOTH, null, relationshipNames, null, null, null);
    }

    @Override
    public void asString(StringBuilder builder) {
        if (relationship != null) {
            relationship.asString(builder);
        }

        builder.append('(');
        if (node != null) {
            node.asString(builder);
            if (nodeLabels != null) {
                nodeLabels.asString(builder);
            }
            if (nodePropertyValues != null) {
                builder.append(' ');
                nodePropertyValues.asString(builder);
            }
        } else {
            if (nodeLabels != null) {
                nodeLabels.asString(builder);
            }
            if (nodePropertyValues != null) {
                nodePropertyValues.asString(builder);
            }
        }
        builder.append(')');
    }
}
