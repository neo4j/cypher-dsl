package org.neo4j.cypherdsl.codegen.sdn6.models.valid.different_properties_for_rel_type;

import javax.annotation.Generated;
import org.neo4j.cypherdsl.core.MapExpression;
import org.neo4j.cypherdsl.core.Node;
import org.neo4j.cypherdsl.core.Properties;
import org.neo4j.cypherdsl.core.Property;
import org.neo4j.cypherdsl.core.RelationshipImpl;
import org.neo4j.cypherdsl.core.SymbolicName;

@Generated(
		value = "org.neo4j.cypherdsl.codegen.core.RelationshipImplBuilder",
		date = "2019-09-21T21:21:00+01:00",
		comments = "This class is generated by the Neo4j Cypher-DSL. All changes to it will be lost after regeneration."
)
public final class ActedInPlay_ extends RelationshipImpl<Person_, Play_, ActedInPlay_> {
	public final Property ROLES = this.property("roles");

	public ActedInPlay_(Person_ start, Play_ end) {
		super(start, "ACTED_IN", end);
	}

	private ActedInPlay_(SymbolicName symbolicName, Node start, String type, Properties properties,
			Node end) {
		super(symbolicName, start, type, properties, end);
	}

	@Override
	public ActedInPlay_ named(SymbolicName newSymbolicName) {
		return new ActedInPlay_(newSymbolicName, getLeft(), getRequiredType(), getDetails().getProperties(), getRight());
	}

	@Override
	public ActedInPlay_ withProperties(MapExpression newProperties) {
		return new ActedInPlay_(getSymbolicName().orElse(null), getLeft(), getRequiredType(), Properties.create(newProperties), getRight());
	}
}