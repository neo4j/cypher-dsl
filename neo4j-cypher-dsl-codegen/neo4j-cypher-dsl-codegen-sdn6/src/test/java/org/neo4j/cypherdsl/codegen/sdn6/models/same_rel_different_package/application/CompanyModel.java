package org.neo4j.cypherdsl.codegen.sdn6.models.same_rel_different_package.application;

import org.neo4j.cypherdsl.codegen.sdn6.models.same_rel_different_package.domain.DomainEntity;

import org.springframework.data.neo4j.core.schema.Node;
import org.springframework.data.neo4j.core.schema.Relationship;

@Node("Company")
public class CompanyModel {

	@Relationship(type = "IN", direction = Relationship.Direction.OUTGOING)
	private PlaceModel place;

	@Relationship(type = "USES", direction = Relationship.Direction.OUTGOING)
	private DomainEntity domainEntity;

}