package org.neo4j.cypherdsl.codegen.sdn6.models.same_rel_different_package.domain;

import org.neo4j.cypherdsl.codegen.sdn6.models.same_rel_different_package.application.PlaceModel;

import org.springframework.data.neo4j.core.schema.Node;
import org.springframework.data.neo4j.core.schema.Relationship;

@Node(primaryLabel = "Company", labels = { "CompanyId" })
public class Company {

	@Relationship(type = "IN", direction = Relationship.Direction.OUTGOING)
	private Place place;

	@Relationship(type = "USES", direction = Relationship.Direction.OUTGOING)
	private DomainEntity domainEntity;

	@Relationship(type = "USES", direction = Relationship.Direction.OUTGOING)
	private DomainEntity domainEntity2;

	@Relationship(type = "ABUSES", direction = Relationship.Direction.OUTGOING)
	private DomainEntity de1;

	@Relationship(type = "ABUSES", direction = Relationship.Direction.OUTGOING)
	private DomainEntity2 de2;

}
