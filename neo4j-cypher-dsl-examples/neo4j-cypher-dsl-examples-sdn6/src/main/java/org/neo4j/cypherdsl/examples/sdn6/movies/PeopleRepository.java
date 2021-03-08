package org.neo4j.cypherdsl.examples.sdn6.movies;

import java.util.Optional;

import org.springframework.data.neo4j.repository.Neo4jRepository;
import org.springframework.data.neo4j.repository.query.Query;

/**
 * @author Michael J. Simons
 */
interface PeopleRepository extends Neo4jRepository<Person, Long> {

	@Query("MATCH (person:Person {name: $name})\n"
		   + "\t\tOPTIONAL MATCH (person)-[:DIRECTED]->(d:Movie)\n"
		   + "\t\tOPTIONAL MATCH (person)<-[r:ACTED_IN]->(a:Movie)\n"
		   + "\t\tOPTIONAL MATCH (person)-->(movies)<-[relatedRole:ACTED_IN]-(relatedPerson)\t\t\n"
		   + "\t\tRETURN DISTINCT person,\n"
		   + "\t\tcollect(DISTINCT d) AS directed,\n"
		   + "\t\tcollect(DISTINCT a) AS actedIn,\n"
		   + "\t\tcollect(DISTINCT relatedPerson) AS related")
	Optional<PersonDetails> getDetailsByName(String name);
}
