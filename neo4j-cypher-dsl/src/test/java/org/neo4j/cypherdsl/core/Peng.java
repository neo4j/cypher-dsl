package org.neo4j.cypherdsl.core;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.driver.AuthTokens;
import org.neo4j.driver.Driver;
import org.neo4j.driver.GraphDatabase;
import org.neo4j.driver.Session;
import org.neo4j.driver.Transaction;
import org.neo4j.driver.Value;
import org.neo4j.driver.reactive.RxSession;
import org.neo4j.driver.summary.ResultSummary;

// TODO better name
public class Peng {

	public static void main(String... a) {
		Driver driver = GraphDatabase.driver("bolt://localhost:7687", AuthTokens.basic("neo4j", "secret"));
		Node m = Cypher.node("Movie").named("m");
		Statement.ResultQuery queryWithResult = Cypher.match(m).returning(m.property("title")).build();
		Statement queryWithoutResult = Cypher.match(m).set(m.property("title").to(Cypher.literalOf("Terminator")))
			.build();

		ResultSummary rr = Mono.usingWhen(
			Mono.fromSupplier(driver::rxSession),
			queryWithoutResult::executeWith,
			RxSession::close
		).block();
		System.out.println(rr);

		rr = Mono.usingWhen(
			Mono.fromSupplier(driver::rxSession),
			 s -> Mono.fromDirect(s.writeTransaction(queryWithoutResult::executeWith)),
			RxSession::close
		).block();

		System.out.println(rr);
		if(true)System.exit(0);


		try (Session session = driver.session()) {

			try (Transaction tx = session.beginTransaction()) {
				queryWithResult
					.fetchWith(tx)
					.forEach(System.out::println);
				tx.commit();
			}

			try (Transaction tx = session.beginTransaction()) {
				ResultSummary summary  = queryWithResult
					.streamWith(tx, s -> s.forEach(System.out::println));
				System.out.println(summary);
				tx.commit();
			}

			session.readTransaction(queryWithResult::fetchWith)
				.forEach(System.out::println);

			try (Transaction tx = session.beginTransaction()) {
				ResultSummary summary = queryWithoutResult
					.executeWith(tx);
				System.out.println(summary);
				tx.commit();
			}

			ResultSummary summary = session.writeTransaction(queryWithoutResult::executeWith);
			System.out.println(summary);
		}
	}
}