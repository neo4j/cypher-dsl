/*
 * Copyright (c) 2019-2025 "Neo4j,"
 * Neo4j Sweden AB [https://neo4j.com]
 *
 * This file is part of Neo4j.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.neo4j.cypherdsl.examples.ogm.books;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.neo4j.cypherdsl.core.Cypher;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * @author Michael J. Simons
 */
class ScopingTests {

	@ParameterizedTest // GH-1014
	@ValueSource(booleans = { true, false })
	void patternExpressionMustNotIntroduceNames(boolean withIt) {

		var userIdParam = Cypher.parameter("userId");
		var limitParam = Cypher.parameter("limit");
		var langParam = Cypher.parameter("lang");

		var book = Book_.BOOK.withProperties(Book_.BOOK.LANG, langParam).named("book");

		var userDetails = UserDetails_.USER_DETAILS.withProperties(UserDetails_.USER_DETAILS.ID, userIdParam)
			.named("user");
		var userPreferences = UserPreferences_.USER_PREFERENCES
			.withProperties(UserPreferences_.USER_PREFERENCES.USER_ID, userIdParam)
			.named("preferences");
		var genre = BookGenre_.BOOK_GENRE.named("genre");
		var userActivity = UserSuggestionActivity_.USER_SUGGESTION_ACTIVITY
			.withProperties(UserSuggestionActivity_.USER_SUGGESTION_ACTIVITY.USER_ID, userIdParam)
			.named("activity");

		var relationBookGenre = book.relationshipTo(genre, "BELONGS_TO").named("rel");

		var relationBookAvoidedGenre = book.relationshipTo(BookGenre_.BOOK_GENRE, "BELONGS_TO")
			.relationshipFrom(userPreferences, "AVOIDED");

		var relationUserPreferencesGenreBook = userDetails.relationshipTo(userPreferences, "HAS_PREFERENCES")
			.relationshipTo(genre, "PREFERRED")
			.relationshipFrom(book, "BELONGS_TO");

		var returningBooks = Cypher.call("distinct").withArgs(Cypher.name("book")).asFunction();

		// This is the correct variant, as shown in the latest code update on the issue,
		// matching the activity first, then
		// passing it on.
		if (withIt) {
			var statement = Cypher.match(relationUserPreferencesGenreBook)
				.match(userDetails.relationshipTo(userActivity, "HAS_ACTIVITY"))
				.where(Cypher.not(Cypher.exists(relationBookAvoidedGenre)))
				.with(book, userActivity)
				.limit(limitParam)
				.match(relationBookGenre)
				.where(Cypher.not(Cypher.exists(book.relationshipBetween(userActivity))))
				.returning(returningBooks, Cypher.collect(Cypher.name("rel")), Cypher.collect(genre))
				.build();

			assertThat(statement.getCypher()).isEqualTo(
					"MATCH (user:`UserDetails` {id: $userId})-[:`HAS_PREFERENCES`]->(preferences:`UserPreferences` {userId: $userId})-[:`PREFERRED`]->(genre:`BookGenre`)<-[:`BELONGS_TO`]-(book:`Book` {lang: $lang}) MATCH (user)-[:`HAS_ACTIVITY`]->(activity:`UserSuggestionActivity` {userId: $userId}) WHERE NOT (exists((book)-[:`BELONGS_TO`]->(:`BookGenre`)<-[:`AVOIDED`]-(preferences))) WITH book, activity LIMIT $limit MATCH (book)-[rel:`BELONGS_TO`]->(genre:`BookGenre`) WHERE NOT (exists((book)--(activity))) RETURN distinct(book), collect(rel), collect(genre)");

		}
		else {
			// Statement is identical, except the with clause. activity goes out of scope,
			// hence in the existential subquery it will be rerendered.
			// There however it must not include the name a new
			var statement = Cypher.match(relationUserPreferencesGenreBook)
				.match(userDetails.relationshipTo(userActivity, "HAS_ACTIVITY"))
				.where(Cypher.not(Cypher.exists(relationBookAvoidedGenre)))
				.with(book)
				.limit(limitParam)
				.match(relationBookGenre)
				.where(Cypher.not(Cypher.exists(book.relationshipBetween(userActivity))))
				.returning(returningBooks, Cypher.collect(Cypher.name("rel")), Cypher.collect(genre))
				.build();
			assertThat(statement.getCypher()).isEqualTo(
					"MATCH (user:`UserDetails` {id: $userId})-[:`HAS_PREFERENCES`]->(preferences:`UserPreferences` {userId: $userId})-[:`PREFERRED`]->(genre:`BookGenre`)<-[:`BELONGS_TO`]-(book:`Book` {lang: $lang}) MATCH (user)-[:`HAS_ACTIVITY`]->(activity:`UserSuggestionActivity` {userId: $userId}) WHERE NOT (exists((book)-[:`BELONGS_TO`]->(:`BookGenre`)<-[:`AVOIDED`]-(preferences))) WITH book LIMIT $limit MATCH (book)-[rel:`BELONGS_TO`]->(genre:`BookGenre`) WHERE NOT (exists((book)--(:`UserSuggestionActivity` {userId: $userId}))) RETURN distinct(book), collect(rel), collect(genre)");
		}
	}

}
