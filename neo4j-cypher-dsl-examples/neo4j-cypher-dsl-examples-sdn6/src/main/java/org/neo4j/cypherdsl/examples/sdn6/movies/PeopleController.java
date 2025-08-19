/*
 * Copyright (c) 2019-2024 "Neo4j,"
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
package org.neo4j.cypherdsl.examples.sdn6.movies;

import java.util.Optional;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.server.ResponseStatusException;

/**
 * Example controller.
 *
 * @author Michael J. Simons
 */
@RestController
@RequestMapping("/api/people")
public class PeopleController {

	private final PeopleService peopleService;

	public PeopleController(PeopleService peopleService) {
		this.peopleService = peopleService;
	}

	@GetMapping("/details/{name}")
	public PersonDetails getDetails(@PathVariable String name) {

		return this.peopleService.findDetails(name)
			.orElseThrow(() -> new ResponseStatusException(HttpStatus.NOT_FOUND));
	}

	@GetMapping("/findPeopleBornInThe70tiesOr")
	public Iterable<Person> findPeopleBornInThe70tiesOr(@RequestParam(name = "name") Optional<String> optionalName) {

		return this.peopleService.findPeopleBornInThe70tiesOr(optionalName);
	}

	@GetMapping("/v1/findPeopleBornAfterThe70ties")
	public Iterable<Person> findPeopleBornAfterThe70ties(
			@RequestParam(name = "conditions") String additionalConditions) {

		return this.peopleService.findPeopleBornAfterThe70tiesAnd(additionalConditions);
	}

	@GetMapping("/v2/findPeopleBornAfterThe70ties")
	public Iterable<Person> findPeopleBornAfterThe70tiesV2(
			@RequestParam(name = "conditions") String additionalConditions) {

		return this.peopleService.findPeopleBornAfterThe70tiesAndV2(additionalConditions);
	}

	@PostMapping("/createNewPerson")
	public Person createNewPerson(@RequestBody NewPersonCmd newPersonCmd) {

		return this.peopleService.createNewPerson(newPersonCmd)
			.orElseThrow(() -> new ResponseStatusException(HttpStatus.NOT_FOUND));
	}

}
