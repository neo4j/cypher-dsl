/*
 * Copyright (c) "Neo4j"
 * Neo4j Sweden AB [http://neo4j.com]
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

		return peopleService.findDetails(name)
			.orElseThrow(() -> new ResponseStatusException(HttpStatus.NOT_FOUND));
	}

	@GetMapping("/findPeopleBornInThe70tiesOr")
	public Iterable<Person> findPeopleBornInThe70tiesOr(@RequestParam(name = "name") Optional<String> optionalName) {

		return peopleService.findPeopleBornInThe70tiesOr(optionalName);
	}

	@GetMapping("/v1/findPeopleBornAfterThe70ties")
	public Iterable<Person> findPeopleBornAfterThe70ties(@RequestParam(name = "conditions") String additionalConditions) {

		return peopleService.findPeopleBornAfterThe70tiesAnd(additionalConditions);
	}

	@GetMapping("/v2/findPeopleBornAfterThe70ties")
	public Iterable<Person> findPeopleBornAfterThe70tiesV2(@RequestParam(name = "conditions") String additionalConditions) {

		return peopleService.findPeopleBornAfterThe70tiesAndV2(additionalConditions);
	}

	@PostMapping("/createNewPerson")
	public Person createNewPerson(@RequestBody NewPersonCmd newPersonCmd) {

		return peopleService.createNewPerson(newPersonCmd)
			.orElseThrow(() -> new ResponseStatusException(HttpStatus.NOT_FOUND));
	}
}
