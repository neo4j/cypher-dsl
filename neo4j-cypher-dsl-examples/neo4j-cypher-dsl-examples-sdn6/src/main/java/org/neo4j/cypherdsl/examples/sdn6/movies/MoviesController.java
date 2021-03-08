package org.neo4j.cypherdsl.examples.sdn6.movies;

import java.util.List;

import javax.print.attribute.standard.MediaSizeName;

import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * @author Michael J. Simons
 */
@RestController
@RequestMapping("/api/movies")
public final class MoviesController {

	private final MovieRepository movieRepository;

	MoviesController(MovieRepository movieRepository) {
		this.movieRepository = movieRepository;
	}

	@GetMapping({ "", "/" })
	public List<Movie> get() {
		// TODO prop -> dingens
		return movieRepository.findAll(Sort.by("title").ascending());
	}
}
