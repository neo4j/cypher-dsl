/*
 * Copyright (c) 2019-2022 "Neo4j,"
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
package org.neo4j.cypherdsl.core.internal;

import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

import org.junit.jupiter.api.Test;
import org.neo4j.cypherdsl.core.ast.Visitable;

/**
 * @author Michael J. Simons
 */
class ReflectiveVisitorTest {

	static class ThrowingVisitable implements Visitable {
	}

	static class SomeException extends RuntimeException {

		private static final long serialVersionUID = -4170504879699181855L;
	}

	@Test
	void visitorShouldThrowDedicatedException() {
		SomeException cause = new SomeException();
		ReflectiveVisitor visitor = new ReflectiveVisitor() {
			@Override
			protected boolean preEnter(Visitable visitable) {
				return true;
			}

			@Override
			protected void postLeave(Visitable visitable) {
			}

			@SuppressWarnings("unused")
			void enter(ThrowingVisitable throwingVisitable) {
				throw cause;
			}
		};

		Visitable v = new ThrowingVisitable();
		assertThatExceptionOfType(HandlerException.class)
			.isThrownBy(() -> visitor.enter(v))
			.withRootCauseInstanceOf(SomeException.class);
	}
}
