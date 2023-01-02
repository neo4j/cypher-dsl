/*
 * Copyright (c) 2019-2023 "Neo4j,"
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
package org.neo4j.cypherdsl.core.ast;

/**
 * Sometimes it will  be necessary - for example in dialects - to change the flow of elements visted. Some {@link Visitable vistables}
 * will react on {@link Visitor#enterWithResult(Visitable)} and change course (not all and we don't give any guarantees on any behaviour).
 * This class has been introduced for visitors providing such a behaviour so that an implementation doesn't need to deal
 * with an empty {@link Visitor#enter(Visitable)} method.
 *
 * @author Michael J. Simons
 * @soundtrack Metallica - Ride The Lightning
 * @since 2022.3.0
 */
public abstract class VisitorWithResult implements Visitor {

	@Override
	public final void enter(Visitable segment) {
		enterWithResult(segment);
	}
}
