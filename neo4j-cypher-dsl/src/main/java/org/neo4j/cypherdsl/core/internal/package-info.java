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
/**
 * This is basically the kitchen sink for all classes that must be public but are not actually part of the public API.
 * We will change them at will and don't give any guarantees about them. All classes inside that package are marked as
 * {@link org.apiguardian.api.API.Status#INTERNAL}. The {@code internal} package won't be exported when the Cypher-DSL
 * is run on the module path.
 *
 * @author Michael J. Simons
 */
package org.neo4j.cypherdsl.core.internal;
