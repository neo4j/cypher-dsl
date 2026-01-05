/*
 * Copyright (c) 2019-2026 "Neo4j,"
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
package org.neo4j.cypherdsl.codegen.ogm.models.labels;

import org.neo4j.ogm.annotation.NodeEntity;

/**
 * Tests various ways to define labels. The generated classes will end up in a subpackage
 * as they are static, inner classes.
 *
 * @author Michael J. Simons
 */
public class NodesWithDifferentLabelAnnotations {

	@NodeEntity("ALabel")
	static class LabelOnNode1 {

	}

	@SuppressWarnings("checkstyle:AnnotationUseStyle")
	@NodeEntity(value = "ALabel")
	static class LabelOnNode2 {

	}

}
