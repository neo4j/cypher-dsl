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
package org.neo4j.cypherdsl.codegen.sdn6.models.labels;

import org.springframework.data.neo4j.core.schema.Node;

/**
 * Tests various ways to define labels. The generated classes will end up in a subpackage
 * as they are static, inner classes.
 *
 * @author Michael J. Simons
 */
public class NodesWithDifferentLabelAnnotations {

	@Node("ALabel")
	static class LabelOnNode1 {

	}

	@SuppressWarnings("checkstyle:AnnotationUseStyle")
	@Node(value = "ALabel")
	static class LabelOnNode2 {

	}

	@Node(primaryLabel = "ALabel")
	static class LabelOnNode3 {

	}

	@SuppressWarnings("checkstyle:AnnotationUseStyle")
	@Node(value = { "Label1", "Label2" })
	static class MultipleLabels1 {

	}

	@Node(primaryLabel = "PL", value = { "Label1", "Label2" })
	static class MultipleLabels2 {

	}

	@Node(primaryLabel = "PL", value = { "Label1", "Label2" }, labels = "Some more")
	static class MultipleLabels3 {

	}

}
