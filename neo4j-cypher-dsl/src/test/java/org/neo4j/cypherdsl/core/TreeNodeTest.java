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
package org.neo4j.cypherdsl.core;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

import java.util.NoSuchElementException;

import org.junit.jupiter.api.Test;

/**
 * @author Michael J. Simons
 */
class TreeNodeTest {

	private final static TreeNode<Integer> ROOT;

	static {
		ROOT = TreeNode.root(10);
		var n2 = ROOT.append(2);
		ROOT.append(34);
		var n56 = ROOT.append(56);
		var n100 = ROOT.append(100);
		n2.append(77);
		n2.append(88);
		var n1 = n56.append(1);
		n1.append(23);
		n100.append(7);
		n100.append(8);
		n100.append(9);
	}

	@Test
	void breadthFirstSearchShouldWork() {

		var it = ROOT.breadthFirst();
		assertThat(it)
			.toIterable()
			.extracting(TreeNode::getValue)
			.containsExactly(10, 2, 34, 56, 100, 77, 88, 1, 7, 8, 9, 23);

		assertThatExceptionOfType(NoSuchElementException.class)
			.isThrownBy(it::next);
	}

	@Test
	void preOrderShouldWork() {

		var it = ROOT.preOrder();
		assertThat(it)
			.toIterable()
			.extracting(TreeNode::getValue)
			.containsExactly(10, 2, 77, 88, 34, 56, 1, 23, 100, 7, 8, 9);

		assertThatExceptionOfType(NoSuchElementException.class)
			.isThrownBy(it::next);
	}

	@Test
	void rootsBloodyRoots() {
		assertThat(ROOT.isRoot()).isTrue();
	}

	@Test
	void printShouldWork() {

		var buffer = new StringBuilder();
		ROOT.printTo(buffer::append, node -> node.getValue().toString());
		assertThat(buffer.toString())
			.isEqualTo("""
				└── 10
				    ├── 2
				    │   ├── 77
				    │   └── 88
				    ├── 34
				    ├── 56
				    │   └── 1
				    │       └── 23
				    └── 100
				        ├── 7
				        ├── 8
				        └── 9
				""");
	}
}
