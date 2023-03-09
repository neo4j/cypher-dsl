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
package org.neo4j.cypherdsl.core.blerg;

import java.util.ArrayDeque;
import java.util.Deque;

import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.internal.ReflectiveVisitor;

public class Wurstsalat implements Visitor {


	Deque<MutableTree<String>> nodes = new ArrayDeque<>();

	MutableTree<String> root;


	@Override
	public void enter(Visitable segment) {
		var currentParent = nodes.peek();
		if(currentParent == null) {
			currentParent = MutableTree.root(segment.toString());
		} else {
			currentParent = currentParent.append(segment.toString());
		}

		nodes.push(currentParent);
	}

	@Override
	public void leave(Visitable segment) {
		root = nodes.pop();
	}


	public static void main(String... a) {
		var stment = Cypher.match(Cypher.node("Movie").named("n"))
			.returning(Cypher.name("n").property("f"))
			.build();
		var visitor = new Wurstsalat();
		System.out.println(stment.getCypher());
		stment.accept(visitor);

		//visitor.root.bfs();
		System.out.println("\n---");

		var root = MutableTree.root(10);
		var n2 = root.append(2);
		//System.out.println(n2.parent());
		var n34 = root.append(34);
		var n56= root.append(56);
		var n100 = root.append(100);
		var n77 = root.append(77);
		var n88 = n2.append(88);
		var n1 = n56.append(1);
		n1.append(23);
		var n7 = n100.append( 7);
		var n8 = n100.append( 8);
		var n9 = n100.append( 9);
		// root.bfs();
		System.out.println("---");
		// root.dfs();
	}

	/*

		public Iterator<MutableTree<E>> bfs() {
		Queue<MutableTree<E>> q = new ArrayDeque<>();
		q.add(this);
		var currentLevel = 0;
		while (!q.isEmpty()) {
			var n = q.remove();
			q.addAll(n.children);

			if (currentLevel != n.level) {
				System.out.println();
				currentLevel = n.level;
			}
			System.out.print(n.value + "(" + n.level + ") ");
		}
		System.out.println();
		return null;
	}

public void dfs() {

		Deque<Iterator<MutableTree<E>>> s = new ArrayDeque<>();
		s.push(List.of(this).iterator());
		while (!s.isEmpty()) {
			var f = s.peek();
			var n = f.next();


			if (!f.hasNext()) {
				s.pop();
			}
			if (!n.children.isEmpty()) {
				s.push(n.children.iterator());
			}
			System.out.println(n.value + " (" + n.level + ")");
		}
	}


	 */



}

