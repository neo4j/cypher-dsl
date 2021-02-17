package org.neo4j.cypherdsl.core.renderer;

import org.neo4j.cypherdsl.core.support.Visitor;

/**
 * Internal interface for various implementations of {@link Visitor visitors} that provide a rendered view of the visitable
 * they have been used with.
 *
 * @author Michael J. Simons
 */
interface RenderingVisitor extends Visitor {

	/**
	 * @return Renderer content after this visitor has been accepted by a {@link org.neo4j.cypherdsl.core.support.Visitable}.
	 */
	String getRenderedContent();
}
