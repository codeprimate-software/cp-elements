/*
 * Copyright 2011-Present Author or Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.cp.elements.lang.support;

import org.cp.elements.lang.Interruptable;
import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.Visitor;

/**
 * The InterruptableVisitor class is a {@link Visitor} implementation that visits and object graph and interrupts
 * any {@link Interruptable} objects in the graph visited.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.Interruptable
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.Visitor
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class InterruptableVisitor implements Visitor {

  /**
   * Visits {@link Visitable} objects in a object graph in search of {@link Interruptable} objects
   * in order to interrupt them.
   *
   * @param visitable the Visitable object visited by this Visitor.
   * @see org.cp.elements.lang.Visitable
   */
  @Override
  public void visit(final Visitable visitable) {
    if (visitable instanceof Interruptable) {
      ((Interruptable) visitable).interrupt();
    }
  }

}
