/*
 * Copyright 2016 Author or Authors.
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

import org.cp.elements.lang.Destroyable;
import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.Visitor;

/**
 * The DestroyableVisitor class is a Visitor implementation calling destroy on Destroyable objects in an object
 * graph/hierarchy.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.Destroyable
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.Visitor
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class DestroyableVisitor implements Visitor {

  /**
   * Visits all Destroyable objects in a graph invoking the destroy method.
   *
   * @param visitable the Visitable object visited by this Visitor.
   * @see org.cp.elements.lang.Destroyable#destroy()
   * @see org.cp.elements.lang.Visitable
   */
  @Override
  public void visit(final Visitable visitable) {
    if (visitable instanceof Destroyable) {
      ((Destroyable) visitable).destroy();
    }
  }

}
