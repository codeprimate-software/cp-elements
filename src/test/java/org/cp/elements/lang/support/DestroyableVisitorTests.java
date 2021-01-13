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

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.cp.elements.lang.Destroyable;
import org.cp.elements.lang.Visitable;
import org.junit.Test;

/**
 * Test suite of test cases testing the contract and functionality of the {@link DestroyableVisitor} class.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Destroyable
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.support.DestroyableVisitor
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class DestroyableVisitorTests {

  @Test
  public void visitDestroyable() {
    VisitableDestroyable mockVisitableDestroyable = mock(VisitableDestroyable.class);

    DestroyableVisitor visitor = new DestroyableVisitor();

    visitor.visit(mockVisitableDestroyable);

    verify(mockVisitableDestroyable, times(1)).destroy();
  }

  @Test
  public void visitNonDestroyableVisitable() {
    new DestroyableVisitor().visit(mock(Visitable.class));
  }

  @Test
  public void visitNull() {
    new DestroyableVisitor().visit(null);
  }

  protected interface VisitableDestroyable extends Destroyable, Visitable {
  }
}
