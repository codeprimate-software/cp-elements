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
import org.cp.elements.test.AbstractMockingTestSuite;
import org.jmock.Expectations;
import org.junit.Test;

/**
 * The DestroyableVisitorTest class is a test suite of test cases testing the contract and functionality of the
 * DestroyableVisitor class.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.support.DestroyableVisitor
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.junit.Test
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class DestroyableVisitorTest extends AbstractMockingTestSuite {

  @Test
  public void testVisitDestroyable() {
    final VisitableDestroyable mockDestroyable = mockContext.mock(VisitableDestroyable.class);

    mockContext.checking(new Expectations() {{
      oneOf(mockDestroyable).destroy();
    }});

    DestroyableVisitor visitor = new DestroyableVisitor();

    visitor.visit(mockDestroyable);
  }

  @Test
  public void testVisitNonDestroyableVisitable() {
    new DestroyableVisitor().visit(mockContext.mock(Visitable.class));
  }

  protected interface VisitableDestroyable extends Destroyable, Visitable {
  }

}
