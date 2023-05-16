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

import org.cp.elements.lang.Interruptable;
import org.cp.elements.lang.Visitable;
import org.junit.jupiter.api.Test;

/**
 * The InterruptableVisitorTest class is a test suite of test cases testing the contract and functionality
 * of the InterruptableVisitor class.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.support.InterruptableVisitor
 * @since 1.0.0
 */
public class InterruptableVisitorTest {

  private InterruptableVisitor visitor = new InterruptableVisitor();

  @Test
  public void visitInterruptableObjectInvokesInterrupt() {
    InterruptableVisitable mockInterruptable = mock(InterruptableVisitable.class);

    visitor.visit(mockInterruptable);

    verify(mockInterruptable, times(1)).interrupt();
  }

  @Test
  public void visitNonInterruptableObjectWithNoConsequences() {
    visitor.visit(mock(Visitable.class));
  }

  @Test
  public void visitNullWithNoConsequences() {
    visitor.visit(null);
  }

  interface InterruptableVisitable extends Interruptable, Visitable {
  }

}
