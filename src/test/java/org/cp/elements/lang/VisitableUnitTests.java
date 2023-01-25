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
package org.cp.elements.lang;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import org.junit.Test;

/**
 * Unit Tests for {@link Visitable}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.Visitor
 * @since 1.0.0
 */
public class VisitableUnitTests {

  @Test
  public void visitorVisitsVisitable() {

    Visitable mockVisitable = mock(Visitable.class);

    Visitor mockVisitor = mock(Visitor.class);

    doCallRealMethod().when(mockVisitable).accept(any(Visitor.class));

    mockVisitable.accept(mockVisitor);

    verify(mockVisitable, times(1)).accept(eq(mockVisitor));
    verify(mockVisitor, times(1)).visit(eq(mockVisitable));
    verifyNoMoreInteractions(mockVisitable, mockVisitor);
  }

  @Test
  public void visitableVisitedByNullIsNullSafe() {

    Visitable mockVisitable = mock(Visitable.class);

    mockVisitable.accept(null);

    verify(mockVisitable, times(1)).accept(isNull());
    verifyNoMoreInteractions(mockVisitable);
  }
}
