/*
 * Copyright 2016 Author or Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */
package org.cp.elements.beans.support;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import org.cp.elements.beans.event.ChangeRecorder;
import org.cp.elements.lang.Visitable;
import org.junit.Test;

/**
 * Unit Tests for {@link ClearDirtyStateVisitor}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.beans.support.ClearDirtyStateVisitor
 * @since 1.0.0
 */
public class ClearDirtyStateVisitorUnitTests {

  @Test
  public void clearsChangeRecorder() {

    ChangeRecorder mockChangeRecorder = mock(ChangeRecorder.class);

    ClearDirtyStateVisitor.INSTANCE.visit(mockChangeRecorder);

    verify(mockChangeRecorder, times(1)).clear();
    verifyNoMoreInteractions(mockChangeRecorder);
  }

  @Test
  public void visitsVisitable() {

    Visitable mockVisitable = mock(Visitable.class);

    ClearDirtyStateVisitor.INSTANCE.visit(mockVisitable);

    verifyNoInteractions(mockVisitable);
  }

  @Test
  public void visitIsNullSafe() {
    ClearDirtyStateVisitor.INSTANCE.visit(null);
  }
}
