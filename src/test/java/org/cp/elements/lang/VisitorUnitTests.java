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

import static org.cp.elements.lang.LangExtensions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyNoInteractions;

import org.junit.Test;

/**
 * Unit Tests for {@link Visitor}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Visitor
 * @since 1.0.1
 */
public class VisitorUnitTests {

  @Test
  public void nullSafeVisitorWithNonNullVisitor() {

    Visitor mockVisitor = mock(Visitor.class);

    assertThat(Visitor.nullSafeVisitor(mockVisitor)).isSameAs(mockVisitor);

    verifyNoInteractions(mockVisitor);
  }

  @Test
  public void nullSafeVisitorWithNullVisitor() {

    Visitable mockVisitable = mock(Visitable.class);

    Visitor visitor = Visitor.nullSafeVisitor(null);

    assertThat(visitor).isNotNull();

    visitor.visit(mockVisitable);

    verifyNoInteractions(mockVisitable);
  }
}
