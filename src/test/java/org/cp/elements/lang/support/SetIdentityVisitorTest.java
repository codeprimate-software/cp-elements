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

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.IdentifierSequence;
import org.cp.elements.lang.Visitable;
import org.junit.Test;

/**
 * Unit tests for {@link SetIdentityVisitor}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Identifiable
 * @see org.cp.elements.lang.IdentifierSequence
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.support.SetIdentityVisitor
 * @since 1.0.0
 */
public class SetIdentityVisitorTest {

  private SetIdentityVisitor visitor = new SetIdentityVisitor();

  @Test
  public void constructDefaultSetIdentifyVisitor() {

    assertThat(visitor).isNotNull();
    assertThat(visitor.getIdentifierSequence()).isInstanceOf(UUIDIdentifierSequence.class);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void constructInitializedSetIdentityVisitor() {

    IdentifierSequence<Long> mockIdentifierSequence = mock(IdentifierSequence.class);

    SetIdentityVisitor<Long> visitor = new SetIdentityVisitor<>(mockIdentifierSequence);

    assertThat(visitor).isNotNull();
    assertThat(visitor.getIdentifierSequence()).isSameAs(mockIdentifierSequence);
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructSetIdentityVisitorWithNull() {

    try {
      new SetIdentityVisitor<>(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("The IdentifierSequence cannot be null");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void visitIdentifiableVisitable() {

    IdentifiableVisitable mockIdentifiableVisitable = mock(IdentifiableVisitable.class);

    SetIdentityVisitor<Long> visitor = new SetIdentityVisitor<>(new SimpleIdentifierSequence());

    visitor.visit(mockIdentifiableVisitable);

    verify(mockIdentifiableVisitable, times(1)).setId(eq(1L));
  }

  @Test
  public void visitNonIdentifiableVisitable() {

    Visitable mockVisitable = mock(Visitable.class);

    SetIdentityVisitor<?> visitor = new SetIdentityVisitor<>();

    visitor.visit(mockVisitable);
  }

  @Test
  public void visitNull() {
    this.visitor.visit(null);
  }

  protected interface IdentifiableVisitable extends Identifiable<Long>, Visitable { }

}
