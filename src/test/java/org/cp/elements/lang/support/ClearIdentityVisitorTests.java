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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.Visitor;
import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link ClearIdentityVisitor}.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Identifiable
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.Visitor
 * @see org.cp.elements.lang.support.ClearIdentityVisitor
 * @since 1.0.0
 */
public class ClearIdentityVisitorTests {

  private ClearIdentityVisitor visitor = new ClearIdentityVisitor();

  @Test
  public void visitIdentifiableVisitable() {

    IdentifiableVisitable mockIdentifiableVisitable = mock(IdentifiableVisitable.class);

    doAnswer((invocationOnMock) -> {
        invocationOnMock.<Visitor>getArgument(0).visit(mockIdentifiableVisitable);
        return null;
      }
    ).when(mockIdentifiableVisitable).accept(any(ClearIdentityVisitor.class));

    mockIdentifiableVisitable.accept(visitor);

    verify(mockIdentifiableVisitable, times(1)).setId(isNull());
  }

  @Test
  @SuppressWarnings("all")
  public void visitNonIdentifiableVisitable() {

    IdentifierVisitable identifier = new IdentifierVisitable();

    assertThat(identifier.id.longValue()).isEqualTo(1L);

    identifier.accept(visitor);

    assertThat(identifier.id.longValue()).isEqualTo(1L);
  }

  @Test
  public void visitNull() {
    this.visitor.visit(null);
  }

  public interface IdentifiableVisitable extends Identifiable<Long>, Visitable { }

  protected static class IdentifierVisitable implements Visitable {

    private Long id = 1L;

    @Override
    public void accept(final Visitor visitor) {
      visitor.visit(this);
    }
  }
}
