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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.isNull;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.Visitor;
import org.junit.Test;

/**
 * Test suite of test cases testing the contract and functionality of the {@link ClearIdentityVisitor} class.
 *
 * @author John J. Blum
 * @see org.junit.Test
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
        invocationOnMock.getArgumentAt(0, Visitor.class).visit(mockIdentifiableVisitable);
        return null;
      }
    ).when(mockIdentifiableVisitable).accept(any(ClearIdentityVisitor.class));

    mockIdentifiableVisitable.accept(visitor);

    verify(mockIdentifiableVisitable, times(1)).setId(isNull(Long.class));
  }

  @Test
  public void visitNonIdentifiableVisitable() {
    IdentifierVisitable identifier = new IdentifierVisitable();

    assertThat(identifier.id.longValue(), is(equalTo(1l)));

    identifier.accept(visitor);

    assertThat(identifier.id.longValue(), is(equalTo(1l)));
  }

  @Test
  public void visitNull() {
    visitor.visit(null);
  }

  public interface IdentifiableVisitable extends Identifiable<Long>, Visitable {
  }

  protected static class IdentifierVisitable implements Visitable {

    private Long id = 1l;

    @Override
    public void accept(final Visitor visitor) {
      visitor.visit(this);
    }
  }
}
