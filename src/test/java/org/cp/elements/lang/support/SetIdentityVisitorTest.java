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

import static org.junit.Assert.assertEquals;

import java.util.HashSet;
import java.util.Set;

import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.Visitor;
import org.cp.elements.test.AbstractMockingTestSuite;
import org.hamcrest.Description;
import org.jmock.Expectations;
import org.jmock.api.Action;
import org.jmock.api.Invocation;
import org.junit.Test;

/**
 * The SetIdentityVisitorTest class is a test suite of test cases testing the contract and functionality
 * of the SetIdentityVisitor class.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.support.SetIdentityVisitor
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.junit.Test
 * @since 1.0.0
 */
public class SetIdentityVisitorTest extends AbstractMockingTestSuite {

  private SetIdentityVisitor visitor = new SetIdentityVisitor();

  @Test
  public void testVisitIdentifiableVisitable() {
    final IdentifiableVisitable mockIdentifiableVisitable = mock(IdentifiableVisitable.class,
      "testVisitIdentifiableVisitable");

    checking(new Expectations() {{
      oneOf(mockIdentifiableVisitable).accept(with(equal(visitor)));
      will(new Action() {
        @Override
        public Object invoke(final Invocation invocation) throws Throwable {
          ((Visitor) invocation.getParameter(0)).visit(mockIdentifiableVisitable);
          return null;
        }

        @Override
        public void describeTo(final Description description) {
          description.appendText("Visiting a mock IdentifiableVisitable object with an instance of SetIdentityVisitor!");
        }
      });
      oneOf(mockIdentifiableVisitable).setId(with(aNonNull(Long.class)));
    }});

    mockIdentifiableVisitable.accept(visitor);
  }

  @Test
  public void testVisitNonIdentifiableVisitable() {
    IdentifierVisitable identifierVisitor = new IdentifierVisitable();

    assertEquals(1l, identifierVisitor.id.longValue());

    identifierVisitor.accept(visitor);

    assertEquals(1l, identifierVisitor.id.longValue());
  }

  @Test
  public void testVisitNull() {
    visitor.visit(null);
  }

  @Test
  public void testGetNextSequencedIdentifier() {
    Set<Long> identifiers = new HashSet<>(100);

    synchronized (SetIdentityVisitor.class) {
      for (int count = 100; count > 0; count--) {
        identifiers.add(SetIdentityVisitor.getNextSequencedIdentifier());
      }
    }

    assertEquals(100, identifiers.size());
  }

  protected static class IdentifierVisitable implements Visitable {

    private Long id = 1l;

    @Override
    public void accept(final Visitor visitor) {
      visitor.visit(this);
    }
  }

  protected interface IdentifiableVisitable extends Identifiable<Long>, Visitable {
  }

}
