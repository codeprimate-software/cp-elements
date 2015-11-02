/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang.support;

import static org.junit.Assert.assertEquals;

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
 * The ClearIdentityVisitorTest class is a test suite fo the test cases testing the contract and functionality
 * of the ClearIdentityVisitor class.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.support.ClearIdentityVisitor
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.junit.Test
 * @since 1.0.0
 */
public class ClearIdentityVisitorTest extends AbstractMockingTestSuite {

  private ClearIdentityVisitor visitor = new ClearIdentityVisitor();

  @Test
  public void testVisitIdentifiableVisitable() {
    final IdentifiableVisitable mockIdentifiableVisitable = mock(IdentifiableVisitable.class, "testIdentifiableVisitable");

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
          description.appendText("Visiting a mock IdentifiableVisitable object with an instance of ClearIdentityVisitor!");
        }
      });
      oneOf(mockIdentifiableVisitable).setId(null);
    }});

    mockIdentifiableVisitable.accept(visitor);
  }

  @Test
  public void testVisitNonIdentifiableVisitable() {
    IdentifierVisitable identifier = new IdentifierVisitable();

    assertEquals(1l, identifier.id.longValue());

    identifier.accept(visitor);

    assertEquals(1l, identifier.id.longValue());
  }

  @Test
  public void testVisitNull() {
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
