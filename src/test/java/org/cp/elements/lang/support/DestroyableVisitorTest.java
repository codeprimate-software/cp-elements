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

import org.cp.elements.lang.Destroyable;
import org.cp.elements.lang.Visitable;
import org.cp.elements.test.AbstractMockingTestSuite;
import org.jmock.Expectations;
import org.junit.Test;

/**
 * The DestroyableVisitorTest class is a test suite of test cases testing the contract and functionality of the
 * DestroyableVisitor class.
 * <p/>
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
