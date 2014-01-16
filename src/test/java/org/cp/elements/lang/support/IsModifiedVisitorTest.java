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

import static org.junit.Assert.*;

import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.Visitor;
import org.jmock.Mockery;
import org.jmock.lib.legacy.ClassImposteriser;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * The IsModifiedVisitorTest class is a test suite of test cases testing the contract and functionality of the
 * IsModifiedVisitor class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.Visitor
 * @see org.cp.elements.lang.support.IsModifiedVisitor
 * @see org.jmock.Mockery
 * @see org.jmock.lib.legacy.ClassImposteriser
 * @see org.junit.Test
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class IsModifiedVisitorTest {

  private Mockery mockContext = new Mockery();

  @Before
  public void setup() {
    mockContext = new Mockery();
    mockContext.setImposteriser(ClassImposteriser.INSTANCE);
  }

  @After
  public void tearDown() {
    mockContext.assertIsSatisfied();
    mockContext = null;
  }

  @Test
  public void testVisitOnAuditableObject() {
    IsModifiedVisitor visitor = new IsModifiedVisitor();

    assertFalse(visitor.isModified());

    visitor.visit(new AuditableVisitable(false));

    assertFalse(visitor.isModified());

    visitor.visit(new AuditableVisitable(true));

    assertTrue(visitor.isModified());
    assertTrue(visitor.isModified());
  }

  @Test
  public void testVisitOnMultipleAuditableObjects() {
    IsModifiedVisitor visitor = new IsModifiedVisitor();

    assertFalse(visitor.isModified());

    (new AuditableVisitable(false, new AuditableVisitable(true, new AuditableVisitable(false)))).accept(visitor);

    assertTrue(visitor.isModified());
    assertTrue(visitor.isModified());
  }

  @Test
  public void testVisitOnNonAuditableObject() {
    IsModifiedVisitor visitor = new IsModifiedVisitor();

    assertFalse(visitor.isModified());

    visitor.visit(mockContext.mock(Visitable.class));

    assertFalse(visitor.isModified());
  }

  protected static final class AuditableVisitable extends AuditableAdapter implements Visitable {

    private final boolean modified;
    private final Visitable visitable;

    public AuditableVisitable() {
      this(false, null);
    }

    public AuditableVisitable(final boolean modified) {
      this(modified, null);
    }

    public AuditableVisitable(final boolean modified, final Visitable visitable) {
      this.modified = modified;
      this.visitable = visitable;
    }

    @Override
    public boolean isModified() {
      return modified;
    }

    @Override
    public void accept(final Visitor visitor) {
      visitor.visit(this);

      if (visitable != null) {
        visitable.accept(visitor);
      }
    }
  }

}
