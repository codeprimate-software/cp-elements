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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.Visitor;
import org.cp.elements.test.AbstractMockingTestSuite;
import org.junit.Test;

/**
 * The IsModifiedVisitorTest class is a test suite of test cases testing the contract and functionality of the
 * IsModifiedVisitor class.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.Visitor
 * @see org.cp.elements.lang.support.IsModifiedVisitor
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.junit.Test
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class IsModifiedVisitorTest extends AbstractMockingTestSuite {

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
