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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.Visitor;
import org.junit.Test;

/**
 * Test suite of test cases testing the contract and functionality of the {@link IsModifiedVisitor} class.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.Visitor
 * @see org.cp.elements.lang.support.IsModifiedVisitor
 * @since 1.0.0
 */
public class IsModifiedVisitorTests {

  @Test
  public void visitAuditableObject() {
    IsModifiedVisitor visitor = new IsModifiedVisitor();

    assertFalse(visitor.isModified());

    visitor.visit(new AuditableVisitable(false));

    assertFalse(visitor.isModified());

    visitor.visit(new AuditableVisitable(true));

    assertTrue(visitor.isModified());
    assertTrue(visitor.isModified());
  }

  @Test
  public void visitMultipleAuditableObjects() {
    IsModifiedVisitor visitor = new IsModifiedVisitor();

    assertFalse(visitor.isModified());

    (new AuditableVisitable(false, new AuditableVisitable(true, new AuditableVisitable(false)))).accept(visitor);

    assertTrue(visitor.isModified());
    assertTrue(visitor.isModified());
  }

  @Test
  public void visitNonAuditableObject() {
    IsModifiedVisitor visitor = new IsModifiedVisitor();

    assertFalse(visitor.isModified());

    visitor.visit(mock(Visitable.class));

    assertFalse(visitor.isModified());
  }

  protected static final class AuditableVisitable extends AuditableAdapter implements Visitable {

    private final boolean modified;
    private final Visitable visitable;

    public AuditableVisitable(boolean modified) {
      this(modified, null);
    }

    public AuditableVisitable(boolean modified, Visitable visitable) {
      this.modified = modified;
      this.visitable = visitable;
    }

    @Override
    public boolean isModified() {
      return modified;
    }

    @Override
    public void accept(Visitor visitor) {
      visitor.visit(this);

      if (visitable != null) {
        visitable.accept(visitor);
      }
    }
  }
}
