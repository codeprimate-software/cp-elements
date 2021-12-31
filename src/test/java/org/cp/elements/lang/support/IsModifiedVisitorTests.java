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
import static org.mockito.Mockito.mock;

import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.Visitor;
import org.cp.elements.lang.annotation.NotNull;
import org.junit.Test;

/**
 * Unit Tests for {@link IsModifiedVisitor}.
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

    assertThat(visitor.isModified()).isFalse();

    visitor.visit(new AuditableVisitable(false));

    assertThat(visitor.isModified()).isFalse();

    visitor.visit(new AuditableVisitable(true));

    assertThat(visitor.isModified()).isTrue();
    assertThat(visitor.isModified()).isTrue();
  }

  @Test
  public void visitMultipleAuditableObjects() {

    IsModifiedVisitor visitor = new IsModifiedVisitor();

    assertThat(visitor.isModified()).isFalse();

    new AuditableVisitable(false, new AuditableVisitable(true, new AuditableVisitable(false)))
      .accept(visitor);

    assertThat(visitor.isModified()).isTrue();
    assertThat(visitor.isModified()).isTrue();
  }

  @Test
  public void visitNonAuditableObject() {

    IsModifiedVisitor visitor = new IsModifiedVisitor();

    assertThat(visitor.isModified()).isFalse();

    visitor.visit(mock(Visitable.class));

    assertThat(visitor.isModified()).isFalse();
  }

  @SuppressWarnings("rawtypes")
  protected static final class AuditableVisitable extends AbstractAuditable implements Visitable {

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
      return this.modified;
    }

    @Override
    public void accept(@NotNull Visitor visitor) {

      visitor.visit(this);

      if (this.visitable != null) {
        this.visitable.accept(visitor);
      }
    }
  }
}
