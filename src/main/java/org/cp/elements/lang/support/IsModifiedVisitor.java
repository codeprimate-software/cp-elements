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

import org.cp.elements.lang.Auditable;
import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.Visitor;
import org.cp.elements.lang.annotation.Nullable;

/**
 * {@link Visitor} implementation that {@literal visits} a {@link Visitable} object graph/hierarchy
 * in search of any {@link Auditable} objects that may have been modified.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.Auditable
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.Visitor
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class IsModifiedVisitor implements Visitor {

  private volatile boolean modified;

  /**
   * Determines whether any of the {@link Auditable}, {@link Visitable} objects visited were modified.
   *
   * @return a boolean value indicating whether any of the {@link Auditable}, {@link Visitable} objects
   * in the object graph hierarchy were modified.
   */
  public boolean isModified() {
    return this.modified;
  }

  /**
   * Visits all {@link Auditable}, {@link Visitable} objects in an object graph hierarchy in search of
   * any modified objects.
   *
   * @param visitable {@link Visitable} object visited by {@literal this} {@link Visitor}.
   * @see org.cp.elements.lang.Auditable#isModified()
   * @see org.cp.elements.lang.Visitable
   */
  @Override
  @SuppressWarnings("all")
  public void visit(@Nullable Visitable visitable) {

    if (visitable instanceof Auditable) {
      this.modified |= ((Auditable) visitable).isModified();
    }
  }
}
