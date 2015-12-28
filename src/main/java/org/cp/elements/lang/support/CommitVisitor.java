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

import org.cp.elements.lang.Auditable;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.Visitor;

/**
 * The CommitVisitor class is a Visitor implementation used to walk an application domain object graph hierarchy
 * after the application domain objects have been persisted to a persistent data store in order to update
 * the persistent state of the application domain to committed.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.Auditable
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.Visitor
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class CommitVisitor implements Visitor {

  private final Object target;

  /**
   * Default public no-arg constructor constructing an uninitialized instance of the CommitVisitor.  This instance
   * will target all application domain objects for the 'commit' operation.
   */
  public CommitVisitor() {
    this(null);
  }

  /**
   * Constructs an instance of the CommitVisitor initialized with the specified target object, which will be
   * the subject of the 'commit' operation.  Only the target object will be committed, or affected by this Visitor.
   *
   * @param target the target Object of the 'commit' operation.
   */
  public CommitVisitor(final Object target) {
    this.target = target;
  }

  /**
   * Visits all objects in an application domain object graph hierarchy targeting objects to be 'committed'.
   *
   * @param visitable the object visited by this Visitor.
   * @see org.cp.elements.lang.Visitable
   * @see #isCommitable(Object)
   */
  @Override
  public void visit(final Visitable visitable) {
    if (isCommitable(visitable)) {
      ObjectUtils.setField(visitable, "lastModifiedBy", ((Auditable) visitable).getModifiedBy());
      ObjectUtils.setField(visitable, "lastModifiedDateTime", ((Auditable) visitable).getModifiedDateTime());
      ObjectUtils.setField(visitable, "lastModifyingProcess", ((Auditable) visitable).getModifyingProcess());
    }
  }

  /**
   * Determines whether the specified visitable object is commit-able.  The object is commit-able if the object
   * is Auditable and this Visitor is not targeting a specific object in the application domain object graph
   * hierarchy.
   *
   * @param visitable the visited object being evaluated for commit-ability.
   * @return a boolean value indicating whether the targeted object can be committed.
   * @see org.cp.elements.lang.Auditable
   */
  protected boolean isCommitable(final Object visitable) {
    return (visitable instanceof Auditable && (target == null || identity(visitable) == identity(target)));
  }

  /* (non-Javadoc) */
  private int identity(final Object obj) {
    return System.identityHashCode(obj);
  }

}
