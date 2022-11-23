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
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.Visitor;
import org.cp.elements.lang.annotation.Nullable;

/**
 * A {@link Visitor} implementation used to walk an application domain object graph after the application domain objects
 * have been persisted to a persistent data store in order to update the persistent state of the application domain
 * objects to committed.
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
   * Default public no-arg constructor used to construct a newm, uninitialized instance of the {@link CommitVisitor}.
   *
   * This instance will target all application domain objects for the {@literal commit} operation.
   *
   * @see #CommitVisitor(Object)
   */
  public CommitVisitor() {
    this(null);
  }

  /**
   * Construct a new instance of {@link CommitVisitor} initialized with the specified {@literal target} {@link Object},
   * used as the subject of the {@literal commit} operation.
   *
   * Only the {@literal target} {@link Object} will be committed, or affected by {@literal this} {@link Visitor}.
   *
   * @param target the target Object of the 'commit' operation.
   */
  public CommitVisitor(Object target) {
    this.target = target;
  }

  /**
   * Visits all {@link Object Objects} in an application domain object (entity) graph hierarchy
   * targeting {@link Object Objects} to be {@literal committed}.
   *
   * @param visitable {@link Object} visited by {@literal this} {@link Visitor}.
   * @see org.cp.elements.lang.Visitable
   * @see #isCommittable(Object)
   */
  @Override
  @SuppressWarnings("rawtypes")
  public void visit(@Nullable Visitable visitable) {

    if (isCommittable(visitable)) {
      ObjectUtils.setField(visitable, "lastModifiedBy", ((Auditable) visitable).getModifiedBy());
      ObjectUtils.setField(visitable, "lastModifiedOn", ((Auditable) visitable).getModifiedOn());
      ObjectUtils.setField(visitable, "lastModifiedWith", ((Auditable) visitable).getModifiedWith());
    }
  }

  /**
   * Determines whether the specified {@link Visitable} {@link Object} is committable.
   *
   * The {@link Object} is committable if the {@link Object} is {@link Auditable} and {@literal this} {@link Visitor}
   * is not targeting a specific {@link Object} in the application domain object (entity) graph hierarchy.
   *
   * @param visitable {@link Object} visited and being evaluated for commit.
   * @return a boolean value indicating whether the {@literal targeted} {@link Object} can be committed.
   * @see org.cp.elements.lang.Auditable
   */
  protected boolean isCommittable(@Nullable Object visitable) {

    return visitable instanceof Auditable
      && (this.target == null || identity(visitable) == identity(this.target));
  }

  private int identity(@Nullable Object obj) {
    return System.identityHashCode(obj);
  }
}
