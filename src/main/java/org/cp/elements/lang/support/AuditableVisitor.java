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

import java.time.LocalDateTime;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Auditable;
import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.Visitor;

/**
 * The AuditableVisitor class is a {@link Visitor} implementation visiting the an object graph
 * to set the auditable properties of an {@link Auditable} object.
 *
 * @author John J. Blum
 * @see java.time.LocalDateTime
 * @see org.cp.elements.lang.Auditable
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.Visitor
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AuditableVisitor<USER, PROCESS> implements Visitor {

  private final LocalDateTime dateTime;

  private final PROCESS process;

  private final USER user;

  /**
   * Constructs an instance of the AuditableVisitor class initialized with a creating/modifying user and process.
   *
   * @param user the user authorized and responsible for changing the Auditable object.
   * @param process the process authorized and responsible for changing the Auditable object.
   * @see #AuditableVisitor(Object, Object, LocalDateTime)
   */
  public AuditableVisitor(USER user, PROCESS process) {
    this(user, process, LocalDateTime.now());
  }

  /**
   * Constructs an instance of the AuditableVisitor class initialized with a creating/modifying user and process
   * as well as the date and time the Auditable object was created or modified.
   *
   * @param user the user authorized and responsible for changing the Auditable object.
   * @param process the process authorized and responsible for changing the Auditable object.
   * @param dateTime a Calendar instance indicating the date and time the Auditable object was changed.
   * @see #AuditableVisitor(Object, Object)
   * @see java.util.Calendar
   */
  public AuditableVisitor(USER user, PROCESS process, LocalDateTime dateTime) {
    Assert.notNull(user, "User must not be null");
    Assert.notNull(process, "Process must not be null");

    this.user = user;
    this.process = process;
    this.dateTime = (dateTime != null ? dateTime : LocalDateTime.now());
  }

  /**
   * Gets the user authorized and responsible for changing the Auditable object.
   *
   * @return the user authorized and responsible for changing the Auditable object.
   */
  public USER getUser() {
    return user;
  }

  /**
   * Gets the process authorized and responsible for changing the Auditable object.
   *
   * @return the process authorized and responsible for changing the Auditable object.
   */
  public PROCESS getProcess() {
    return process;
  }

  /**
   * Gets the date and time that the Auditable object was changed.
   *
   * @return a Calendar instance indicating the date and time the Auditable object was changed.
   * @see java.util.Calendar
   */
  public LocalDateTime getDateTime() {
    return dateTime;
  }

  /**
   * Determines whether the created Auditable properties are currently unset, specifically only evaluated the
   * createdBy and createdDateTime properties.
   *
   * @param auditable the Auditable object being evaluated for unset created properties.
   * @return a boolean value indicating whether the Auditable object's created properties are currently unset.
   * @see org.cp.elements.lang.Auditable#getCreatedBy()
   * @see org.cp.elements.lang.Auditable#getCreatedOn()
   */
  protected boolean isCreatedUnset(Auditable auditable) {
    return (auditable.getCreatedBy() == null || auditable.getCreatedOn() == null);
  }

  /**
   * Visits Auditable objects in an object graph/hierarchy setting auditable information (created/modified
   * by/date-time/process).
   *
   * @param visitable the Visitable object visited by this Visitor.
   * @see org.cp.elements.lang.Auditable
   */
  @Override
  @SuppressWarnings("unchecked")
  public void visit(Visitable visitable) {
    if (visitable instanceof Auditable) {
      Auditable<USER, PROCESS, ?> auditable = (Auditable<USER, PROCESS, ?>) visitable;

      if (auditable.isNew() || isCreatedUnset(auditable)) {
        auditable.setCreatedBy(getUser());
        auditable.setCreatedOn(getDateTime());
        auditable.setCreatedWith(getProcess());
      }

      if (auditable.isModified()) {
        auditable.setModifiedBy(getUser());
        auditable.setModifiedOn(getDateTime());
        auditable.setModifiedWith(getProcess());
      }
    }
  }
}
