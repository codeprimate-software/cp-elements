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

import java.time.Instant;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Auditable;
import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.Visitor;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * {@link Visitor} implementation used to visit an application domain object graph to set the auditable properties
 * of {@link Auditable} objects in the graph.
 *
 * @author John J. Blum
 * @see java.time.Instant
 * @see org.cp.elements.lang.Auditable
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.Visitor
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AuditableVisitor<USER, PROCESS> implements Visitor {

  private final Instant dateTime;

  private final PROCESS process;

  private final USER user;

  /**
   * Constructs a new instance of {@link AuditableVisitor} initialized with the creating and modifying user and process.
   *
   * @param user {@link USER} authorized and responsible for changing {@literal this} {@link Auditable} object.
   * @param process {@link PROCESS} authorized and responsible for changing {@literal this} {@link Auditable} object.
   * @throws IllegalArgumentException if the {@link USER} or {@link PROCESS} references are {@literal null}.
   * @see #AuditableVisitor(Object, Object, Instant)
   * @see java.time.Instant#now()
   */
  public AuditableVisitor(@NotNull USER user, @NotNull PROCESS process) {
    this(user, process, Instant.now());
  }

  /**
   * Constructs a new instance of {@link AuditableVisitor} initialized with the creating and modifying user and process
   * as well as the date and time {@literal this} {@link Auditable} object was created or modified.
   *
   * @param user {@link USER} authorized and responsible for changing {@literal this} {@link Auditable} object.
   * @param process {@link PROCESS} authorized and responsible for changing {@literal this} {@link Auditable} object.
   * @param dateTime {@link Instant} indicating the date and time {@literal this} {@link Auditable} object was changed.
   * @throws IllegalArgumentException if the {@link USER} or {@link PROCESS} references are {@literal null}.
   * @see java.time.Instant
   */
  public AuditableVisitor(@NotNull USER user, @NotNull PROCESS process, @Nullable Instant dateTime) {

    Assert.notNull(user, "User is required");
    Assert.notNull(process, "Process is required");

    this.user = user;
    this.process = process;
    this.dateTime = dateTime != null ? dateTime : Instant.now();
  }

  /**
   * Gets the user authorized and responsible for changing the {@link Auditable} object.
   *
   * @return the user authorized and responsible for changing the {@link Auditable} object.
   */
  public @NotNull USER getUser() {
    return this.user;
  }

  /**
   * Gets the process authorized and responsible for changing the {@link Auditable} object.
   *
   * @return the process authorized and responsible for changing the {@link Auditable} object.
   */
  public @NotNull PROCESS getProcess() {
    return this.process;
  }

  /**
   * Gets the date and time that the {@link Auditable} object was changed.
   *
   * @return {@link Instant} with the date and time the {@link Auditable} object was changed.
   * @see java.time.Instant
   */
  public @NotNull Instant getDateTime() {
    return this.dateTime;
  }

  /**
   * Determines whether the primary created {@link Auditable} object properties are currently unset, specifically only
   * evaluating the {@literal createdBy} and {@literal createdOn} properties.
   *
   * @param auditable {@link Auditable} object being evaluated for unset created properties.
   * @return a boolean value indicating whether the {@link Auditable} object's created properties
   * are currently unset.
   * @see org.cp.elements.lang.Auditable#getCreatedBy()
   * @see org.cp.elements.lang.Auditable#getCreatedOn()
   */
  protected boolean isCreatedUnset(@NotNull Auditable<?, ?, ?> auditable) {
    return auditable.getCreatedBy() == null || auditable.getCreatedOn() == null;
  }

  /**
   * Visits the {@link Auditable} objects in an object graph/hierarchy setting auditable properties (created, modified
   * by/date&time/process properties).
   *
   * @param visitable {@link Visitable} object visited by {@literal this} {@link Visitor}.
   * @see org.cp.elements.lang.Auditable
   * @see org.cp.elements.lang.Visitable
   */
  @Override
  @SuppressWarnings("unchecked")
  public void visit(@Nullable Visitable visitable) {

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
