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
import java.util.function.Supplier;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Auditable;
import org.cp.elements.lang.Auditor;
import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.Visitor;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * {@link Visitor} implementation used to visit an application domain object graph to set the auditable properties
 * of {@link Auditable} objects in the graph.
 *
 * @author John J. Blum
 * @param <USER> {@link Class type} used to track the user.
 * @param <PROCESS> {@link Class type} used track the process.
 * @see java.time.Instant
 * @see org.cp.elements.lang.Auditable
 * @see org.cp.elements.lang.Auditor
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.Visitor
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AuditableVisitor<USER, PROCESS> implements Visitor {

  private final Auditor<USER, PROCESS> auditor;

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

    this.auditor = newAuditor(user, process, dateTime);
  }

  /**
   * Constructs a new isntance of {@link Auditor} initialized with the {@link USER}, {@link PROCESS}
   * and {@link Instant date and time} used when auditing {@link Auditable} objects.
   *
   * @param user {@link USER} who changed the {@link Auditable} object.
   * @param process {@link PROCESS} that changed the {@link Auditable} object.
   * @param dateTime {@link Instant} when the {@link Auditable} object changed.
   * @return a new {@link Auditor}.
   * @see org.cp.elements.lang.Auditor
   */
  private Auditor<USER, PROCESS> newAuditor(@NotNull USER user, @NotNull PROCESS process, @NotNull Instant dateTime) {

    return new Auditor<USER, PROCESS>() {

      @Override
      public Supplier<Instant> getDateTime() {
        return () -> dateTime;
      }

      @Override
      public Supplier<PROCESS> getProcess() {
        return () -> process;
      }

      @Override
      public Supplier<USER> getUser() {
        return () -> user;
      }
    };
  }

  /**
   * Gets the configured {@link Auditor}.
   *
   * @return the configured {@link Auditor}.
   * @see org.cp.elements.lang.Auditor
   */
  protected @NotNull Auditor<USER, PROCESS> getAuditor() {
    return this.auditor;
  }

  /**
   * Gets the date and time that the {@link Auditable} object was changed.
   *
   * @return {@link Instant} with the date and time the {@link Auditable} object was changed.
   * @see java.time.Instant
   */
  public @NotNull Instant getDateTime() {
    return getAuditor().getDateTime().get();
  }

  /**
   * Gets the process authorized and responsible for changing the {@link Auditable} object.
   *
   * @return the process authorized and responsible for changing the {@link Auditable} object.
   */
  public @NotNull PROCESS getProcess() {
    return getAuditor().getProcess().get();
  }

  /**
   * Gets the user authorized and responsible for changing the {@link Auditable} object.
   *
   * @return the user authorized and responsible for changing the {@link Auditable} object.
   */
  public @NotNull USER getUser() {
    return getAuditor().getUser().get();
  }

  /**
   * Visits the {@link Auditable} objects in an object graph/hierarchy setting auditable properties (created, modified
   * by/date&amp;time/process properties).
   *
   * @param visitable {@link Visitable} object visited by {@literal this} {@link Visitor}.
   * @see org.cp.elements.lang.Auditable
   * @see org.cp.elements.lang.Visitable
   */
  @Override
  public void visit(@Nullable Visitable visitable) {
    getAuditor().audit(visitable);
  }
}
