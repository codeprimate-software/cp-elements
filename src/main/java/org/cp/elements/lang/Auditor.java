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
package org.cp.elements.lang;

import java.time.Instant;
import java.util.function.Supplier;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Interface defining a contract for an {@link Auditor} capable of {@literal auditing} an {@link Auditable object}.
 *
 * @author John Blum
 * @param <USER> {@link Class type} of the user used by the application for auditing.
 * @param <PROCESS> {@link Class type} of the process used by the application for auditing.
 * @see org.cp.elements.lang.Auditable
 * @see java.time.Instant
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Auditor<USER, PROCESS> {

  /**
   * Gets the date and time (timestamp) used to mark an {@link Auditable object} when it changed.
   *
   * @return a new {@link Instant}.
   * @see java.util.function.Supplier
   * @see java.time.Instant
   */
  default @NotNull Supplier<Instant> getDateTime() {
    return Instant::now;
  }

  /**
   * Gets the process (program) responsible for changing the {@link Auditable object}.
   * <p>
   * Returns {@literal null} by default.
   *
   * @return the process (program) responsible for changing the {@link Auditable object}.
   * @see java.util.function.Supplier
   */
  default @NotNull Supplier<PROCESS> getProcess() {
    return () -> null;
  }

  /**
   * Gets the user responsible for changing the {@link Auditable object}.
   * <p>
   * By default, the {@literal user.name} {@link System#getProperties() System property} is used
   * to identify the user.
   *
   * @return a {@link String} containing the {@literal username}, as determined by the {@literal user.name}
   * {@link System#getProperties() System property}, of the user responsible for changing the {@link Auditable object}.
   * @see java.lang.System#getProperty(String)
   * @see java.util.function.Supplier
   */
  @SuppressWarnings("unchecked")
  default @NotNull Supplier<USER> getUser() {
    return () -> (USER) System.getProperty("user.name");
  }

  /**
   * Tries to audit the given {@link Object} if the {@link Object} is {@link Auditable}
   * otherwise simply returns the given {@link Object}.
   *
   * @param target {@link Object} to audit.
   * @return the given {@link Object}.
   * @see #audit(Auditable)
   */
  @NullSafe
  @SuppressWarnings("unchecked")
  default @Nullable Object audit(@Nullable Object target) {

    return target instanceof Auditable
      ? audit((Auditable<USER, PROCESS, ?>) target)
      : target;
  }

  /**
   * Audits the given, required {@link Auditable object}.
   *
   * @param <T> {@link Class subtype} of {@link Auditable object}.
   * @param <ID> {@link Class type} of the {@link Auditable object}'s identifier.
   * @param auditable {@link Auditable object} to audit.
   * @return the given {@link Auditable object}.
   * @throws IllegalArgumentException if the {@link Auditable object} is {@literal null}.
   * @throws IllegalStateException if the configured user is not set.
   * @see org.cp.elements.lang.Auditable
   * @see #getDateTime()
   * @see #getProcess()
   * @see #getUser()
   */
  default @NotNull <ID extends Comparable<ID>, T extends Auditable<USER, PROCESS, ID>> T audit(@NotNull T auditable) {

    Assert.notNull(auditable, "The Auditable object to audit is required");

    Instant dateTime = getDateTime().get();

    PROCESS process = getProcess().get();
    USER user = requireUser(getUser().get());

    boolean setCreatedProperties = auditable.isNew() || isCreatedPropertiesUnset(auditable);

    if (setCreatedProperties) {
      auditable.setCreatedBy(user);
      auditable.setCreatedOn(dateTime);
      auditable.setCreatedWith(process);
    }

    boolean setModifiedProperties = auditable.isModified();

    if (setModifiedProperties) {
      auditable.setModifiedBy(user);
      auditable.setModifiedOn(dateTime);
      auditable.setModifiedWith(process);
    }

    return auditable;
  }

  /**
   * Determines whether the created properties of the given, required {@link Auditable object} have been properly set.
   * <p>
   * Only the {@literal createdBy} and {@literal createdOn} properties are used to make the determination.
   *
   * @param auditable {@link Auditable object} who's created properties are evaluated.
   * @return a boolean value indicating whether the created properties of the given, required {@link Auditable object}
   * have been properly set.
   * @see org.cp.elements.lang.Auditable
   */
  default boolean isCreatedPropertiesUnset(@NotNull Auditable<USER, PROCESS, ?> auditable) {
    return auditable.getCreatedBy() == null || auditable.getCreatedOn() == null;
  }

  /**
   * Requires that the {@link USER} be known when auditing.
   *
   * @param user {@link USER} object to evaluate.
   * @return the given {@link USER} object.
   * @throws IllegalStateException if the {@link USER} is not known.
   */
  default @NotNull USER requireUser(@Nullable USER user) {
    Assert.state(user != null, "User is required");
    return user;
  }
}
