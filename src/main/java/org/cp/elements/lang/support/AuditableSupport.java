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
import java.util.Optional;

import org.cp.elements.lang.Auditable;
import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * {@link AuditableSupport} is an abstract base class supporting implementations of the {@link Auditable} interface.
 *
 * @author John Blum
 * @param <USER> {@link Class type} used to track the user.
 * @param <PROCESS> {@link Class type} used track the process.
 * @param <ID> {@link Class type} used as the {@link Identifiable} object's identifier.
 * @see java.lang.Comparable
 * @see java.time.Instant
 * @see org.cp.elements.lang.Auditable
 * @see org.cp.elements.lang.support.IdentifiableSupport
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AuditableSupport<USER, PROCESS, ID extends Comparable<ID>> extends IdentifiableSupport<ID>
    implements Auditable<USER, PROCESS, ID> {

  private static final String USER_NAME_SYSTEM_PROPERTY = "user.name";

  private Instant createdOn;
  private Instant lastModifiedOn;
  private Instant modifiedOn;

  private PROCESS createdWith;
  private PROCESS lastModifiedWith;
  private PROCESS modifiedWith;

  private USER createdBy;
  private USER lastModifiedBy;
  private USER modifiedBy;

  @Override
  public @Nullable USER getCreatedBy() {
    return this.createdBy;
  }

  @Override
  public void setCreatedBy(@NotNull USER createdBy) {
    this.createdBy = ObjectUtils.requireObject(createdBy, "Created by is required");
  }

  @Override
  public @Nullable Instant getCreatedOn() {
    return this.createdOn;
  }

  @Override
  public void setCreatedOn(@NotNull Instant createdOn) {
    this.createdOn = ObjectUtils.requireObject(createdOn, "Created on is required");
  }

  @Override
  public @Nullable PROCESS getCreatedWith() {
    return this.createdWith;
  }

  @Override
  public void setCreatedWith(@NotNull PROCESS createdWith) {
    this.createdWith = ObjectUtils.requireObject(createdWith, "Created with is required");
  }

  @Override
  public @Nullable USER getLastModifiedBy() {
    return this.lastModifiedBy;
  }

  @Override
  public @Nullable Instant getLastModifiedOn() {
    return this.lastModifiedOn;
  }

  @Override
  public @Nullable PROCESS getLastModifiedWith() {
    return this.lastModifiedWith;
  }

  @Override
  public @Nullable USER getModifiedBy() {
    return Optional.ofNullable(this.modifiedBy)
      .orElseGet(this::getCreatedBy);
  }

  @Override
  @SuppressWarnings("unchecked")
  public void setModifiedBy(@NotNull USER modifiedBy) {
    this.modifiedBy = ObjectUtils.requireObject(modifiedBy, "Modified by is required");
    this.lastModifiedBy = ObjectUtils.returnFirstNonNullValue(this.lastModifiedBy, this.modifiedBy);
  }

  @Override
  public @Nullable Instant getModifiedOn() {
    return Optional.ofNullable(this.modifiedOn)
      .orElseGet(this::getCreatedOn);
  }

  @Override
  public void setModifiedOn(@NotNull Instant modifiedOn) {
    this.modifiedOn = ObjectUtils.requireObject(modifiedOn, "Modified on is required");
    this.lastModifiedOn = ObjectUtils.returnFirstNonNullValue(this.lastModifiedOn, this.modifiedOn);
  }

  @Override
  public @Nullable PROCESS getModifiedWith() {
    return Optional.ofNullable(this.modifiedWith)
      .orElseGet(this::getCreatedWith);
  }

  @Override
  @SuppressWarnings("unchecked")
  public void setModifiedWith(@NotNull PROCESS modifiedWith) {
    this.modifiedWith = ObjectUtils.requireObject(modifiedWith, "Modified with is required");
    this.lastModifiedWith = ObjectUtils.returnFirstNonNullValue(this.lastModifiedWith, this.modifiedWith);
  }

  /**
   * Returns the {@link String name} of the {@link USER} running the application.
   *
   * The {@link String name} of the {@link USER} can be defined by
   * the Java {@link System#getProperties() System property}, {@literal user.name}.
   *
   * @return a {@link String} containing the {@literal name} of the {@link USER} running the application.
   * @see java.lang.System#getProperty(String)
   */
  protected @Nullable String getUsername() {
    return System.getProperty(USER_NAME_SYSTEM_PROPERTY);
  }
}
