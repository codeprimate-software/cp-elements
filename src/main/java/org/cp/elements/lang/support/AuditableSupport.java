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

import static org.cp.elements.lang.ObjectUtils.defaultIfNull;

import java.time.Instant;
import java.util.Optional;
import java.util.function.Supplier;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Auditable;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * {@link AuditableSupport} is an abstract base class supporting implementations of the {@link Auditable} interface.
 *
 * @author John Blum
 * @see java.time.Instant
 * @see org.cp.elements.lang.Auditable
 * @see org.cp.elements.lang.support.IdentifiableSupport
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AuditableSupport<USER, PROCESS, ID extends Comparable<ID>> extends IdentifiableSupport<ID>
    implements Auditable<USER, PROCESS, ID>  {

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

  /**
   * Assert that the given {@link Object value} is not {@literal null}.
   *
   * @param <T> {@link Class type} of the {@link Object value}.
   * @param value {@link Object} to evaluate.
   * @param message {@link Supplier} containing the message used in the {@link IllegalArgumentException}.
   * @return the given {@link Object value} if not {@literal null}.
   * @throws IllegalArgumentException with {@link Supplier message} if the {@link Object value} is {@literal null}.
   * @see java.util.function.Supplier
   */
  private <T> T assertNotNull(T value, Supplier<String> message) {
    Assert.notNull(value, message);
    return value;
  }

  /**
   * @inheritDoc
   */
  @Override
  public @Nullable USER getCreatedBy() {
    return this.createdBy;
  }

  /**
   * @inheritDoc
   */
  @Override
  public void setCreatedBy(@NotNull USER createdBy) {
    this.createdBy = assertNotNull(createdBy, () -> "Created by is required");
  }

  /**
   * @inheritDoc
   */
  @Override
  public @Nullable Instant getCreatedOn() {
    return this.createdOn;
  }

  /**
   * @inheritDoc
   */
  @Override
  public void setCreatedOn(@NotNull Instant createdOn) {
    this.createdOn = assertNotNull(createdOn, () -> "Created on is required");
  }

  /**
   * @inheritDoc
   */
  @Override
  public @Nullable PROCESS getCreatedWith() {
    return this.createdWith;
  }

  /**
   * @inheritDoc
   */
  @Override
  public void setCreatedWith(@NotNull PROCESS createdWith) {
    this.createdWith = assertNotNull(createdWith, () -> "Created with is required");
  }

  /**
   * @inheritDoc
   */
  @Override
  public @Nullable USER getLastModifiedBy() {
    return this.lastModifiedBy;
  }

  /**
   * @inheritDoc
   */
  @Override
  public @Nullable Instant getLastModifiedOn() {
    return this.lastModifiedOn;
  }

  /**
   * @inheritDoc
   */
  @Override
  public @Nullable PROCESS getLastModifiedWith() {
    return this.lastModifiedWith;
  }

  /**
   * @inheritDoc
   */
  @Override
  public @Nullable USER getModifiedBy() {
    return Optional.ofNullable(this.modifiedBy)
      .orElseGet(this::getCreatedBy);
  }

  /**
   * @inheritDoc
   */
  @Override
  @SuppressWarnings("unchecked")
  public void setModifiedBy(@NotNull USER modifiedBy) {
    this.modifiedBy = assertNotNull(modifiedBy, () -> "Modified by is required");
    this.lastModifiedBy = defaultIfNull(this.lastModifiedBy, this.modifiedBy);
  }

  /**
   * @inheritDoc
   */
  @Override
  public @Nullable Instant getModifiedOn() {
    return Optional.ofNullable(this.modifiedOn)
      .orElseGet(this::getCreatedOn);
  }

  /**
   * @inheritDoc
   */
  @Override
  public void setModifiedOn(@NotNull Instant modifiedOn) {
    this.modifiedOn = assertNotNull(modifiedOn, () -> "Modified on is required");
    this.lastModifiedOn = defaultIfNull(this.lastModifiedOn, this.modifiedOn);
  }

  /**
   * @inheritDoc
   */
  @Override
  public @Nullable PROCESS getModifiedWith() {
    return Optional.ofNullable(this.modifiedWith)
      .orElseGet(this::getCreatedWith);
  }

  /**
   * @inheritDoc
   */
  @Override
  @SuppressWarnings("unchecked")
  public void setModifiedWith(@NotNull PROCESS modifiedWith) {
    this.modifiedWith = assertNotNull(modifiedWith, () -> "Modified with is required");
    this.lastModifiedWith = defaultIfNull(this.lastModifiedWith, this.modifiedWith);
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
