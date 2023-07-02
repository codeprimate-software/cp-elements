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

import java.util.concurrent.atomic.AtomicReference;

import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.Versioned;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Abstract Data Type (ADT) used to support {@literal versioning} for an {@link Object application domain model object}
 * or {@literal POJO}.
 *
 * @author John Blum
 * @param <T> specific {@link Class type} and subclass extension of {@link AbstractVersionedObject}.
 * @param <VERSION> {@link Class type} of the {@literal version} value.
 * @see org.cp.elements.lang.Versioned
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AbstractVersionedObject<T extends AbstractVersionedObject<T, VERSION>, VERSION>
    implements Versioned<VERSION> {

  private volatile VERSION version;

  private final AtomicReference<VERSION> currentVersion = new AtomicReference<>(null);

  /**
   * Builder method used to set the {@link VERSION version} of this application domain model object.
   *
   * @param version {@literal VERSION} of this application domain model object; must not be {@literal null}.
   * @return this application domain model object.
   * @throws IllegalArgumentException if {@link VERSION version} is {@literal null}.
   * @see #setVersion(Object)
   */
  @SuppressWarnings("unchecked")
  public @NotNull T atVersion(@NotNull VERSION version) {
    setVersion(version);
    return (T) this;
  }

  /**
   * Gets the current {@link VERSION version} of this application domain model object as persisted in
   * the underlying, backend data store.
   *
   * @return the current {@link VERSION version} of this application domain model object as persisted in
   * the underlying, backend data store; maybe {@literal null} if this object has not yet been persisted,
   * or is new.
   * @see #getVersion()
   */
  public @Nullable VERSION getCurrentVersion() {
    return this.currentVersion.getAndUpdate(version -> getVersion());
  }

  @Override
  public @NotNull VERSION getVersion() {
    return ObjectUtils.requireState(this.version, "Version [%s] was not initialized");
  }

  /**
   * Sets the {@link VERSION version} of this application domain model object.
   *
   * @param version {@literal VERSION} of this application domain model object; must not be {@literal null}.
   * @throws IllegalArgumentException if {@link VERSION version} is {@literal null}.
   * @see #atVersion(Object)
   */
  public void setVersion(@NotNull VERSION version) {
    this.version = ObjectUtils.requireObject(version, "Version is required");
  }
}
