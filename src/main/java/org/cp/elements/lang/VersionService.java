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

import java.util.Optional;

import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.support.AbstractVersionedObject;
import org.cp.elements.service.annotation.Service;

/**
 * Elements {@link Service} component used to generate new {@link VERSION versions}.
 *
 * @author John Blum
 * @see java.lang.FunctionalInterface
 * @see org.cp.elements.lang.Versioned
 * @since 1.0.0
 */
@Service
@FunctionalInterface
@SuppressWarnings("unused")
public interface VersionService<VERSION> {

  /**
   * Generates a new, unique {@link VERSION} identifier for an application domain model object.
   *
   * @return a new, unique {@link VERSION} identifier for an application domain model object.
   */
  VERSION newVersion();

  /**
   * Sets the {@link VERSION version} of the given {@link Versioned} object.
   *
   * Only sets the {@link VERSION} if the {@link Versioned} object is {@literal non-null}
   * and is a {@link AbstractVersionedObject}.
   *
   * @param <T> {@link Class type} of the {@link Versioned} object.
   * @param versionedObject {@link Versioned} object on which to set the version.
   * @return the given {@link Versioned} object.
   * @see org.cp.elements.lang.Versioned
   * @see #newVersion()
   */
  @SuppressWarnings("unchecked")
  default @Nullable <T extends Versioned<?>> T setVersion(@Nullable T versionedObject) {

    return Optional.ofNullable(versionedObject)
      .filter(AbstractVersionedObject.class::isInstance)
      .map(AbstractVersionedObject.class::cast)
      .map(abstractVersionedObject -> (T) abstractVersionedObject.atVersion(newVersion()))
      .orElse(versionedObject);
  }
}
