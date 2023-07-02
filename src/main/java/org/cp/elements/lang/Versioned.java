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

/**
 * Interface defining a contract for {@link Object Objects} that can be {@literal versioned}.
 * <p>
 * The {@link Versioned} interface can serve as an alternative to {@link Auditable} object metadata in transactional,
 * optimistic locking use cases.
 *
 * @author John Blum
 * @param <T> {@link Class type} of {@link Object} modeling the {@literal version}.
 * @see <a href="https://en.wikipedia.org/wiki/Optimistic_concurrency_control">Optimistic Concurrency Control (aka Optimistic Locking)</a>
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Versioned<T> {

  /**
   * Gets the {@link T version} of {@literal this} {@link Object}.
   *
   * @return the {@link T version} of {@literal this} {@link Object}.
   */
  T getVersion();

}
