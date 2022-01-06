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

import org.cp.elements.lang.annotation.NotNull;

/**
 * The {@link Visitable} interface defines a contract for {@link Object objects} that can be visited by
 * a {@link Visitor}.
 *
 * The {@link Visitable} interface is an essential component in the {@literal Visitor Software Design Pattern}.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.Visitor
 * @see <a href="https://en.wikipedia.org/wiki/Visitor_pattern">Visitor Software Design Pattern</a>
 * @since 1.0.0
 */
public interface Visitable {

  /**
   * Accepts a {@link Visitor} used to perform an operation or evaluation of {@literal this} {@link Visitable} object.
   *
   * @param visitor {@link Object} who's {@link Class} implements the {@link Visitor} interface and is used to
   * walk a graph of {@link Object objects} in order to perform an operation or make an evaluation;
   * must not be {@literal null}.
   * @see org.cp.elements.lang.Visitor
   */
  default void accept(@NotNull Visitor visitor) {
    visitor.visit(this);
  }
}
