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
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Interface defining a contract for {@link Object objects} whose {@link Class type} implements this interface
 * in order to walk an {@link Object} graph to perform some operation or make some evaluation of particular
 * {@link Object objects} or {@link Class types} of {@link Object objects}.
 * <p>
 * The {@link Visitor} interface is a definition of the {@literal Visitor Software Design Pattern}, used to
 * separate algorithm from {@link Object} structure.
 *
 * @author John J. Blum
 * @see java.lang.FunctionalInterface
 * @see org.cp.elements.lang.Visitable
 * @since 1.0.0
 */
@FunctionalInterface
public interface Visitor {

  /**
   * Factory method used to guard against a {@literal null} {@link Visitor}.
   *
   * @param visitor {@link Visitor} to evaluate for {@literal null}.
   * @return the given {@link Visitor} if not {@literal null}, otherwise returns a default,
   * no-op {@link Visitor} implementation.
   */
  @NullSafe
  static @NotNull Visitor nullSafeVisitor(@Nullable Visitor visitor) {
    return visitor != null ? visitor : visitable -> { };
  }

  /**
   * Visits the {@link Visitable object} in order to perform a function or make an evaluation of
   * the target {@link Object}.
   *
   * @param visitable {@link Visitable object} visited by this {@link Visitor}.
   * @see org.cp.elements.lang.Visitable
   */
  void visit(Visitable visitable);

}
