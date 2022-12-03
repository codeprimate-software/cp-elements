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
import org.cp.elements.lang.annotation.Nullable;

/**
 * {@link FunctionalInterface} defining a contract for implementing objects used to evaluate a required condition
 * of the application or system.
 *
 * @author John Blum
 * @see java.lang.FunctionalInterface
 * @since 1.0.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface Condition {

  Condition FALSE_CONDITION = () -> false;
  Condition TRUE_CONDITION = () -> true;

  /**
   * Factory method used to evaluate the given {@link Condition} for null-safety, returning the given {@link Condition}
   * if not {@literal null} or returning {@link Condition#TRUE_CONDITION} if {@link Boolean returnTrue}
   * is {@literal true} and {@link Condition#FALSE_CONDITION} otherwise.
   *
   * @param condition {@link Condition} to evaluate for null-safety.
   * @param returnTrue {@link Boolean} value indicating whether to return a {@link Condition#TRUE_CONDITION}
   * or {@link Condition#FALSE_CONDITION} when the given {@link Condition} is {@literal null}.
   * @return the given {@link Condition} if not {@literal null} or the {@link Condition#TRUE_CONDITION}
   * or {@link Condition#FALSE_CONDITION} when {@link Condition} is {@literal null}.
   */
  static @NotNull Condition nullSafeCondition(@Nullable Condition condition, boolean returnTrue) {
    return condition != null ? condition : returnTrue ? TRUE_CONDITION : FALSE_CONDITION;
  }

  /**
   * Evaluates the criteria of this {@link Condition} to determine whether the still criteria holds.
   *
   * {@link Condition} evaluation can be dynamic. Therefore, it is possible, during the runtime of the program,
   * that the conditions satisfying the criteria of this {@link Condition} change and therefore may no longer hold,
   * or the conditions switch from unsatisfied to now being upheld. This might change several times during runtime
   * as well.
   *
   * @return a boolean value indicating whether the criteria of this {@link Condition} holds.
   */
  boolean evaluate();

  /**
   * Composes this {@link Condition} with the given {@link Condition} using the logical AND operator.
   *
   * @param condition {@link Condition} to compose with this {@link Condition} using logical AND.
   * @return a new {@link Condition} composed of this {@link Condition} with the given {@link Condition}
   * using the logical AND operator.
   */
  default @NotNull Condition andThen(@NotNull Condition condition) {
    return () -> this.evaluate() && condition.evaluate();
  }

  /**
   * Composes this {@link Condition} with the given {@link Condition} using the logical OR operator.
   *
   * @param condition {@link Condition} to compose with this {@link Condition} using logical OR.
   * @return a new {@link Condition} composed of this {@link Condition} with the given {@link Condition}
   * using the logical OR operator.
   */
  default @NotNull Condition orElse(@NotNull Condition condition) {
    return () -> this.evaluate() || condition.evaluate();
  }

  /**
   * Composes this {@link Condition} with the given {@link Condition} using the XOR operator.
   *
   * @param condition {@link Condition} to compose with this {@link Condition} using the XOR operator.
   * @return a new {@link Condition} composed of this {@link Condition} with the given {@link Condition}
   * using the XOR operator.
   */
  default @NotNull Condition xor(@NotNull Condition condition) {
    return () -> this.evaluate() ^ condition.evaluate();
  }
}
