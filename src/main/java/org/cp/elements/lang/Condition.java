/*
 * Copyright 2016 Author or Authors.
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
 * The {@link Condition} interface defines a contract for implementing objects used to evaluate a required condition
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
   * Evaluates the required criteria of this condition to determine whether the condition holds.
   *
   * @return a boolean value indicating whether the condition holds.
   */
  boolean evaluate();

  /**
   * Composes this {@link Condition} with the given {@link Condition} using the logical AND operator.
   *
   * @param condition {@link Condition} to compose with this {@link Condition} using logical AND.
   * @return a new {@link Condition} composed of this {@link Condition} with the given {@link Condition}
   * using the logical AND operator.
   */
  default Condition andThen(Condition condition) {
    return () -> this.evaluate() && condition.evaluate();
  }

  /**
   * Composes this {@link Condition} with the given {@link Condition} using the logical OR operator.
   *
   * @param condition {@link Condition} to compose with this {@link Condition} using logical OR.
   * @return a new {@link Condition} composed of this {@link Condition} with the given {@link Condition}
   * using the logical OR operator.
   */
  default Condition orElse(Condition condition) {
    return () -> this.evaluate() || condition.evaluate();
  }

  /**
   * Composes this {@link Condition} with the given {@link Condition} using the XOR operator.
   *
   * @param condition {@link Condition} to compose with this {@link Condition} using the XOR operator.
   * @return a new {@link Condition} composed of this {@link Condition} with the given {@link Condition}
   * using the XOR operator.
   */
  default Condition xor(Condition condition) {
    return () -> this.evaluate() ^ condition.evaluate();
  }
}
