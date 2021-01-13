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
 * The {@link Visitable} interface defines a contract for objects that can be visited by a {@link Visitor}.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.Visitor
 * @since 1.0.0
 */
public interface Visitable {

  /**
   * Accepts a {@link Visitor} to perform an operation or evaluation on this {@link Visitable} object.
   *
   * @param visitor {@link Object} who's {@link Class} implements the {@link Visitor} interface,
   * walking a graph of objects to perform an evaluation or operation.
   * @see org.cp.elements.lang.Visitor
   */
  default void accept(Visitor visitor) {
    visitor.visit(this);
  }
}
