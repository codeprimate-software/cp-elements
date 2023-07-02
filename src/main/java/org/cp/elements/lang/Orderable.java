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
 * Interface defining a contract for {@link Object objects} that can be organized in an ordered context, such as
 * an ordered data structure, like an array or {@link java.util.List}.
 * <p>
 * In general, the order of {@link Object objects} can be applied in different ways including sort order,
 * prioritization, or to ascertain precedence between {@link Orderable} {@link Object objects}
 * of the same {@link Class type}.
 *
 * @author John J. Blum
 * @param <T> {@link Class type} parameter specifying the class of the {@link Comparable} object.
 * @see java.lang.Comparable
 * @see java.lang.FunctionalInterface
 * @see org.cp.elements.lang.Ordered
 * @since 1.0.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface Orderable<T extends Comparable<T>> {

  /**
   * Gets the value of the order property which indicates the order of this {@link Object} relative to
   * other {@link Object objects} of the same {@link Class type}.
   *
   * @return a {@link Comparable value} of {@link Class type T} indicating this {@link Object object's}
   * sort order, prioritization or precedence.
   */
  T getOrder();

}
