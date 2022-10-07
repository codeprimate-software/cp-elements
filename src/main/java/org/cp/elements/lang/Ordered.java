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
 * Interface defining a contract for {@link Object Objects} whose {@link Class types} defines an order
 * determined by an {@link Integer index}.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.Orderable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Ordered {

  int DEFAULT = 0;
  int FIRST = Integer.MIN_VALUE;
  int LAST = Integer.MAX_VALUE;

  /**
   * Gets the {@link Integer index} of this {@link Object} in an ordered context.
   *
   * @return the {@link Integer index} of this {@link Object} in an ordered context.
   */
  int getIndex();

  /**
   * Set the {@link Integer index} of this {@link Object} in an ordered context.
   *
   * @param index {@link Integer} indicating the order of this {@link Object} in an ordered context.
   */
  void setIndex(int index);

}
