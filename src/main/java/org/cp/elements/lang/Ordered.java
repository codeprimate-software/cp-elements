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
 * The Ordered interface defines a contract for classes where an object's order is determined according to an index.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.Orderable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Ordered {

  /**
   * Gets the index of this object in an ordered context.
   *
   * @return a integer value indicating this object's index in the ordered context.
   */
  int getIndex();

  /**
   * Sets the index of this object in the ordered context.
   *
   * @param index an integer value indicating this object's index in the ordered context.
   */
  void setIndex(int index);

}
