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
 * The Orderable interface defines a contract for classes whose objects can be organized in an ordered context, such as
 * an ordered data structure using arrays or Lists.  In general, the order of objects can be applied in many different
 * ways including sort order, prioritization, or to ascertain precedence between Orderable objects of the same type.
 *
 * @author John J. Blum
 * @param <T> a type parameter indicating the class of the Orderable type.
 * @see java.lang.Comparable
 * @see org.cp.elements.lang.Ordered
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Orderable<T extends Comparable<T>> {

  /**
   * Gets the value of the order property which indicates the order of this object relative to it's peers.
   *
   * @return a Comparable value of type T indicating this object's order of precedence.
   */
  T getOrder();

}
