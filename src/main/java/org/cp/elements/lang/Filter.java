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
 * The {@link Filter} interface defines a contract for an object that functions as a filter for other objects.
 *
 * @author John J. Blum
 * @see java.lang.FunctionalInterface
 * @since 1.0.0
 */
@FunctionalInterface
public interface Filter<T> {

  /**
   * Determines whether the specified object meets the criteria (rules) defined by this Filter.
   *
   * @param obj the Object being evaluated by this Filter.
   * @return a boolean value indicating whether the specified Object satisfies the criteria (rules) of this Filter.
   */
  boolean accept(T obj);

}
