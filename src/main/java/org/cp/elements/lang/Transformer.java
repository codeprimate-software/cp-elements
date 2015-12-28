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
 * The Transformer interface defines a contract for implementing classes who's objects transform data from one value
 * to another value of the same class type.
 *
 * @author John J. Blum
 * @param <T> the Class type of the data value (datum) to transform.
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Transformer<T> {

  /**
   * Transforms the given value of class type T into another value of class type T.
   *
   * @param value the value to transform.
   * @return the transformed value.
   */
  T transform(T value);

}
