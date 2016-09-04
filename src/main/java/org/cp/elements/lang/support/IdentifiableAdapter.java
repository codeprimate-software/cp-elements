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

package org.cp.elements.lang.support;

import org.cp.elements.lang.Constants;
import org.cp.elements.lang.Identifiable;

/**
 * The IdentifiableAdapter class is an abstract base class implementing the {@link Identifiable} interface
 * with default, no-op implementations of the interface methods throwing {@link UnsupportedOperationException}.
 *
 * @author John J. Blum
 * @param <T> Class type of the identifier.
 * @see org.cp.elements.lang.Identifiable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class IdentifiableAdapter<T extends Comparable<T>> implements Identifiable<T> {

  /**
   * Returns the identifier uniquely identifying this object.
   *
   * @return the value uniquely identifying this object.
   */
  @Override
  public T getId() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Sets the identifier uniquely identifying this object.
   *
   * @param id value of type T assigned as this object's unique identifier.
   */
  @Override
  public void setId(T id) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }
}
