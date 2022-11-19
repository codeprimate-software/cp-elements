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

import org.cp.elements.lang.annotation.Nullable;

/**
 * Abstract Data Type (ADT) used to create methods with in/out parameters.
 *
 * This class is a wrapper around the value it encapsulates. In essence, an instance of this class is the same as
 * the {@link Object value} itself, as determined by the {@literal equals} and {@literal hashCode} methods
 * and so this class also serves as {@link Object value holder}.
 *
 * @author John J. Blum
 * @param <T> {@link Class type} of the in/out parameter's {@link Object value}.
 * @see org.cp.elements.lang.ValueHolder
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class InOutParameter<T> {

  private volatile T value;

  /**
   * Default constructor creating an instance of InOutParameter initialized with a null value.
   */
  public InOutParameter() { }

  /**
   * Constructs an instance of InOutParameter with the specified value.
   *
   * @param value initial value of this parameter.
   */
  public InOutParameter(@Nullable T value) {
    this.value = value;
  }

  /**
   * Gets the value of this in/out parameter.
   *
   * @return the value of this in/out parameter.
   * @see #setValue(Object)
   */
  public T getValue() {
    return this.value;
  }

  /**
   * Sets the value of this in/out parameter.
   *
   * @param value the Object value to set this in/out parameter to.
   * @see #getValue()
   */
  public void setValue(T value) {
    this.value = value;
  }

  /**
   * Determines whether the in/out parameter is equal in value to the specified Object.
   *
   * Note, this is not a typical or recommended equals method implementation, but then this is not
   * your typical class either!
   *
   * @param obj the Object value being evaluated for equality with this in/out parameter value.
   * @return boolean value indicating whether this in/out parameter value is equal in value to the specified Object.
   * @see java.lang.Object#equals(Object)
   */
  @Override
  public boolean equals(Object obj) {

    if (this == obj) {
      return true;
    }

    if (obj instanceof InOutParameter) {
      obj = ((InOutParameter<?>) obj).getValue();
    }

    return ObjectUtils.equalsIgnoreNull(this.value, obj);
  }

  /**
   * Computes the hash code of this in/out parameter.
   *
   * @return an integer value constituting the computed hash code of this in/out parameter.
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {
    return ObjectUtils.hashCode(getValue());
  }

  /**
   * Gets the String representation of this in/out parameter.
   *
   * @return a String describing this in/out parameter.
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    return String.valueOf(getValue());
  }
}
