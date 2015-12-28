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

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamException;
import java.io.Serializable;

import org.cp.elements.util.ComparatorUtils;

/**
 * The ValueHolder class is a value holding type.
 *
 * @author John Blum
 * @param <T> the Class type of the value held by this ValueHolder.
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ValueHolder<T> {

  private T value;

  /**
   * Factory method to construct an instance of the ValueHolder class with a Comparable value.  The ValueHolder
   * implementation itself implements the Comparable interface.
   *
   * @param <T> the Class type of the Comparable value.
   * @param value the Comparable value to hold.
   * @return a ValueHolder implementation that holds Comparable values.
   * @see java.lang.Comparable
   */
  public static <T extends Comparable<T>> ComparableValueHolder<T> withComparableValue(final T value) {
    return new ComparableValueHolder<>(value);
  }

  /**
   * Factory method to construct an instance of the ValueHolder class with an immutable Object value.
   *
   * @param <T> the Class type of the Cloneable value.
   * @param value the Cloneable, immutable value to hold.
   * @return a ValueHolder implementation that enforces the immutability of values it holds
   * by way of the Object.clone() method for values that implement the java.lang.Cloneable interface.
   * @see java.lang.Cloneable
   */
  public static <T extends Cloneable> ValueHolder<T> withImmutableValue(final T value) {
    return new ValueHolder<T>(ObjectUtils.clone(value)) {
      @Override public T getValue() {
        return ObjectUtils.clone(super.getValue());
      }

      @Override public void setValue(final T value) {
        super.setValue(ObjectUtils.clone(value));
      }
    };
  }

  /**
   * Factory method to construct an instance of the ValueHolder class with a non-null value.
   *
   * @param <T> the Class type of the Object value.
   * @param value the non-null value to hold.
   * @return a ValueHolder instance that enforces non-null values.
   * @throws NullPointerException if the value is null.
   */
  public static <T> ValueHolder<T> withNonNullValue(final T value) {
    Assert.notNull(value, "The value must not be null!");

    return new ValueHolder<T>(value) {
      @Override
      public void setValue(final T value) {
        Assert.notNull(value, "The value must not be null!");
        super.setValue(value);
      }
    };
  }

  /**
   * Factory method to construct in instance of the ValueHolder class with a Serializable value.
   *
   * @param <T> the Class type of the Serializable value.
   * @param value the Serializable value to be held.
   * @return a ValueHolder implementation that holds Serializable values.
   * @see java.io.Serializable
   */
  public static <T extends Serializable> SerializableValueHolder<T> withSerializableValue(final T value) {
    return new SerializableValueHolder<>(value);
  }

  /**
   * Constructs an instance of the ValueHolder class with a null value.
   *
   * @see #ValueHolder(Object)
   */
  public ValueHolder() {
  }

  /**
   * Constructs an instance of the ValueHolder class initialized with the specified value.
   *
   * @param value the Object value held by this ValueHolder.
   * @see #ValueHolder()
   */
  public ValueHolder(final T value) {
    this.value = value;
  }

  /**
   * Gets the value held by this ValueHolder.
   *
   * @return the value of this ValueHolder.
   */
  public T getValue() {
    return value;
  }

  /**
   * Sets the value to be held by this ValueHolder.
   *
   * @param value the value of this ValueHolder.
   */
  public void setValue(final T value) {
    this.value = value;
  }

  /* (non-Javadoc) */
  @Override
  public boolean equals(final Object obj) {
    if (obj == this) {
      return true;
    }

    if (!(obj instanceof ValueHolder)) {
      return false;
    }

    ValueHolder that = (ValueHolder) obj;

    return ObjectUtils.equalsIgnoreNull(this.getValue(), that.getValue());
  }

  /* (non-Javadoc) */
  @Override
  public int hashCode() {
    int hashValue = 17;
    hashValue = 37 * hashValue + ObjectUtils.hashCode(getValue());
    return hashValue;
  }

  /* (non-Javadoc) */
  @Override
  public String toString() {
    return String.valueOf(getValue());
  }

  /**
   * The ComparableValueHolder class is an extension of ValueHolder that holds Comparable values.
   *
   * @param <T> the Class type of the Comparable value.
   * @see java.lang.Comparable
   */
  public static class ComparableValueHolder<T extends Comparable<T>> extends ValueHolder<T> implements Comparable<T> {

    public ComparableValueHolder() {
    }

    public ComparableValueHolder(final T value) {
      super(value);
    }

    @Override
    @SuppressWarnings("all")
    public int compareTo(final T value) {
      return ComparatorUtils.compareIgnoreNull(getValue(), value);
    }
  }

  /**
   * The SerializableValueHolder class is an extension of ValueHolder that holds Serializable values.
   *
   * @param <T> the Class type of the Serializable value.
   * @see java.io.Serializable
   */
  public static class SerializableValueHolder<T extends Serializable> extends ValueHolder<T> implements Serializable {

    private static final long serialVersionUID = 421081248;

    public SerializableValueHolder() {
    }

    public SerializableValueHolder(final T value) {
      super(value);
    }

    @SuppressWarnings("unchecked")
    private void readObject(final ObjectInputStream in) throws ClassNotFoundException, IOException {
      setValue((T) in.readObject());
    }

    private void readObjectNoData() throws ObjectStreamException {
      setValue(null);
    }

    private void writeObject(final ObjectOutputStream out) throws IOException {
      out.writeObject(getValue());
    }
  }

}
