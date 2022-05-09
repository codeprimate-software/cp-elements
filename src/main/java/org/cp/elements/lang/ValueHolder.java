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

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamException;
import java.io.Serializable;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.ComparatorUtils;

/**
 * Value {@link Class type} used to hold a {@link Object value}.
 *
 * @author John Blum
 * @param <T> {@link Class type} of {@link Object value} held by {@literal this} {@link ValueHolder}.
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ValueHolder<T> {

  private T value;

  /**
   * Factory method used to construct a new instance of {@link ValueHolder} initialized with
   * the given {@link Comparable value}.
   *
   * The {@link ValueHolder} implementation itself implements the {@link Comparable} interface.
   *
   * @param <T> {@link Class type} of {@link Comparable value}.
   * @param value {@link Comparable value} to hold.
   * @return a {@link ValueHolder} implementation that holds {@link Comparable} values.
   * @see org.cp.elements.lang.ValueHolder.ComparableValueHolder
   * @see java.lang.Comparable
   */
  public static @NotNull <T extends Comparable<T>> ComparableValueHolder<T> withComparableValue(@Nullable T value) {
    return new ComparableValueHolder<>(value);
  }

  /**
   * Factory method used to construct a new instance of {@link ValueHolder} initialized with
   * the given immutable {@link Object value}.
   *
   * @param <T> {@link Class type} of the {@link Object immutable value}.
   * @param value {@link Object immutable value} to hold.
   * @return a {@link ValueHolder} implementation that enforces the immutability of {@link Object values} it holds
   * by using the {@link Object#clone()} method for values that implement the {@link Cloneable} interface.
   * @throws IllegalTypeException if {@link Object value} is not {@link Cloneable}.
   * @see java.lang.Cloneable
   */
  public static @NotNull <T extends Cloneable> ValueHolder<T> withImmutableValue(@Nullable T value) {

    Assert.isInstanceOf(value, Cloneable.class, "Value [%s] is not Cloneable",
      ObjectUtils.getClassName(value));

    return new ValueHolder<T>(ObjectUtils.clone(value)) {

      @Override
      public @NotNull T getValue() {
        return ObjectUtils.clone(super.getValue());
      }

      @Override
      public void setValue(@NotNull T value) {
        super.setValue(ObjectUtils.clone(value));
      }
    };
  }

  /**
   * Factory method used to construct a new instance of {@link ValueHolder} initialized with
   * the given, required {@link Object value}.
   *
   * @param <T> {@link Class type} of {@link Object value}.
   * @param value {@link Object value} to hold; must not be {@literal null}.
   * @return a {@link ValueHolder} instance that enforces {@literal non-null} {@link Object values}.
   * @throws IllegalArgumentException if {@link Object value} is {@literal null}.
   */
  public static @NotNull <T> ValueHolder<T> withNonNullValue(@NotNull T value) {

    Assert.notNull(value, "Value is required");

    return new ValueHolder<T>(value) {

      @Override
      public void setValue(@NotNull T value) {
        Assert.notNull(value, "Value is required");
        super.setValue(value);
      }
    };
  }

  /**
   * Factory method used to construct a new instance of {@link ValueHolder} initialized with
   * the given {@link Serializable value}.
   *
   * @param <T> {@link Class type} of {@link Serializable value}.
   * @param value {@link Serializable value} to be held.
   * @return a {@link ValueHolder} implementation that holds {@link Serializable values}.
   * @see java.io.Serializable
   */
  public static @NotNull <T extends Serializable> SerializableValueHolder<T> withSerializableValue(@Nullable T value) {
    return new SerializableValueHolder<>(value);
  }

  /**
   * Constructs a new instance of {@link ValueHolder} with no {@link Object value}.
   *
   * @see #ValueHolder(Object)
   */
  public ValueHolder() { }

  /**
   * Constructs a new instance of {@link ValueHolder} initialized with the given {@link Object value}.
   *
   * @param value {@link Object value} held by {@literal this} {@link ValueHolder}.
   * @see #ValueHolder()
   */
  public ValueHolder(@Nullable T value) {
    this.value = value;
  }

  /**
   * Gets the {@link Object value} held by {@literal this} {@link ValueHolder}.
   *
   * @return the {@link Object value} held by {@literal this} {@link ValueHolder}.
   */
  public @Nullable T getValue() {
    return this.value;
  }

  /**
   * Sets the {@link Object value} held by {@literal this} {@link ValueHolder}.
   *
   * @param value {@link Object value} held by {@literal this} {@link ValueHolder}.
   */
  public void setValue(@Nullable T value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object obj) {

    if (this == obj) {
      return true;
    }

    if (!(obj instanceof ValueHolder)) {
      return false;
    }

    ValueHolder<?> that = (ValueHolder<?>) obj;

    return ObjectUtils.equalsIgnoreNull(this.getValue(), that.getValue());
  }

  @Override
  public int hashCode() {
    return ObjectUtils.hashCodeOf(getValue());
  }

  @Override
  public String toString() {
    return String.valueOf(getValue());
  }

  /**
   * {@link ValueHolder} implementation that holds {@link Comparable values}.
   *
   * @param <T> {@link Class type} of {@link Comparable value}.
   * @see java.lang.Comparable
   */
  public static class ComparableValueHolder<T extends Comparable<T>> extends ValueHolder<T> implements Comparable<T> {

    public ComparableValueHolder() { }

    public ComparableValueHolder(@Nullable T value) {
      super(value);
    }

    @Override
    @SuppressWarnings("all")
    public int compareTo(@Nullable T value) {
      return ComparatorUtils.compareIgnoreNull(getValue(), value);
    }
  }

  /**
   * {@link ValueHolder} implementation that holds {@link Serializable values}.
   *
   * @param <T> {@link Class type} of {@link Serializable value}.
   * @see java.io.Serializable
   */
  public static class SerializableValueHolder<T extends Serializable> extends ValueHolder<T> implements Serializable {

    private static final long serialVersionUID = 421081248;

    public SerializableValueHolder() { }

    public SerializableValueHolder(@Nullable T value) {
      super(value);
    }

    @SuppressWarnings("unchecked")
    private void readObject(@NotNull ObjectInputStream in) throws ClassNotFoundException, IOException {
      setValue((T) in.readObject());
    }

    private void readObjectNoData() throws ObjectStreamException {
      setValue(null);
    }

    private void writeObject(@NotNull ObjectOutputStream out) throws IOException {
      out.writeObject(getValue());
    }
  }
}
