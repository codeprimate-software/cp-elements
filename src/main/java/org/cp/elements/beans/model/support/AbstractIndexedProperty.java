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
package org.cp.elements.beans.model.support;

import java.beans.PropertyDescriptor;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.SortedSet;
import java.util.function.BiFunction;
import java.util.function.Predicate;

import org.cp.elements.beans.model.BeanModel;
import org.cp.elements.beans.model.BeanUtils;
import org.cp.elements.beans.model.Property;
import org.cp.elements.data.struct.KeyValue;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.Orderable;
import org.cp.elements.lang.annotation.Dsl;
import org.cp.elements.lang.annotation.FluentApi;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Abstract Data Type (ADT) modeling an {@literal indexed} bean property.
 *
 * The primary function of this ADT is to model {@literal ordered}, index-based bean properties, such as an array,
 * {@link List} or {@link Map}, where the property's collection of values can be individually referenced with
 * an index, such as with a positional index in an array or {@link Map} key.
 *
 * @author John Blum
 * @see java.beans.PropertyDescriptor
 * @see org.cp.elements.beans.model.BeanModel
 * @see org.cp.elements.beans.model.Property
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractIndexedProperty extends Property {

  protected static final Predicate<Class<?>> IS_ARRAY = ClassUtils::isArray;
  protected static final Predicate<Class<?>> IS_LIST = type -> List.class.isAssignableFrom(nullSafeType(type));
  protected static final Predicate<Class<?>> IS_MAP = type -> Map.class.isAssignableFrom(nullSafeType(type));
  protected static final Predicate<Class<?>> IS_SORTED_SET = type -> SortedSet.class.isAssignableFrom(nullSafeType(type));
  protected static final Predicate<Class<?>> IS_INDEXED = IS_ARRAY.or(IS_LIST).or(IS_MAP).or(IS_SORTED_SET);

  /**
   * Determines whether the given bean {@link Property} is indexed-based.
   *
   * @param property {@link Property} to evaluate.
   * @return a boolean value indicating whether the given bean {@link Property} is indexed-based.
   * @see org.cp.elements.beans.model.Property
   */
  @NullSafe
  public static boolean isIndexed(@Nullable Property property) {
    return IS_INDEXED.test(BeanUtils.resolveType(property));
  }

  /**
   * Determines whether the bean property described by the given {@link PropertyDescriptor} is indexed-based.
   *
   * @param propertyDescriptor {@link PropertyDescriptor} to evaluate.
   * @return a boolean value indicating whether the bean property described by
   * the given {@link PropertyDescriptor} is indexed-based.
   * @see java.beans.PropertyDescriptor
   */
  @NullSafe
  public static boolean isIndexed(@Nullable PropertyDescriptor propertyDescriptor) {
    return IS_INDEXED.test(BeanUtils.resolveType(propertyDescriptor));
  }

  /**
   * Returns the given {@link Class type} if not {@literal null} or {@link Class Object.class}
   * if {@link Class type} is {@literal null}.
   *
   * @param type {@link Class} object to evaluate.
   * @return the given {@link Class type} if not {@literal null} or {@link Class Object.class}
   * if {@link Class type} is {@literal null}.
   */
  @NullSafe
  protected static @NotNull Class<?> nullSafeType(@Nullable Class<?> type) {
    return type != null ? type : Object.class;
  }

  /**
   * Constructs a new instance of {@link AbstractIndexedProperty} initialized with the given,
   * required {@link BeanModel} modeling the bean containing this indexed property along with
   * the given, required {@link PropertyDescriptor} describing this indexed property.
   *
   * @param beanModel {@link BeanModel} modeling the bean containing this property; must not be {@literal null}.
   * @param propertyDescriptor {@link PropertyDescriptor} describing the indexed bean property;
   * must not be {@literal null}.
   * @throws IllegalArgumentException if the {@link BeanModel} or {@link PropertyDescriptor} are {@literal null}.
   * @see org.cp.elements.beans.model.BeanModel
   * @see java.beans.PropertyDescriptor
   */
  protected AbstractIndexedProperty(@NotNull BeanModel beanModel,
      @NotNull PropertyDescriptor propertyDescriptor) {

    super(beanModel, propertyDescriptor);
  }

  /**
   * Gets the {@link Object} value of the bean property at the given {@link INDEX index}.
   *
   * @param <INDEX> {@link Class type} of the index. For example, if an array, then an {@link Integer}.
   * @param index {@link INDEX} referencing the specific element in an index-based property, such as an array.
   * @return the {@link Object} value of the bean property at the given {@link INDEX index}.
   * @see #getValueAccessorFunction()
   * @see #getValue()
   */
  public <INDEX> Object getValue(INDEX index) {
    return getValueAccessorFunction().apply(getValue(), index);
  }

  /**
   * {@link BiFunction Function} used to access (get) a specific element in an index-based data structure,
   * such as an array or a {@link Map}.
   *
   * @param <INDEX> {@link Class type} of the index. For example, if an array, then an {@link Integer}.
   * @return the {@link BiFunction} used to access a specific element in an index-based data structure,
   * such as an array or a {@link Map}.
   * @see java.util.function.BiFunction
   */
  protected abstract <INDEX> BiFunction<Object, INDEX, Object> getValueAccessorFunction();

  /**
   * Set the {@link Object} value of the bean property at the given {@link INDEX index}.
   *
   * @param <INDEX> {@link Class type} of the index. For example, if an array, then an {@link Integer}.
   * @param index {@link INDEX} referencing the specific element in an index-based property, such as an array.
   * @return the previous {@link Object value} at the given {@link INDEX index}.
   * @see org.cp.elements.beans.model.support.AbstractIndexedProperty.IndexedValue
   * @see #getValueMutatorFunction()
   * @see #getValue()
   */
  public <INDEX> Object setValue(INDEX index, Object value) {
    return this.<INDEX>getValueMutatorFunction().apply(getValue(), IndexedValue.at(index).with(value));
  }

  /**
   * {@link BiFunction Function} used to mutate (set) a specific element in an index-based data structure,
   * such as an array or a {@link Map}.
   *
   * @param <INDEX> {@link Class type} of the index. For example, if an array, then an {@link Integer}.
   * @return the {@link BiFunction} used to mutate a specific element in an index-based data structure,
   * such as an array or a {@link Map}.
   * @see java.util.function.BiFunction
   */
  protected abstract <INDEX> BiFunction<Object, IndexedValue<INDEX, Object>, Object> getValueMutatorFunction();

  /**
   * Abstract Data Type (ADT) modeling both an {@link INDEX index} and {@link VALUE value}.
   *
   * The {@link Object value} is expected to be at the given {@link INDEX index}, position
   * within a {@link Collection Collection-like} data structure, such as an array or a {@link List}
   * or referencable by the {@link INDEX index} in a {@link Map Map-based} data structure.
   *
   * @param <INDEX> {@link Class type} of the index.
   * @param <VALUE> {@link Class type} of the value.
   * @see org.cp.elements.lang.annotation.FluentApi
   * @see org.cp.elements.data.struct.KeyValue
   */
  @FluentApi
  protected static class IndexedValue<INDEX, VALUE> implements KeyValue<INDEX, VALUE> {

    protected static final String INDEXED_VALUE_TO_STRING = "Value [%s] at Index [%s]";

    /**
     * Factory method used to construct a new instance of {@link IndexedValue} initialized with a
     * {@literal null} {@link Object value} at the given, required {@link INDEX index}.
     *
     * @param <INDEX> {@link Class type} of the index.
     * @param <VALUE> {@link Class type} of the value.
     * @param index index of the value within an indexed data structure,
     * such as an array, {@link List} or {@link Map}.
     * @return a new {@link IndexedValue}.
     * @throws IllegalArgumentException if {@link INDEX index} is {@literal null}.
     * @see org.cp.elements.lang.annotation.Dsl
     * @see #from(Object, Object)
     */
    @Dsl
    public static @NotNull <INDEX, VALUE> IndexedValue<INDEX, VALUE> at(@NotNull INDEX index) {
      return from(index, null);
    }

    /**
     * Factory method used to construct a new instance of {@link IndexedValue} initialized with a
     * given {@link VALUE value} at the given, required {@link INDEX index}.
     *
     * @param <INDEX> {@link Class type} of the index.
     * @param <VALUE> {@link Class type} of the value.
     * @param index index of the value within an indexed data structure,
     * such as an array, {@link List} or {@link Map}.
     * @param value {@link Object} referencing the value at index.
     * @return a new {@link IndexedValue}.
     * @throws IllegalArgumentException if {@link INDEX index} is {@literal null}.
     * @see org.cp.elements.lang.annotation.Dsl
     * @see #IndexedValue(INDEX, VALUE)
     */
    @Dsl
    public static @NotNull <INDEX, VALUE> IndexedValue<INDEX, VALUE> from(@NotNull INDEX index, @Nullable VALUE value) {
      return new IndexedValue<>(index, value);
    }

    private final INDEX index;

    private VALUE value;

    /**
     * Constructs a new instance of {@link IndexedValue} initialized with the given {@link VALUE value}
     * at the given, required {@link INDEX index} within an indexed data structure, such as an array,
     * {@link List} or {@link Map}.
     *
     * @param index index of the value within an indexed data structure,
     * such as an array, {@link List} or {@link Map}.
     * @param value {@link Object} referencing the value at index.
     * @throws IllegalArgumentException if {@link INDEX index} is {@literal null}.
     */
    protected IndexedValue(@NotNull INDEX index, @Nullable VALUE value) {

      this.index = ObjectUtils.requireObject(index, "Index is required");
      this.value = value;
    }

    /**
     * Gets the index of the value contained in the indexed data structure.
     *
     * @return the index of the value contained in the indexed data structure.
     */
    public @NotNull INDEX getIndex() {
      return this.index;
    }

    /**
     * Alias for {@link #getIndex()}.
     *
     * @return the value from {@link #getIndex()}.
     * @see #getIndex()
     */
    @Override
    public @NotNull INDEX getKey() {
      return getIndex();
    }

    /**
     * Gets the value at index in the indexed data structure.
     *
     * @return the value at index in the indexed data structure.
     * @see java.util.Optional
     */
    public Optional<VALUE> getValue() {
      return Optional.ofNullable(this.value);
    }

    /**
     * Build method used to set the value at index in the indexed data structure.
     *
     * @param value {@link Object} referencing the value to set at index.
     * @return this {@link IndexedValue}.
     * @see org.cp.elements.lang.annotation.Dsl
     */
    @Dsl
    public @NotNull IndexedValue<INDEX, VALUE> with(@Nullable VALUE value) {
      this.value = value;
      return this;
    }

    @Override
    @SuppressWarnings("unchecked")
    public boolean equals(Object obj) {

      if (this == obj) {
        return true;
      }

      if (!(obj instanceof IndexedValue)) {
        return false;
      }

      IndexedValue<INDEX, VALUE> that = (IndexedValue<INDEX, VALUE>) obj;

      return ObjectUtils.equals(this.getIndex(), that.getIndex())
        && ObjectUtils.equalsIgnoreNull(this.getValue(null), that.getValue(null));
    }

    @Override
    public int hashCode() {
      return ObjectUtils.hashCodeOf(getIndex(), getValue(null));
    }

    @Override
    public String toString() {
      return String.format(INDEXED_VALUE_TO_STRING, getValue(null), getIndex());
    }
  }

  @FluentApi
  protected static class OrderableIndexedValue<INDEX extends Comparable<INDEX>, VALUE>
      extends IndexedValue<INDEX, VALUE> implements Comparable<OrderableIndexedValue<INDEX, VALUE>>, Orderable<INDEX> {

    @Dsl
    public static @NotNull <INDEX extends Comparable<INDEX>, VALUE> OrderableIndexedValue<INDEX, VALUE> at(
        @NotNull INDEX index) {

      return new OrderableIndexedValue<>(index, null);
    }

    protected OrderableIndexedValue(@NotNull INDEX index, @Nullable VALUE value) {
      super(index, value);
    }

    @Override
    public @NotNull INDEX getOrder() {
      return getIndex();
    }

    @Override
    public int compareTo(@NotNull OrderableIndexedValue<INDEX, VALUE> value) {
      return this.getOrder().compareTo(value.getOrder());
    }
  }
}
