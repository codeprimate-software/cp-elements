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
package org.cp.elements.beans.model;

import java.beans.BeanInfo;
import java.beans.PropertyDescriptor;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import org.cp.elements.beans.PropertyNotFoundException;
import org.cp.elements.function.FunctionUtils;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.CollectionUtils;
import org.cp.elements.util.stream.StreamUtils;
import org.cp.elements.util.stream.Streamable;

/**
 * Abstract Data Type (ADT) modeling a collection of bean properties.
 *
 * @author John Blum
 * @see java.beans.PropertyDescriptor
 * @see java.lang.Iterable
 * @see java.util.stream.Stream
 * @see org.cp.elements.beans.model.Property
 * @see org.cp.elements.util.stream.Streamable
 */
@SuppressWarnings("unused")
public class Properties implements Iterable<Property>, Streamable<Property> {

  /**
   * Factory method used to construct a new, empty instance of {@link Properties}.
   *
   * @return a new {@link Properties} containing no {@link Property properties}.
   */
  public static @NotNull Properties empty() {
    return new Properties();
  }

  /**
   * Factory method used to construct a new instance of {@link Properties} from the given, required {@link BeanModel}
   * that contains {@link Property properties} of the bean.
   *
   * @param beanModel {@link BeanModel} model describing (modeling) the bean.
   * @return a new {@link Properties} containing all the {@link Property properties} of the bean
   * represented by the given, required {@link BeanModel model}.
   * @throws IllegalArgumentException if the {@link BeanModel} is {@literal null}.
   * @see org.cp.elements.beans.model.AbstractPropertyFactory
   * @see org.cp.elements.beans.model.BeanModel
   * @see #of(Iterable)
   */
  public static @NotNull Properties from(@NotNull BeanModel beanModel) {

    Assert.notNull(beanModel, "BeanModel is required");

    BeanInfo beanInfo = beanModel.getBeanInfo();

    PropertyDescriptor[] propertyDescriptors =
      ArrayUtils.nullSafeArray(beanInfo.getPropertyDescriptors(), PropertyDescriptor.class);

    return of(Arrays.stream(propertyDescriptors)
      .filter(Objects::nonNull)
      .map(propertyDescriptor -> AbstractPropertyFactory.create(beanModel, propertyDescriptor))
      .collect(Collectors.toSet()));
  }

  /**
   * Factory method used to construct a new instance of {@link Properties}
   * from the given array of {@link Property properties}.
   *
   * @param properties array of {@link Property properties} contained by this collection.
   * @return a new {@link Properties} containing the array of {@link Property properties}.
   * @see org.cp.elements.beans.model.Property
   * @see #Properties(Property...)
   * @see #of(Iterable)
   */
  public static @NotNull Properties of(Property... properties) {
    return new Properties(properties);
  }

  /**
   * Factory method used to construct a new instance of {@link Properties}
   * from the given {@link Iterable} of {@link Property properties}.
   *
   * @param properties {@link Iterable} of {@link Property properties} contained by this collection.
   * @return a new {@link Properties} containing the {@link Iterable} of {@link Property properties}.
   * @see org.cp.elements.beans.model.Property
   * @see java.lang.Iterable
   * @see #of(Property...)
   */
  public static @NotNull Properties of(Iterable<Property> properties) {
    return of(ArrayUtils.asArray(properties, Property.class));
  }

  /**
   * Factory method usec to construct a new isntance of {@link Properties}
   * from the given {@link Stream} of {@link Property properties}.
   *
   * @param properties {@link Stream} of {@link Property properties} contained by this collection.
   * @return a new {@link Properties}.
   * @see org.cp.elements.beans.model.Property
   * @see java.util.stream.Stream
   * @see #of(Iterable)
   */
  public static @NotNull Properties of(Stream<Property> properties) {
    return of(StreamUtils.nullSafeStream(properties).collect(Collectors.toSet()));
  }

  private final Set<Property> properties;

  /**
   * Constructs a new instance of {@link Properties} initialized from the given array of {@link Property properties}.
   *
   * @param properties array of {@link Property properties} used to initialize this collection.
   * @see org.cp.elements.beans.model.Property
   */
  protected Properties(Property... properties) {
    this.properties = new TreeSet<>(CollectionUtils.asSet(properties));
  }

  /**
   * Gets a {@link Set} of {@link Property properties} contained in this collection.
   *
   * @return a {@link Set} of {@link Property properties} contained in this collection.
   * @see org.cp.elements.beans.model.Property
   * @see java.util.Set
   */
  protected @NotNull Set<Property> getProperties() {
    return Collections.unmodifiableSet(this.properties);
  }

  /**
   * Determines whether these {@link Properties} contains the given {@link Property}.
   *
   * @param property {@link Property} to evaluate.
   * @return a boolean value indicating whether the given {@link Property} is in
   * this set of {@link Properties}.
   * @see org.cp.elements.beans.model.Property
   */
  @NullSafe
  public boolean contains(@Nullable Property property) {
    return property != null && getProperties().contains(property);
  }

  /**
   * Finds all {@link Property properties} in this collection matching the given {@link Predicate}.
   *
   * If the given {@link Predicate} is {@literal null}, then this method behaves as if there is no {@link Predicate}
   * in the query used to match {@link Property properties} and therefore matches all {@link Property properites}.
   *
   * @param predicate {@link Predicate} used to match {@link Property properties} in this collection.
   * @return a {@link Stream} of {@link Property properties} that match the {@link Predicate}.
   * @see java.util.function.Predicate
   * @see java.util.stream.Stream
   * @see #getProperties()
   */
  public @NotNull Stream<Property> findBy(@Nullable Predicate<Property> predicate) {

    return getProperties().stream()
      .filter(FunctionUtils.nullSafePredicateMatchAll(predicate));
  }

  /**
   * Finds an individual {@link Property} identify by the given {@link String name}.
   *
   * @param name {@link String} containing the {@literal name} of the {@link Property} to find.
   * @return a single {@link Property} identify by the given {@link String name}.
   * @throws PropertyNotFoundException if a {@link Property} with the given {@link String name}
   * could not be found in this collection.
   * @see org.cp.elements.beans.model.Property
   * @see Property#getName()
   * @see #findBy(Predicate)
   */
  public @NotNull Property findByName(@Nullable String name) {

    return findBy(property -> property.getName().equals(name)).findFirst()
      .orElseThrow(() -> new PropertyNotFoundException(String.format("Property with name [%s] not found", name)));
  }

  /**
   * Finds all bean {@link Property properties} of the given {@link Class type}.
   *
   * @param type {@link Class} type used to match the {@link Property properties}
   * having an assignable {@link Class type}.
   * @return all {@link Property properties} in this collection assignable to
   * the given {@link Class type}.
   * @see Property#getType()
   * @see #findBy(Predicate)
   */
  public @NotNull Properties findByType(@NotNull Class<?> type) {

    return Properties.of(findBy(property -> ClassUtils.assignableTo(property.getType(), type))
      .collect(Collectors.toSet()));
  }

  /**
   * Finds all readable {@link Properties}.
   *
   * @return all readable {@link Properties}.
   * @see Property#isReadable()
   * @see #findBy(Predicate)
   * @see #findWritable()
   */
  public @NotNull Properties findReadable() {
    return Properties.of(findBy(Property::isReadable).collect(Collectors.toSet()));
  }

  /**
   * Finds all required {@link Properties}.
   *
   * @return all required {@link Properties}.
   * @see Property#isRequired()
   * @see #findBy(Predicate)
   */
  public @NotNull Properties findRequired() {
    return Properties.of(findBy(Property::isRequired).collect(Collectors.toSet()));
  }

  /**
   * Finds all {@literal serializable} {@link Properties}.
   *
   * {@link Properties} that are {@link Property#isReadable()} and is not {@link Property#isTransient()}
   * are considered {@literal serializable}. However, this method does not take into account
   * the {@link Property#getType()} and whether the {@link Class type} of the {@link Property}
   * implements the {@link java.io.Serializable} interface, or is generally {@literal serializable}
   * given there are multiple methods for serializing a value, not limited simply to Java Serialization.
   *
   * @return all {@literal serializable} {@link Properties}.
   * @see Property#isSerializable()
   * @see #findBy(Predicate)
   */
  public @NotNull Properties findSerializable() {
    return Properties.of(findBy(Property::isSerializable).collect(Collectors.toSet()));
  }

  /**
   * Finds all transient {@link Properties}.
   *
   * @return all transient {@link Properties}.
   * @see Property#isTransient()
   * @see #findBy(Predicate)
   */
  public @NotNull Properties findTransient() {
    return Properties.of(findBy(Property::isTransient).collect(Collectors.toSet()));
  }

  /**
   * Finds all writable {@link Properties}.
   *
   * @return all writable {@link Properties}.
   * @see Property#isWritable()
   * @see #findBy(Predicate)
   * @see #findReadable()
   */
  public @NotNull Properties findWritable() {
    return Properties.of(findBy(Property::isWritable).collect(Collectors.toSet()));
  }

  /**
   * Iterates over the {@link Property properties} in this collection.
   *
   * @return an unmodifiable {@link Iterator} over the {@link Property properties} in this collection.
   * @see org.cp.elements.beans.model.Property
   * @see java.util.Iterator
   * @see #getProperties()
   */
  @Override
  public @NotNull Iterator<Property> iterator() {
    return Collections.unmodifiableSet(getProperties()).iterator();
  }

  /**
   * Returns a {@link Stream} of {@link Property properties} in this collection.
   *
   * @return a {@link Stream} of {@link Property properties} in this collection.
   * @see java.util.stream.Stream
   * @see #iterator()
   */
  @Override
  public @NotNull Stream<Property> stream() {
    return StreamSupport.stream(this.spliterator(), false);
  }
}
