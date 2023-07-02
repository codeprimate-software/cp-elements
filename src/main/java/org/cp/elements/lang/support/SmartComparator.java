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
package org.cp.elements.lang.support;

import java.io.Serializable;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Predicate;

import org.cp.elements.function.FunctionUtils;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.Integers;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.OrderUtils;
import org.cp.elements.lang.Ordered;
import org.cp.elements.lang.Registry;
import org.cp.elements.lang.annotation.Dsl;
import org.cp.elements.lang.annotation.FluentApi;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.support.SmartComparator.ComparatorDescriptor;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.stream.StreamUtils;

/**
 * {@link Comparator} implementation capable of comparing {@link Object Objects} using a variety of strategies.
 * <p>
 * It should be noted that since {@link Comparator} objects are not {@link Serializable} by default,
 * then the "registered" {@link Comparator Comparators} by {@link Class type} are not serialized.
 *
 * @author John Blum
 * @see java.lang.Iterable
 * @see java.io.Serializable
 * @see java.util.Comparator
 * @see org.cp.elements.lang.annotation.FluentApi
 * @see org.cp.elements.lang.support.SmartComparator.ComparatorDescriptor
 * @since 1.0.0
 */
@FluentApi
public class SmartComparator implements Comparator<Object>, Iterable<ComparatorDescriptor>, Serializable {

  private static final long serialVersionUID = -1851003770115084180L;

  /**
   * Factory method used to construct a new {@link SmartComparator}.
   *
   * @return a new {@link SmartComparator}.
   */
  @Dsl
  public static @NotNull SmartComparator newSmartComparator() {
    return new SmartComparator();
  }

  private final transient ComparatorRegistry comparatorRegistry = new ComparatorRegistry();

  /**
   * Smartly {@link Comparator#compare(Object, Object) compares} two {@link Object objects}.
   * <p>
   * First, an attempt is made to {@link #findByExactType(Class) find} a {@link Comparator}
   * {@link #register(Comparator) registered} with this {@link SmartComparator} that exactly matches one of the given
   * {@link Object object's} {@link Class type}. If such a {@link Comparator} is found, then the {@link Comparator}
   * is used to {@link Comparator#compare(Object, Object) compare} the {@link Object objects}.
   * <p>
   * Next, an attempt is made to {@link #findByCompatibleType(Class) find} a {@link Comparator}
   * {@link #register(Comparator) registered} with this {@link SmartComparator} that is compatible with one of
   * the given {@link Object object's} {@link Class type}. When the {@link Comparator}
   * is {@link #register(Comparator) registered}, the {@link ParameterizedType type parameter} of the {@link Comparator}
   * is inspected is used to determine if one of the {@link Object object's} {@link Class type}
   * is {@link Class#isAssignableFrom(Class) assignment compatible}. If such a {@link Comparator} is found,
   * then the {@link Comparator} is used to {@link Comparator#compare(Object, Object) compare}
   * the {@link Object objects}.
   * <p>
   * If an appropriate {@link Comparator} cannot be found to {@link Comparator#compare(Object, Object) compare}
   * the {@link Object objects}, and if either or both of the {@link Object objects} are {@link Comparable},
   * then the {@link Comparable Comparable objects} {@link Comparable#compareTo(Object)} methods is called.
   * <p>
   * Then, if either {@link Object} in the comparison is an instance of {@link String}, then this method attempts
   * to {@link Comparator#compare(Object, Object)} the {@link Object objects} using their
   * {@link String#valueOf(Object) String rpresentation}.
   * <p>
   * Finally, if none of the above methods were successful in comparing the {@link Object objects}, then any declared
   * ordering is applied, returning a {@link Ordered#DEFAULT default order}, if order was not declared, as the result
   * of this method.
   * <p>
   * Always orders {@literal null} values {@link Ordered#LAST last}.
   *
   * @param objectOne first {@link Object} in the comparison.
   * @param objectTwo second {@link Object} in the comparison.
   * @return a {@link Integer negative number} if the first {@link Object} is less than the second {@link Object},
   * returns a {@link Integer positive number} if the first {@link Object} is greater than the second {@link Object},
   * otherwise returns {@literal 0}.
   */
  @NullSafe
  @Override
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public int compare(@Nullable Object objectOne, @Nullable Object objectTwo) {

    if (TypeUtils.areMaybeComparable(objectOne, objectTwo)) {

      Comparator comparator = resolveComparator(objectOne, objectTwo);

      // First, attempt to use a registered Comparator
      if (comparator != null) {
        try {
          return comparator.compare(objectOne, objectTwo);
        }
        catch (ClassCastException ignore) {
          // Nope!
        }
      }

      // Next, attempt to use a Comparable object's compareTo(:Object) method
      if (TypeUtils.isComparable(objectOne)) {
        try {
          return ((Comparable) objectOne).compareTo(objectTwo);
        }
        catch (ClassCastException tryAgain) {
          if (TypeUtils.isComparable(objectTwo)) {
            try {
              return Integers.invert(((Comparable) objectTwo).compareTo(objectOne));
            }
            catch (ClassCastException ignore) {
              // "Oh well, whatever, never mind" - Nirvana, Smells Like Teen Spirit
            }
          }
        }
      }
    }

    // Then, if either Object in the comparison is a String, then compare their String representations.
    if (TypeUtils.isString(objectOne) || TypeUtils.isString(objectTwo)) {
      return objectOne == null ? 1
        : objectTwo == null ? -1
        : String.valueOf(objectOne).compareTo(String.valueOf(objectTwo));
    }

    // And then, if either Object in the comparison is a Number, then compare their numeric values.
    if (TypeUtils.isNumber(objectOne) && TypeUtils.isNumber(objectTwo)) {
      double doubleValueOne = ((Number) objectOne).doubleValue();
      double doubleValueTwo = ((Number) objectTwo).doubleValue();
      return Double.compare(doubleValueOne, doubleValueTwo);
    }

    // Finally, if all else fails, compare (order) the objects using declared or intrinsic ordering,
    // falling back on a default order
    return Integer.compare(OrderUtils.getOrder(objectOne), OrderUtils.getOrder(objectTwo));
  }

  @SuppressWarnings("rawtypes")
  private @Nullable Comparator resolveComparator(@NotNull Object objectOne, @NotNull Object objectTwo) {

    Comparator comparator = findByExactType(objectOne.getClass())
      .orElseGet(() -> findByExactType(objectTwo.getClass()).orElse(null));

    comparator = comparator != null ? comparator
      : findByCompatibleType(objectOne.getClass())
      .orElseGet(() -> findByCompatibleType(objectTwo.getClass()).orElse(null));

    return comparator;
  }

  /**
   * Finder methods used to search for and return a {@link ComparatorDescriptor} matching the given {@link Predicate}.
   *
   * @param comparatorDescriptorPredicate {@link Predicate} used to search for and match on
   * the requested {@link ComparatorDescriptor}; must not be {@literal null}.
   * @return an {@link Optional} {@link ComparatorDescriptor} matching the given {@link Predicate}.
   * @see java.util.function.Predicate
   * @see ComparatorDescriptor
   * @see java.util.Optional
   */
  private Optional<ComparatorDescriptor> findBy(
      @NotNull Predicate<ComparatorDescriptor> comparatorDescriptorPredicate) {

    return StreamUtils.stream(this)
      .filter(Objects::nonNull)
      .filter(comparatorDescriptor -> FunctionUtils.nullSafePredicateMatchNone(comparatorDescriptorPredicate)
        .test(comparatorDescriptor))
      .findFirst();
  }

  /**
   * Finds an {@link Optional Optionally} registered {@link ComparatorDescriptor}
   * describing the given {@link Comparator}.
   *
   * @param comparator {@link Comparator} used to find a registered {@link ComparatorDescriptor descriptor}.
   * @return an {@link Optional Optionally} registered {@link ComparatorDescriptor}
   * describing the given {@link Comparator}.
   * @see org.cp.elements.lang.support.SmartComparator.ComparatorDescriptor
   * @see java.util.Comparator
   * @see java.util.Optional
   */
  @NullSafe
  public Optional<ComparatorDescriptor> findByComparator(@Nullable Comparator<?> comparator) {
    return findBy(comparatorDescriptor -> comparatorDescriptor.getComparator().equals(comparator));
  }

  /**
   * Finds a registered {@link Comparator} based on whether the given {@link Class type} is compatible (assignable)
   * to the {@link ParameterizedType type parameter} of the registered {@link Comparator}.
   *
   * @param type {@link Class type} used to find a registered, compatible {@link Comparator}.
   * @return a registered {@link Comparator} based on whether the given {@link Class type} is compatible (assignable)
   * to the {@link ParameterizedType type parameter} of the registered {@link Comparator}.
   * @see ComparatorDescriptor#isCompatibleType(Class)
   * @see java.util.Comparator
   * @see java.util.Optional
   * @see #findBy(Predicate)
   */
  @NullSafe
  public Optional<Comparator<?>> findByCompatibleType(@Nullable Class<?> type) {
    return findBy(comparatorDescriptor -> comparatorDescriptor.isCompatibleType(type))
      .map(ComparatorDescriptor::getComparator);
  }

  /**
   * Finds a registered {@link Comparator} based on whether the given {@link Class type} is exactly the same as
   * the {@link ParameterizedType type parameter} of the registered {@link Comparator}.
   *
   * @param type {@link Class type} used to find a registered, exact {@link Comparator}.
   * @return a registered {@link Comparator} based on whether the given {@link Class type} is exactly the same as
   * the {@link ParameterizedType type parameter} of the registered {@link Comparator}.
   * @see ComparatorDescriptor#isExactType(Class)
   * @see java.util.Comparator
   * @see java.util.Optional
   * @see #findBy(Predicate)
   */
  @NullSafe
  public Optional<Comparator<?>> findByExactType(@Nullable Class<?> type) {
    return findBy(comparatorDescriptor -> comparatorDescriptor.isExactType(type))
      .map(ComparatorDescriptor::getComparator);
  }

  /**
   * Iterates over the {@link ComparatorDescriptor} objects for all {@link Comparator Comparators} registered with
   * this {@link SmartComparator}.
   *
   * @return an {@link Iterator} over the {@link ComparatorDescriptor} objects for all {@link Comparator Comparators}
   * registered with this {@link SmartComparator}.
   * @see org.cp.elements.lang.support.SmartComparator.ComparatorDescriptor
   * @see java.util.Iterator
   */
  @Override
  public @NotNull Iterator<ComparatorDescriptor> iterator() {
    return this.comparatorRegistry.iterator();
  }

  /**
   * Registers the given, required {@link Comparator} with this {@link SmartComparator} to be called upon
   * for {@link #compare(Object, Object) comparing} {@link Object objects}.
   *
   * @param comparator {@link Comparator} to register; must not be {@literal null}.
   * @return a boolean value indicating whether the given, required {@link Comparator} was successfully
   * registered with this {@link SmartComparator}
   * @see #withRegistrationFor(Comparator)
   * @see #unregister(Comparator)
   * @see java.util.Comparator
   */
  @NullSafe
  public boolean register(@NotNull Comparator<?> comparator) {

    return comparator != null
      && this.comparatorRegistry.<ComparatorRegistry>register(ComparatorDescriptor.from(comparator))
        .isRegistered(comparator);
  }

  /**
   * Unregisters the given {@link Comparator} from this {@link SmartComparator} if not {@literal null}
   * and the given {@link Comparator} was {@link #register(Comparator) registered} in the first place.
   *
   * @param comparator {@link Comparator} to unregister; must not be {@literal null}.
   * @return a boolean value indicating whether the given, required {@link Comparator} was successfully
   * unregistered from this {@link SmartComparator}
   * @see #unregister(ComparatorDescriptor)
   * @see #findByComparator(Comparator)
   * @see #register(Comparator)
   * @see java.util.Comparator
   */
  @NullSafe
  public boolean unregister(@NotNull Comparator<?> comparator) {

    return findByComparator(comparator)
      .filter(this::unregister)
      .isPresent();
  }

  /**
   * Unregisters the given, required {@link ComparatorDescriptor} from this {@link SmartComparator}.
   * <p>
   * Internally, {@link Comparator Comparators} are described and registered as
   * {@link ComparatorDescriptor descriptors}.
   *
   * @param comparatorDescriptor {@link ComparatorDescriptor} to unregister
   * @return a boolean value indicating whether the given {@link ComparatorDescriptor} was successfully
   * unregistered from this {@link SmartComparator}.
   * @see org.cp.elements.lang.support.SmartComparator.ComparatorDescriptor
   * @see #unregister(Comparator)
   */
  private boolean unregister(@NotNull ComparatorDescriptor comparatorDescriptor) {
    return comparatorDescriptor != null
      && !this.comparatorRegistry.unregister(comparatorDescriptor).isRegistered(comparatorDescriptor);
  }

  /**
   * Builder method used to register the given, required {@link Comparator} with this {@link SmartComparator}.
   *
   * @param comparator {@link Comparator} to register; must not be {@literal null}.
   * @return this {@link SmartComparator}.
   * @see org.cp.elements.lang.annotation.Dsl
   * @see #register(Comparator)
   * @see java.util.Comparator
   */
  @Dsl
  public @NotNull SmartComparator withRegistrationFor(@NotNull Comparator<?> comparator) {
    register(comparator);
    return this;
  }

  /**
   * Abstract Data Type (ADT) used to {@literal describe} a {@link Comparator}.
   *
   * @see java.util.Comparator
   */
  protected static class ComparatorDescriptor {

    /**
     * Factory method used to construct a new {@link ComparatorDescriptor} initialized with the given,
     * required {@link Comparator} that will be described by the {@link ComparatorDescriptor descriptor}.
     *
     * @param comparator {@link Comparator} to describe with the {@link ComparatorDescriptor descriptor};
     * must not be {@literal null}.
     * @return a new {@link ComparatorDescriptor} describing the {@link Comparator}.
     * @throws IllegalArgumentException if the {@link Comparator} is {@literal null}.
     * @see java.util.Comparator
     */
    protected static @NotNull ComparatorDescriptor from(@NotNull Comparator<?> comparator) {
      return new ComparatorDescriptor(comparator);
    }

    private final AtomicReference<Class<?>> lazyResolvedType = new AtomicReference<>(null);

    private final Comparator<?> comparator;

    /**
     * Constructs a new instance of {@link ComparatorDescriptor} initialized with the given,
     * required {@link Comparator} that will be described by this descriptor.
     *
     * @param comparator {@link Comparator} to describe with this descriptor; must not be {@literal null}.
     * @throws IllegalArgumentException if the {@link Comparator} is {@literal null}.
     * @see java.util.Comparator
     */
    protected ComparatorDescriptor(@NotNull Comparator<?> comparator) {
      this.comparator = ObjectUtils.requireObject(comparator, "Comparator is required");
    }

    /**
     * Returns a reference to the {@link Comparator} represented by this {@link ComparatorDescriptor descriptor}.
     *
     * @return a reference to the {@link Comparator} represented by this {@link ComparatorDescriptor descriptor}.
     * @see java.util.Comparator
     */
    protected @NotNull Comparator<?> getComparator() {
      return this.comparator;
    }

    /**
     * Gets the resolved {@link Class type} of the {@link Comparator Comparator's} type parameter.
     * <p>
     * For instance, if a user's {@literal CustomComparator} implements {@link Comparator Comparator&lt;User&gt;},
     * then this method returns {@literal User}.
     *
     * @return the resolved {@link Class type} of the {@link Comparator Comparator's} type parameter.
     * @see #resolveComparatorTypeParameterType()
     */
    protected @NotNull Class<?> getType() {
      return this.lazyResolvedType.updateAndGet(it -> it != null ? it : resolveComparatorTypeParameterType());
    }

    /**
     * Determines whether the {@link Comparator Comparator's} type parameter {@link Class type} is compatible with,
     * or {@link Class#isAssignableFrom(Class) assignable from} the given {@link Class type}.
     *
     * @param type {@link Class type} to evaluate for assignment compatibility; must not be {@literal null}.
     * @return a boolean value indicating whether the {@link Comparator Comparator's} type parameter {@link Class type}
     * is compatible with, or {@link Class#isAssignableFrom(Class) assignable from} the given {@link Class type}.
     * @see java.lang.Class#isAssignableFrom(Class)
     */
    @NullSafe
    protected boolean isCompatibleType(@NotNull Class<?> type) {
      return type != null && getType().isAssignableFrom(type);
    }

    /**
     * Determines whether the {@link Comparator Comparator's} type parameter {@link Class type} is equal to
     * the given {@link Class type}.
     *
     * @param type {@link Class type} to evaluate.
     * @return a boolean value indicating whether the {@link Comparator Comparator's} type parameter
     * {@link Class type} is equal to the given {@link Class type}.
     * @see java.lang.Class#equals(Object)
     */
    @NullSafe
    protected boolean isExactType(@Nullable Class<?> type) {
      return getType().equals(type);
    }

    @SuppressWarnings("all")
    private @NotNull Class<?> resolveComparatorTypeParameterType() {

      Comparator<?> comparator = getComparator();

      ParameterizedType parameterizedType =
        Arrays.stream(ArrayUtils.nullSafeArray(comparator.getClass().getGenericInterfaces(), Type.class))
          .filter(TypeUtils::isParameterizedComparatorType)
          .findFirst()
          .map(ParameterizedType.class::cast)
          .orElseGet(() -> {

            Type genericSuperclass = comparator.getClass().getGenericSuperclass();

            return TypeUtils.isParameterizedComparatorType(genericSuperclass)
              ? (ParameterizedType) genericSuperclass
              : null;

          });

      Assert.notNull(parameterizedType, "Comparator [%s] was not properly parameterized",
        comparator.getClass().getName());

      Type[] actualTypeParameters = parameterizedType.getActualTypeArguments();

      Assert.isTrue(actualTypeParameters.length > 0,
        "Expected the parameterized type [%s] to have at least 1 type parameter",
        parameterizedType.getClass().getName());

      return ObjectUtils.toRawType(actualTypeParameters[0]);
    }

    @Override
    public boolean equals(Object obj) {

      if (this == obj) {
        return true;
      }

      if (!(obj instanceof ComparatorDescriptor)) {
        return false;
      }

      ComparatorDescriptor that = (ComparatorDescriptor) obj;

      return ObjectUtils.equals(this.getComparator(), that.getComparator())
        && ObjectUtils.equalsIgnoreNull(this.lazyResolvedType.get(), that.lazyResolvedType.get());
    }

    @Override
    public int hashCode() {
      return ObjectUtils.hashCodeOf(getComparator(), this.lazyResolvedType.get());
    }
  }

  /**
   * {@link Registry} implementation for {@link Comparator} objects wrapped as a {@link ComparatorDescriptor}.
   *
   * @see org.cp.elements.lang.support.SmartComparator.ComparatorDescriptor
   * @see org.cp.elements.lang.Registry
   * @see java.util.Comparator
   */
  protected static class ComparatorRegistry implements Registry<ComparatorDescriptor> {

    private final transient Set<ComparatorDescriptor> registry = Collections.synchronizedSet(new HashSet<>());

    /**
     * Determines whether the given {@link Comparator} has been registered with this {@link Registry}.
     *
     * @param comparator {@link Comparator} to evaluate for registration.
     * @return a boolean value indicating whether the given {@link Comparator} has been registered with
     * this {@link Registry}.
     * @see java.util.Comparator
     */
    protected boolean isRegistered(@Nullable Comparator<?> comparator) {

      return comparator != null && StreamUtils.stream(this)
        .anyMatch(comparatorDescriptor -> comparatorDescriptor.getComparator().equals(comparator));
    }

    @Override
    public Iterator<ComparatorDescriptor> iterator() {
      return Collections.unmodifiableSet(this.registry).iterator();
    }

    @Override
    @SuppressWarnings("unchecked")
    public @NotNull <R extends Registry<ComparatorDescriptor>> R register(
      @NotNull ComparatorDescriptor comparatorDescriptor) {

      if (comparatorDescriptor != null) {
        this.registry.add(comparatorDescriptor);
      }

      return (R) this;
    }

    @Override
    @SuppressWarnings("unchecked")
    public @NotNull <R extends Registry<ComparatorDescriptor>> R unregister(
      @NotNull ComparatorDescriptor comparatorDescriptor) {

      if (comparatorDescriptor != null) {
        this.registry.remove(comparatorDescriptor);
      }

      return (R) this;
    }
  }

  /**
   * {@link Comparator} implementation of {@link Comparable} objects.
   *
   * @param <T> {@link Comparable} {@link Class type} of the {@link Object objects} in the comparison
   * expected by this {@link Comparator}.
   * @see java.io.Serializable
   * @see java.lang.Comparable
   * @see java.util.Comparator
   */
  @SuppressWarnings({ "rawtypes" })
  public static class ComparableComparator<T extends Comparable<T>> implements Comparator<T>, Serializable {

    public static final ComparableComparator INSTANCE = new ComparableComparator();

    @Override
    public int compare(@NotNull T objectOne, @NotNull T objectTwo) {
      return objectOne.compareTo(objectTwo);
    }

    private @NotNull Object readResolve() {
      return INSTANCE;
    }
  }

  /**
   * {@link Comparator} implementation of an {@link Object Object's} {@link Object#hashCode() hash code}.
   *
   * @see java.io.Serializable
   * @see java.util.Comparator
   */
  @SuppressWarnings("unused")
  public static class HashCodeComparator implements Comparator<Object>, Serializable {

    public static final HashCodeComparator INSTANCE = new HashCodeComparator();

    @NullSafe
    @Override
    public int compare(@Nullable Object objectOne, @Nullable Object objectTwo) {
      return Integer.compare(ObjectUtils.hashCode(objectOne), ObjectUtils.hashCode(objectTwo));
    }

    private @NotNull Object readResolve() {
      return INSTANCE;
    }
  }

  @SuppressWarnings("unused")
  protected abstract static class TypeUtils extends ClassUtils {

    @NullSafe
    protected static boolean areMaybeComparable(@Nullable Object objectOne, @Nullable Object objectTwo) {
      return objectOne != null && objectTwo != null;
    }

    @NullSafe
    protected static boolean isComparable(@Nullable Object target) {
      return target instanceof Comparable;
    }

    @NullSafe
    protected static boolean isNumber(@Nullable Object target) {
      return target instanceof Number;
    }

    @NullSafe
    protected static boolean isParameterizedComparatorType(@Nullable Type type) {
      return type instanceof ParameterizedType && assignableTo(toRawType(type), Comparator.class);
    }

    @NullSafe
    protected static boolean isString(@Nullable Object target) {
      return target instanceof String;
    }
  }
}
