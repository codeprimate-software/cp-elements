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
package org.cp.elements.util.stream;

import java.util.Arrays;
import java.util.Collection;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.CollectionUtils;

/**
 * Abstract utility class containing a collection of methods for processing Java 8 {@link Stream Streams}.
 *
 * @author John J. Blum
 * @see java.util.Arrays
 * @see java.util.stream.Stream
 * @see java.util.stream.StreamSupport
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class StreamUtils {

  /**
   * Returns an empty {@link Stream}.
   *
   * @param <T> {@link Class type} of elements in the {@link Stream}.
   * @return an empty {@link Stream}.
   * @see java.util.stream.Stream
   * @see #stream(Object[])
   */
  @SuppressWarnings("all")
  public static <T> Stream<T> empty() {
    return stream();
  }

  /**
   * Null-safe method used to evaluate a given {@link Stream} for a {@literal null} reference.
   *
   * @param <T> {@link Class type} of elements in the {@link Stream}.
   * @param stream {@link Stream} reference to evaluate for {@literal null}.
   * @return the given {@link Stream} if not {@literal null}
   * or return an {@literal empty} {@link Stream} otherwise.
   * @see java.util.stream.Stream
   * @see #empty()
   */
  @NullSafe
  public static @NotNull <T> Stream<T> nullSafeStream(@Nullable Stream<T> stream) {
    return stream != null ? stream : empty();
  }

  /**
   * Returns a {@link Stream} from the given array of elements.
   *
   * @param <T> {@link Class type} of elements in the array.
   * @param elements array of elements to convert into a {@link Stream}.
   * @return a {@link Stream} containing the elements in the array.
   * @see java.util.stream.Stream
   */
  @SafeVarargs
  public static @NotNull <T> Stream<T> stream(T... elements) {
    return Arrays.stream(elements);
  }

  /**
   * Streams the contents from the given, required {@link Enumeration}.
   *
   * @param <T> {@link Class type} of the {@link Object elements} contained in the {@link Enumeration}.
   * @param enumeration {@link Enumeration} to stream; must not be {@literal null}.
   * @return a {@link Stream} over the contents of the given, required {@link Enumeration}.
   * @throws IllegalArgumentException if the {@link Enumeration} is {@literal null}.
   * @see java.util.stream.Stream
   * @see java.util.Enumeration
   */
  public static @NotNull <T> Stream<T> stream(@NotNull Enumeration<T> enumeration) {
    return stream(CollectionUtils.asIterable(ObjectUtils.requireObject(enumeration, "Enumeration is required")));
  }

  /**
   * Returns a {@link Stream} from the given {@link Iterable} collection of elements.
   *
   * @param <T> {@link Class type} of elements in the {@link Iterable}.
   * @param iterable {@link Iterable} collection of elements to convert into a {@link Stream}.
   * @return a {@link Stream} containing the elements in the {@link Iterable}.
   * @throws IllegalArgumentException if the {@link Iterable} is null.
   * @see java.util.stream.Stream
   * @see java.lang.Iterable
   */
  public static @NotNull <T> Stream<T> stream(@NotNull Iterable<T> iterable) {

    Assert.notNull(iterable, "Iterable is required");

    return iterable instanceof Collection<T> collection ? collection.stream()
      : StreamSupport.stream(iterable.spliterator(), false);
  }

  /**
   * Streams the contents from the given, required {@link Iterator}.
   *
   * @param <T> {@link Class type} of the {@link Object elements} contained in the {@link Iterator}.
   * @param iterator {@link Iterator} to stream; must not be {@literal null}.
   * @return a {@link Stream} over the contents of the given, required {@link Iterator}.
   * @throws IllegalArgumentException if the {@link Iterator} is {@literal null}.
   * @see java.util.stream.Stream
   * @see java.util.Iterator
   */
  public static @NotNull <T> Stream<T> stream(@NotNull Iterator<T> iterator) {
    return stream(CollectionUtils.asIterable(ObjectUtils.requireObject(iterator, "Iterator is required")));
  }

  /**
   * Converts the given {@link Stream} into a {@link List}.
   *
   * @param <T> {@link Class type} of elements in the {@link Stream}.
   * @param stream {@link Stream} to convert into a {@link List}.
   * @return a {@link List} all elements from the given {@link Stream}.
   * @see java.util.stream.Stream
   * @see java.util.List
   */
  public static @NotNull <T> List<T> toList(@Nullable Stream<T> stream) {
    return toList(stream, arg -> true, Function.identity());
  }

  /**
   * Converts the given {@link Stream} into a {@link List}.
   *
   * @param <T> {@link Class type} of elements in the {@link Stream}.
   * @param <R> {@link Class type} of elements in the resulting {@link List}.
   * @param stream {@link Stream} to convert into a {@link List}.
   * @param function {@link Function} used to transform elements in the {@link Stream}.
   * @return a {@link List} containing transformed elements from the given {@link Stream}.
   * @see java.util.function.Function
   * @see java.util.stream.Stream
   * @see java.util.List
   */
  public static @NotNull <T, R> List<R> toList(@Nullable Stream<T> stream, @NotNull Function<T, R> function) {
    return toList(stream, arg -> true, function);
  }

  /**
   * Converts the given {@link Stream} into a {@link List}.
   *
   * @param <T> {@link Class type} of elements in the {@link Stream}.
   * @param stream {@link Stream} to convert into a {@link List}.
   * @param predicate {@link Predicate} used to filter elements in the {@link Stream}.
   * @return a {@link List} containing filtered elements from the given {@link Stream}.
   * @see java.util.function.Predicate
   * @see java.util.stream.Stream
   * @see java.util.List
   */
  public static @NotNull <T> List<T> toList(@Nullable Stream<T> stream, @NotNull Predicate<T> predicate) {
    return toList(stream, predicate, Function.identity());
  }

  /**
   * Converts the given {@link Stream} into a {@link List}.
   *
   * @param <T> {@link Class type} of elements in the {@link Stream}.
   * @param <R> {@link Class type} of elements in the resulting {@link List}.
   * @param stream {@link Stream} to convert into a {@link List}.
   * @param predicate {@link Predicate} used to filter elements in the {@link Stream}.
   * @param function {@link Function} used to transform elements in the {@link Stream}.
   * @return a {@link List} containing filtered, transformed elements from the given {@link Stream}.
   * @see java.util.function.Function
   * @see java.util.function.Predicate
   * @see java.util.stream.Stream
   * @see java.util.List
   */
  @SuppressWarnings("unchecked")
  public static @NotNull <T, R> List<R> toList(@Nullable Stream<T> stream,
      @NotNull Predicate<T> predicate, @NotNull Function<T, R> function) {

    return (List<R>) nullSafeStream(stream)
      .filter(resolve(predicate))
      .map(resolve(function))
      .collect(Collectors.toList());
  }

  /**
   * Converts the given {@link Stream} into a {@link Set}.
   *
   * @param <T> {@link Class type} of elements in the {@link Stream}.
   * @param stream {@link Stream} to convert into a {@link Set}.
   * @return a {@link Set} containing all elements from the given {@link Stream}.
   * @see java.util.stream.Stream
   * @see java.util.Set
   */
  public static @NotNull <T> Set<T> toSet(@Nullable Stream<T> stream) {
    return toSet(stream, args -> true, Function.identity());
  }

  /**
   * Converts the given {@link Stream} into a {@link Set}.
   *
   * @param <T> {@link Class type} of elements in the {@link Stream}.
   * @param <R> {@link Class type} of elements in the resulting {@link Set}.
   * @param stream {@link Stream} to convert into a {@link Set}.
   * @param function {@link Function} used to transform elements in the {@link Stream}.
   * @return a {@link Set} containing all transformed elements from the given {@link Stream}.
   * @see java.util.function.Function
   * @see java.util.stream.Stream
   * @see java.util.Set
   */
  public static @NotNull <T, R> Set<R> toSet(@Nullable Stream<T> stream, @NotNull Function<T, R> function) {
    return toSet(stream, args -> true, function);
  }

  /**
   * Converts the given {@link Stream} into a {@link Set}.
   *
   * @param <T> {@link Class type} of elements in the {@link Stream}.
   * @param stream {@link Stream} to convert into a {@link Set}.
   * @param predicate {@link Predicate} used to filter elements in the {@link Stream}.
   * @return a {@link Set} containing filtered elements from the given {@link Stream}.
   * @see java.util.function.Predicate
   * @see java.util.stream.Stream
   * @see java.util.Set
   */
  public static @NotNull <T> Set<T> toSet(@Nullable Stream<T> stream, @NotNull Predicate<T> predicate) {
    return toSet(stream, predicate, Function.identity());
  }

  /**
   * Converts the given {@link Stream} into a {@link Set}.
   *
   * @param <T> {@link Class type} of elements in the {@link Stream}.
   * @param <R> {@link Class type} of elements in the resulting {@link Set}.
   * @param stream {@link Stream} to convert into a {@link Set}.
   * @param predicate {@link Predicate} used to filter elements in the {@link Stream}.
   * @param function {@link Function} used to transform elements in the {@link Stream}.
   * @return a {@link Set} containing filtered, transformed elements from the given {@link Stream}.
   * @see java.util.function.Function
   * @see java.util.function.Predicate
   * @see java.util.stream.Stream
   * @see java.util.Set
   */
  @SuppressWarnings("unchecked")
  public static @NotNull <T, R> Set<R> toSet(@Nullable Stream<T> stream,
      @NotNull Predicate<T> predicate, @NotNull Function<T, R> function) {

    return (Set<R>) nullSafeStream(stream)
      .filter(resolve(predicate))
      .map(resolve(function))
      .collect(Collectors.toSet());
  }

  private static <T, R> Function<T, ?> resolve(@Nullable Function<T, R> function) {
    return function != null ? function : Function.identity();
  }

  private static <T> Predicate<T> resolve(Predicate<T> predicate) {
    return predicate != null ? predicate : arg -> true;
  }
}
