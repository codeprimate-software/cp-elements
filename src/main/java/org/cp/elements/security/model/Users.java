/*
 * Copyright 2017-Present Author or Authors.
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
package org.cp.elements.security.model;

import static org.cp.elements.lang.ElementsExceptionsFactory.newUserNotFoundException;

import java.util.Arrays;
import java.util.Comparator;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.cp.elements.function.FunctionUtils;
import org.cp.elements.lang.annotation.Dsl;
import org.cp.elements.lang.annotation.FluentApi;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.CollectionUtils;
import org.cp.elements.util.stream.StreamUtils;
import org.cp.elements.util.stream.Streamable;

/**
 * Abstract Data Type (ADT) defining an aggregate or collection of {@link User Users}.
 *
 * @author John Blum
 * @see java.lang.Iterable
 * @see java.lang.FunctionalInterface
 * @see org.cp.elements.lang.annotation.FluentApi
 * @see org.cp.elements.security.model.User
 * @see org.cp.elements.util.stream.Streamable
 * @since 2.0.0
 */
@FluentApi
@FunctionalInterface
public interface Users extends Iterable<User<?>>, Streamable<User<?>> {

  /**
   * Factory method used to construct an empty collection of {@link Users}.
   *
   * @return an empty collection of {@link Users}.
   * @see org.cp.elements.lang.annotation.Dsl
   * @see #of(User[])
   * @see #of(Iterable)
   */
  @Dsl
  static Users empty() {
    return of();
  }

  /**
   * Factory method used to construct {@link Users} initialized from the given array.
   *
   * @param array array of {@link User Users} used to initialize the collection.
   * @return a new {@link Users} initialized from the given array.
   * @see org.cp.elements.lang.annotation.Dsl
   * @see #of(Iterable)
   */
  @Dsl
  @NullSafe
  static Users of(User<?>... array) {
    return of(Arrays.asList(ArrayUtils.nullSafeArray(array, User.class)));
  }

  /**
   * Factory method used to construct {@link Users} initialized from the given {@link Iterable}.
   *
   * @param iterable {@link Iterable} collection of {@link User Users} used to initialize the collection.
   * @return a new {@link Users} initialized from the given array.
   * @see org.cp.elements.lang.annotation.Dsl
   * @see java.lang.Iterable
   * @see #of(User[])
   */
  @Dsl
  @NullSafe
  static Users of(Iterable<User<?>> iterable) {
    return CollectionUtils.nullSafeIterable(iterable)::iterator;
  }

  /**
   * Finds all {@link Users} matching the given {@link Predicate}.
   *
   * @param predicate {@link Predicate} used to match {@link Users} in this collection.
   * @return all {@link Users} matching the {@link Predicate}.
   * @see java.util.function.Predicate
   * @see #stream()
   */
  default Users findBy(@Nullable Predicate<User<?>> predicate) {
    return Users.of(stream().filter(FunctionUtils.nullSafePredicateMatchingNone(predicate)).toList());
  }

  /**
   * Finds a single {@link User} with the given {@link String name}.
   *
   * @param name {@link String} containing a name used to match a single {@link User}.
   * @return an {@link Optional} {@link User} from this collection matching the given {@link String name} if present.
   * @see org.cp.elements.security.model.User
   * @see #findOne(Predicate)
   * @see java.util.Optional
   */
  default Optional<User<?>> findByName(@Nullable String name) {
    return findOne(user -> user.getName().equals(name));
  }

  /**
   * Finds the first {@link User} in this collection matching the given {@link Predicate}.
   * <p>
   * If more than 1 {@link User} matches the given {@link Predicate}, then the {@link User} returned
   * is determined by the order of the {@link User Users} in this collection.
   * <p>
   * If the given {@link Predicate} is {@literal null}, then this method returns an {@link Optional#empty()}.
   *
   * @param predicate {@link Predicate} used to find the first matching {@link User} in this collection.
   * @return the first {@link Optional} {@link User} in this colletion matching the given {@link Predicate}.
   * @see org.cp.elements.security.model.User
   * @see java.util.function.Predicate
   * @see java.util.Optional
   * @see #stream()
   */
  default Optional<User<?>> findOne(@Nullable Predicate<User<?>> predicate) {
    return stream().filter(FunctionUtils.nullSafePredicateMatchingNone(predicate)).findFirst();
  }

  /**
   * Returns a single, required {@link User} matching the given {@link Predicate}.
   *
   * @param predicate {@link Predicate} used to match the {@link User}.
   * @return the first {@link User} matching the given {@link Predicate}.
   * @throws UserNotFoundException if no {@link User} matching the given {@link Predicate} exists.
   * @see org.cp.elements.security.model.User
   * @see java.util.function.Predicate
   * @see #findOne(Predicate)
   */
  @NullSafe
  default User<?> requireOne(@NotNull Predicate<User<?>> predicate) {
    return findOne(FunctionUtils.nullSafePredicateMatchingNone(predicate)).orElseThrow(() ->
      newUserNotFoundException("No User was found matching Predicate [%s]", predicate));
  }

  /**
   * Determines the number of {@link User Users} in this collection.
   *
   * @return an {@link Integer} specifying the number of {@link User Users} in this collection.
   */
  default int size() {
    return Long.valueOf(stream().count()).intValue();
  }

  /**
   * Sorts (orders) the {@link User Users} in this collection according to the given {@link Comparator},
   * returning a new collection of {@link Users}.
   *
   * @param comparator {@link Comparator} used to compare and order the {@link User Users} in this collection.
   * @return a new {@link Users} sorted with the given {@link Comparator}.
   * @see java.util.Comparator
   */
  default Users sort(Comparator<User<?>> comparator) {
    return Users.of(stream().sorted(comparator).toList());
  }

  /**
   * Streams the collection of {@link User Users} in this {@link Iterable}.
   *
   * @return a {@link Stream} of {@link User Users} in this collection.
   * @see org.cp.elements.security.model.User
   * @see java.util.stream.Stream
   */
  @NullSafe
  default Stream<User<?>> stream() {
    return StreamUtils.stream(this);
  }
}
