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
package org.cp.elements.data.oql.support;

import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalStateException;

import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.stream.Stream;

import org.cp.elements.data.oql.Oql;
import org.cp.elements.data.oql.Oql.GroupBy;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.util.stream.StreamUtils;
import org.cp.elements.util.stream.Streamable;

/**
 * Abstract Data Type (ADT) modeling a collection of {@link Group Groups}.
 *
 * @author John Blum
 * @param <T> {@link Class type} of {@link Object} being grouped.
 * @see java.lang.Iterable
 * @see org.cp.elements.data.oql.support.Group
 * @see org.cp.elements.util.stream.Streamable
 * @since 2.0.0
 */
@SuppressWarnings("unused")
public interface Groups<T> extends Iterable<Group<T>>, Streamable<Group<T>> {

  /**
   * Factory method used to construct a new {@link Groups} object initialized with the given {@link GroupBy} clause
   * defining the criteria used to determine the groups.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @param groupBy {@link GroupBy} clause defining the criteria to determine the groups.
   * @return a new collection of {@link Group Groups}.
   * @throws IllegalArgumentException if {@link GroupBy} is {@literal null}.
   * @see GroupBy
   */
  static <S, T> Groups<T> from(@NotNull Oql.GroupBy<S, T> groupBy) {

    Assert.notNull(groupBy, "GroupBy is required");

    Map<Integer, Group<T>> groups = new ConcurrentHashMap<>();

    return new Groups<>() {

      @Override
      public GroupBy<S, T> getGroupBy() {
        return groupBy;
      }

      @Override
      public Group<T> compute(T target) {
        int group = getGrouping().group(target);
        Function<Integer, Group<T>> mappingFunction = groupNumber -> Group.with(getGroupBy(), groupNumber);
        return groups.computeIfAbsent(group, mappingFunction);
      }

      @Override
      @SuppressWarnings("all")
      public Iterator<Group<T>> iterator() {
        return Collections.unmodifiableCollection(groups.values()).iterator();
      }
    };
  }

  /**
   * Factory method used to construct a new, non-operable {@link Groups} object.
   *
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @return a new, non-operable collection of {@link Group Groups}.
   */
  static <T> Groups<T> noop() {

    return new Groups<>() {

      @Override
      public GroupBy<?, T> getGroupBy() {
        throw newIllegalStateException("GroupBy not present");
      }

      @Override
      public Group<T> compute(T target) {
        throw newIllegalStateException("Cannot compute Group");
      }

      @Override
      public T group(T target) {
        return target;
      }

      @Override
      @SuppressWarnings("all")
      public Iterator<Group<T>> iterator() {
        return Collections.emptyIterator();
      }
    };
  }

  /**
   * Gets the {@link Oql.GroupBy} clause used to form the {@link Group Groups} in this collection.
   *
   * @return the {@link Oql.GroupBy} clause used to form the {@link Group Groups} in this collection.
   * @see org.cp.elements.data.oql.Oql.GroupBy
   */
  Oql.GroupBy<?, T> getGroupBy();

  /**
   * Gets the {@link Grouping} function used to determine the {@link Group} of an {@link Object}.
   *
   * @return the {@link Grouping} function used to determine the {@link Group} of an {@link Object}.
   * @see org.cp.elements.data.oql.support.Grouping
   * @see #getGroupBy()
   */
  default Grouping<T> getGrouping() {
    return getGroupBy().getGrouping();
  }

  /**
   * Computes the {@link Group} for the given {@link Object}.
   *
   * @param target {@link Object} to evaluate.
   * @return the {@link Group} for the given {@link Object}.
   * @see org.cp.elements.data.oql.support.Group
   */
  Group<T> compute(T target);

  /**
   * Computes the {@link Group} for the given {@link Object} then adds the {@link Object} to the {@link Group}.
   *
   * @param target {@link Object} to group.
   * @return the given {@link Object}.
   * @see #compute(Object)
   */
  default T group(T target) {
    Assert.notNull(target, "Object to group is required");
    compute(target).include(target);
    return target;
  }

  @Override
  default Stream<Group<T>> stream() {
    return StreamUtils.stream(this);
  }
}
