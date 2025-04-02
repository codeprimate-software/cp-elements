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
package org.cp.elements.data.oql.support;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Stream;

import org.cp.elements.data.oql.Oql;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Numbered;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.util.stream.StreamUtils;
import org.cp.elements.util.stream.Streamable;

/**
 * Abstract Data Type (ADT) modeling a {@literal group of members}.
 *
 * @param <T> {@link Class type} of {@link Object members} in this {@link Group}.
 * @see org.cp.elements.util.stream.Streamable
 * @see org.cp.elements.lang.Numbered
 * @see java.lang.Iterable
 */
public class Group<T> implements Iterable<T>, Numbered, Streamable<T> {

  /**
   * Factory method used to construct a new {@link Group} initialized with the given {@link Oql.GroupBy} clause
   * and {@link Integer group number} uniquely identifying this {@link Group}.
   *
   * @param <T> {@link Class type} of {@link Object members} in this {@link Group}.
   * @param groupBy reference to the {@link Oql.GroupBy} clause; required.
   * @param number {@link Integer number} uniquely identifying this {@link Group}.
   * @return a new {@link Group}.
   * @throws IllegalArgumentException if {@link Oql.GroupBy} is {@literal null}.
   * @see org.cp.elements.data.oql.Oql.GroupBy
   */
  static <T> Group<T> with(Oql.GroupBy<?, T> groupBy, int number) {
    return new Group<>(groupBy, number);
  }

  private final int number;

  private final Oql.GroupBy<?, T> groupBy;

  private final List<T> members = new ArrayList<>();

  /**
   * Constructs a new {@link Group} initialized with the given {@link Oql.GroupBy} clause
   * and {@link Integer group number} uniquely identifying this {@link Group}.
   *
   * @param groupBy reference to the {@link Oql.GroupBy} clause; required.
   * @param number {@link Integer number} uniquely identifying this {@link Group}.
   * @throws IllegalArgumentException if {@link Oql.GroupBy} is {@literal null}.
   * @see org.cp.elements.data.oql.Oql.GroupBy
   */
  Group(Oql.GroupBy<?, T> groupBy, int number) {
    this.groupBy = ObjectUtils.requireObject(groupBy, "GroupBy clause is required");
    this.number = number;
  }

  /**
   * Returns a {@link Integer count} with the number of {@link T members} in this {@link Group}.
   *
   * @return a {@link Integer count} with the number of {@link T members} in this {@link Group}.
   * @see #getMembers()
   */
  public long getCount() {
    return getMembers().size();
  }

  /**
   * Determines if this {@link Group} contains any {@link T members}.
   *
   * @return a boolean value indicating whether this {@link Group} contains any {@link T members}.
   * @see #getCount()
   */
  public boolean isEmpty() {
    return getCount() == 0L;
  }

  /**
   * Determines if this {@link Group} contains any {@link T members}.
   *
   * @return a boolean value indicating whether this {@link Group} contains any {@link T members}.
   * @see #isEmpty()
   */
  public boolean isNotEmpty() {
    return !isEmpty();
  }

  /**
   * Gets the {@link Oql.GroupBy} clause containing the {@link Grouping} used to form this {@link Group}.
   *
   * @return the {@link Oql.GroupBy} clause containing the {@link Grouping} used to form this {@link Group}.
   * @see Oql.GroupBy
   */
  @SuppressWarnings("unused")
  protected Oql.GroupBy<?, T> getGroupBy() {
    return this.groupBy;
  }

  /**
   * Gets the {@link Grouping} function used to determine the {@link Group} of an {@link Object}.
   *
   * @return the {@link Grouping} function used to determine the {@link Group} of an {@link Object}.
   * @see org.cp.elements.data.oql.support.Grouping
   * @see #getGroupBy()
   */
  @SuppressWarnings("unused")
  protected Grouping<T> getGrouping() {
    return getGroupBy().getGrouping();
  }

  /**
   * Gets a {@link List} of {@link T members} in this {@link Group}.
   * <p>
   * {@link T Members} in the {@link List} are listed in the order they are added. The {@link List} is immutable.
   * If no {@link T members} exist in this {@link Group}, then an empty {@link List} is returned.
   *
   * @return a {@link List} of {@link T members} in this {@link Group}.
   * @see java.util.List
   */
  public List<T> getMembers() {
    return Collections.unmodifiableList(this.members);
  }

  /**
   * Returns the {@link Long number} used to uniquely identify this {@link Group}.
   *
   * @return the {@link Long number} used to uniquely identify this {@link Group}.
   */
  public long getNumber() {
    return this.number;
  }

  /**
   * Adds the given {@link T member} to this {@link Group}.
   *
   * @param member {@link T member} to add to this {@link Group}; required.
   * @return the given {@link T member}
   * @throws IllegalArgumentException if {@link T member} is {@literal null}.
   */
  protected T include(@NotNull T member) {
    Assert.notNull(member, "Member to include in this Group [%d] is required", getNumber());
    this.members.add(member);
    return member;
  }

  @Override
  @SuppressWarnings("all")
  public Iterator<T> iterator() {
    return getMembers().iterator();
  }

  @Override
  public Stream<T> stream() {
    return StreamUtils.stream(this);
  }
}
