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
 * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} being queried.
 * @param <T> {@link Class type} of the {@link Object projected elements}.
 * @see org.cp.elements.util.stream.Streamable
 * @see Iterable
 */
public class Group<S, T> implements Iterable<S>, Numbered, Streamable<S> {

  /**
   * Factory method used to construct a new {@link Group} initialized with the given {@link Oql.GroupBy} clause
   * and {@link Integer group number} uniquely identifying this {@link Group}.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} being queried.
   * @param <T> {@link Class type} of the {@link Object projected elements}.
   * @param groupBy reference to the {@link Oql.GroupBy} clause; required.
   * @param number {@link Integer number} uniquely identifying this {@link Group}.
   * @return a new {@link Group}.
   * @throws IllegalArgumentException if {@link Oql.GroupBy} is {@literal null}.
   * @see Oql.GroupBy
   */
  static <S, T> Group<S, T> with(Oql.GroupBy<S, T> groupBy, int number) {
    return new Group<>(groupBy, number);
  }

  private final int number;

  private final Oql.GroupBy<S, T> groupBy;

  private final List<S> members = new ArrayList<>();

  /**
   * Constructs a new {@link Group} initialized with the given {@link Oql.GroupBy} clause
   * and {@link Integer group number} uniquely identifying this {@link Group}.
   *
   * @param groupBy reference to the {@link Oql.GroupBy} clause; required.
   * @param number {@link Integer number} uniquely identifying this {@link Group}.
   * @throws IllegalArgumentException if {@link Oql.GroupBy} is {@literal null}.
   * @see Oql.GroupBy
   */
  Group(Oql.GroupBy<S, T> groupBy, int number) {
    this.groupBy = ObjectUtils.requireObject(groupBy, "GroupBy clause is required");
    this.number = number;
  }

  /**
   * Returns a {@link Integer count} for the number of {@link S members} in this {@link Group}.
   *
   * @return a {@link Integer count} for the number of {@link S members} in this {@link Group}.
   * @see #getMembers()
   */
  public long getCount() {
    return getMembers().size();
  }

  /**
   * Gets the {@link Oql.GroupBy} clause containing the {@link Grouping} used to form this {@link Group}.
   *
   * @return the {@link Oql.GroupBy} clause containing the {@link Grouping} used to form this {@link Group}.
   * @see Oql.GroupBy
   */
  @SuppressWarnings("unused")
  protected Oql.GroupBy<S, T> getGroupBy() {
    return this.groupBy;
  }

  /**
   * Gets a {@link List} of {@link S members} in this {@link Group}.
   * <p>
   * {@link S Members} in the {@link List} are listed in the ordered they are added. The {@link List} is immutable.
   * If no {@link S members} exist in this {@link Group} than an empty {@link List} is returned.
   *
   * @return a {@link List} of {@link S members} in this {@link Group}.
   * @see List
   */
  public List<S> getMembers() {
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
   * Adds the given {@link S member} to this {@link Group}.
   *
   * @param member {@link S member} to add to this {@link Group}; required.
   * @return the given {@link S member}
   * @throws IllegalArgumentException if {@link S member} is {@literal null}.
   */
  protected S include(@NotNull S member) {
    Assert.notNull(member, "Member to include in this Group [%d] is required", number);
    this.members.add(member);
    return member;
  }

  @Override
  @SuppressWarnings("all")
  public Iterator<S> iterator() {
    return getMembers().iterator();
  }

  @Override
  public Stream<S> stream() {
    return StreamUtils.stream(this);
  }
}
