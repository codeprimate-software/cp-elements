package org.cp.elements.data.oql.support;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Predicate;

import org.cp.elements.data.oql.Oql;
import org.cp.elements.data.oql.Oql.From;
import org.cp.elements.data.oql.Oql.Grouping;
import org.cp.elements.data.oql.Oql.OrderBy;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Numbered;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;

/**
 * Default implementation of {@link Oql.GroupBy}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.Oql
 * @see org.cp.elements.data.oql.Oql.GroupBy
 * @see org.cp.elements.data.oql.Oql.Grouping
 * @see org.cp.elements.data.oql.support.GroupByClause.Group
 * @since 2.0.0
 */
public class GroupByClause<S, T> implements Oql.GroupBy<S, T> {

  /**
   * Factory method used to copy= the given, existing {@link GroupByClause} into a new instance.
   * <p>
   * Additionally, associates the new {@link GroupByClause} to the query's {@link From} clause.
   *
   * @param <S> {@link Class type} of {@link Object elements} in the {@link Iterable collection} being queried.
   * @param <T> {@link Class type} of the projected result.
   * @param groupBy {@link GroupByClause} to copy; required.
   * @return a new {@link GroupByClause} copied from the given {@link GroupByClause}.
   * @throws IllegalArgumentException if the {@link GroupByClause} to copy is {@literal null}.
   */
  public static <S, T> GroupByClause<S, T> copy(@NotNull GroupByClause<S, T> groupBy) {

    Assert.notNull(groupBy, "GroupBy clause is required");

    From<S, T> from = groupBy.getFrom();
    Grouping<S> grouping = groupBy.getGrouping();
    GroupByClause<S, T> copy = of(from, grouping);

    copy.having(groupBy.getPredicate());

    if (from instanceof FromClause<S,T> fromClause) {
      fromClause.withGroupBy(copy);
    }

    return copy;
  }

  /**
   * Factory method used to construct a new {@link GroupByClause} initialized with the given {@link From}
   * and {@link Grouping}.
   *
   * @param <S> {@link Class type} of {@link Object elements} in the {@link Iterable collection} being queried.
   * @param <T> {@link Class type} of the projected result.
   * @param from {@link From} clause to which the {@link GroupByClause} is assigned.
   * @param grouping {@link Grouping} defining how {@link Object elements} in the {@link Iterable collection}
   * will be grouped, or combined.
   * @return a new {@link GroupByClause}.
   * @throws IllegalArgumentException if {@link From} or {@link Grouping} are {@literal null}.
   * @see org.cp.elements.data.oql.Oql.Grouping
   * @see org.cp.elements.data.oql.Oql.From
   */
  public static <S, T> GroupByClause<S, T> of(@NotNull From<S, T> from, @NotNull Grouping<S> grouping) {
    return new GroupByClause<>(from, grouping);
  }

  private final From<S, T> from;

  private final Grouping<S> grouping;

  private final Map<Integer, Group<S, T>> groups = new ConcurrentHashMap<>();

  private volatile Predicate<T> predicate;

  public GroupByClause(@NotNull From<S, T> from, @NotNull Grouping<S> grouping) {
    this.from = ObjectUtils.requireObject(from, "From is required");
    this.grouping = ObjectUtils.requireObject(grouping, "Grouping is required");
  }

  @Override
  public From<S, T> getFrom() {
    return this.from;
  }

  @Override
  public Grouping<S> getGrouping() {
    return this.grouping;
  }

  @Override
  public Predicate<T> getPredicate() {
    return this.predicate;
  }

  @Override
  public S compute(S target) {

    Assert.notNull(target, "Target object to group is required");

    this.groups.computeIfAbsent(getGrouping().group(target), groupNumber -> Group.with(this, groupNumber))
      .apply(target);

    return target;
  }

  @Override
  public Oql.GroupBy<S, T> having(Predicate<T> predicate) {
    this.predicate = predicate;
    return this;
  }

  @Override
  public OrderBy<S, T> orderBy(Comparator<S> comparator) {
    return OrderByClause.copy(OrderBy.of(getFrom(), comparator));
  }

  public static class Group<S, T> implements Numbered {

    static <S, T> Group<S, T> with(Oql.GroupBy<S, T> groupBy, int number) {
      return new Group<>(groupBy, number);
    }

    private final int number;

    private final Oql.GroupBy<S, T> groupBy;

    private final List<S> members = new ArrayList<>();

    Group(Oql.GroupBy<S, T> groupBy, int number) {
      this.groupBy = ObjectUtils.requireObject(groupBy, "GroupBy is required");
      this.number = number;
    }

    public long getCount() {
      return getMembers().size();
    }

    protected Oql.GroupBy<S, T> getGroupBy() {
      return this.groupBy;
    }

    protected List<S> getMembers() {
      return Collections.unmodifiableList(this.members);
    }

    public long getNumber() {
      return this.number;
    }

    public S apply(S target) {
      this.members.add(target);
      return target;
    }
  }
}
