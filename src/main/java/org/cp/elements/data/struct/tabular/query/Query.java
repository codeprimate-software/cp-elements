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
package org.cp.elements.data.struct.tabular.query;

import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.cp.elements.data.struct.tabular.AbstractView;
import org.cp.elements.data.struct.tabular.Column;
import org.cp.elements.data.struct.tabular.Row;
import org.cp.elements.data.struct.tabular.View;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.Alias;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.text.FormatUtils;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.CollectionUtils;
import org.cp.elements.util.stream.StreamUtils;

/**
 * Abstract Data Type (ADT) modeling a query (e.g. {@literal SELECT} [SQL] statement) on a tabular data structure.
 *
 * @author John Blum
 * @see java.lang.Runnable
 * @see java.util.Comparator
 * @see java.util.List
 * @see java.util.function.Predicate
 * @see org.cp.elements.data.struct.tabular.Column
 * @see org.cp.elements.data.struct.tabular.Row
 * @see org.cp.elements.data.struct.tabular.View
 * @since 1.0.0
 */
public class Query implements Runnable {

  protected static final Predicate<Row> DEFAULT_PREDICATE = row -> true;

  protected static final String SELECT_STATEMENT = "SELECT %1$s FROM %2$s";
  protected static final String WHERE_CLAUSE = " WHERE %s";
  protected static final String ORDER_BY_CLAUSE = " ORDER BY %s";

  /**
   * Factory method used to define a {@link Query} with the {@literal selected} {@link Column Columns}.
   *
   * @param columns array of {@link Column Columns} to select.
   * @return a new {@link Query} initialized with the given array of {@link Column Columns}
   * as the {@literal selection / projection} for the {@link Query}.
   * @throws IllegalArgumentException if the projection is {@literal null} or {@literal empty}.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see #Query(List)
   */
  public static @NotNull Query select(Column<?>... columns) {
    return new Query(Arrays.asList(ArrayUtils.nullSafeArray(columns, Column.class)));
  }

  /**
   * Factory method used to define a {@link Query} with the {@literal selected} {@link Column Columns}.
   *
   * @param columns {@link Iterable} of {@link Column Columns} to select.
   * @return a new {@link Query} initialized with the given {@link Iterable} of {@link Column Columns}
   * as the {@literal selection / projection} for the {@link Query}.
   * @throws IllegalArgumentException if the projection is {@literal null} or {@literal empty}.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see java.lang.Iterable
   * @see #Query(List)
   */
  public static @NotNull Query select(Iterable<Column<?>> columns) {
    return new Query(CollectionUtils.asList(columns));
  }

  private final AtomicReference<View> resultSet = new AtomicReference<>();

  private Comparator<Row> orderBy;

  private final List<Column<?>> projection;

  private Predicate<Row> predicate;

  private View from;

  /**
   * Constructs a new {@link Query} initialized with the {@link List} of {@literal selected} {@link Column Columns}
   * defining the {@literal query's projection}.
   *
   * @param projection {@link List} of {@literal selected} {@link Column Columns} defining this query's projection;
   * must not be {@literal null} or {@literal empty}.
   * @throws IllegalArgumentException if the given {@link List} of {@link Column Columns}
   * (a.k.a. {@literal query projection}) is {@literal null} or {@literal empty}.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see java.util.List
   */
  protected Query(List<Column<?>> projection) {

    Assert.notEmpty(projection, "The projection must contain columns");

    this.projection = projection;
  }

  /**
   * Returns the {@link View} to query.
   *
   * @return the {@link View} to query.
   * @throws IllegalStateException if {@link View} is {@literal null}.
   * @see org.cp.elements.data.struct.tabular.View
   * @see #from(View)
   */
  protected @NotNull View getFrom() {
    return ObjectUtils.requireState(this.from, "From clause is required");
  }

  /**
   * Returns an {@link Optional} {@link Comparator} used to order (sort) the {@link Row result set}
   * of this {@link Query}.
   *
   * @return an {@link Optional} {@link Comparator} used to order (sort) the {@link Row result set}
   * of this {@link Query}.
   * @see org.cp.elements.data.struct.tabular.Row
   * @see java.util.Comparator
   * @see java.util.Optional
   */
  protected Optional<Comparator<Row>> getOrderBy() {
    return Optional.ofNullable(this.orderBy);
  }

  /**
   * Returns an {@link Optional} {@link Predicate} used to filter the {@link Row Rows}
   * of the {@link View} being queried.
   *
   * @return an {@link Optional} {@link Predicate} used to filter the {@link Row Rows}
   * of the {@link View} being queried.
   * @see org.cp.elements.data.struct.tabular.Row
   * @see java.util.function.Predicate
   * @see java.util.Optional
   */
  protected Optional<Predicate<Row>> getPredicate() {
    return Optional.ofNullable(this.predicate);
  }

  /**
   * Returns the {@link List} of {@link Column Columns} used as this {@link Query Query's} projection.
   *
   * @return the {@link List} of {@link Column Columns} used as this {@link Query Query's} projection.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see java.util.List
   * @see #select(Column[])
   * @see #select(Iterable)
   */
  protected List<Column<?>> getProjection() {
    return Collections.unmodifiableList(this.projection);
  }

  /**
   * Alias for {@link #getProjection()}.
   *
   * @return the {@link List} of {@link Column Columns} used as this {@link Query Query's} projection.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see java.util.List
   */
  @SuppressWarnings("unused")
  @Alias(forMember = "Query.getProjection()")
  protected List<Column<?>> getSelection() {
    return getProjection();
  }

  /**
   * Executes this {@link Query} generating a new {@link View} from the {@literal result set}.
   *
   * @return a new {@link View} based on the {@literal result set} of this {@link Query}.
   * @throws IllegalStateException if the {@link View} is {@literal null}.
   * @throws IllegalArgumentException if the {@link View} does not contain all the {@link Column Columns}
   * in the {@literal selected projection} defined by this {@link Query}.
   * @see org.cp.elements.data.struct.tabular.View
   * @see #run(View, Predicate)
   * @see #results()
   * @see #run()
   */
  public synchronized @NotNull View execute() {

    this.resultSet.set(null);

    View fromView = getFrom();

    List<Column<?>> projection = resolveProjection(fromView);

    Predicate<Row> predicate = resolvePredicate();

    List<Row> resultSet = sort(run(fromView, predicate));

    View view = AbstractView.of(projection, resultSet);

    this.resultSet.set(view);

    return view;
  }

  /**
   * Returns a {@link View} of the {@literal result set} after executing this {@link Query}.
   *
   * @return a new {@link View} based on the {@literal result set} of this {@link Query}.
   * @see org.cp.elements.data.struct.tabular.View
   * @see #run()
   */
  public synchronized @NotNull View results() {

    if (this.resultSet.get() == null) {
      run();
    }

    return this.resultSet.get();
  }

  /**
   * Executes this {@link Query}.
   *
   * @see #execute()
   */
  public synchronized void run() {
    execute();
  }

  /**
   * Resolves the {@literal projection} used in this {@link Query} by validating the {@link List} of {@literal selected}
   * {@link Column Columns} against the {@link Column Columns} contained in the {@link View}.
   *
   * @param from {@link View} to query; must not be {@literal null}.
   * @return the {@link List} of {@link Column Columns} constituting the {@literal projection} of this {@link Query}.
   * @throws IllegalArgumentException if the {@literal projection/selected} {@link Column Columns}
   * are not contained in the {@link View} to query.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see #getProjection()
   * @see java.util.List
   */
  private List<Column<?>> resolveProjection(@NotNull View from) {

    List<Column<?>> projection = getProjection();

    Assert.isTrue(projection.stream().allMatch(from::contains),
      () -> String.format("The View of Columns %1$s does not contain all the selected, or projected Columns %2$s",
        from.columns(), projection));

    return projection;
  }

  /**
   * Resolve the {@link Predicate} used to filter {@link Row Rows} in the queried {@link View}.
   *
   * If the configured {@link Predicate} is {@literal null}, this method will return a {@link Predicate}
   * accepting all {@link Row Rows} contained in the queried {@link View}.
   *
   * @return the resolved {@link Predicate}.
   * @see org.cp.elements.data.struct.tabular.Row
   * @see java.util.function.Predicate
   */
  private @NotNull Predicate<Row> resolvePredicate() {
    return getPredicate().orElse(DEFAULT_PREDICATE);
  }

  /**
   * Runs this {@link Query} against the targeted {@link View} by filtering {@link Row Rows}
   * in the queried {@link View} using the given {@link Predicate}.
   *
   * @param from {@link View} to query; must not be {@literal null}.
   * @param predicate {@link Predicate} used to filter {@link Row Rows} in the queried {@link View};
   * must not be {@literal null}.
   * @return a {@link List} of {@link Row Rows} from the queried {@link View} matching the {@link Predicate}.
   * @see org.cp.elements.data.struct.tabular.View
   * @see org.cp.elements.data.struct.tabular.Row
   * @see java.util.function.Predicate
   * @see java.util.List
   */
  private List<Row> run(@NotNull View from, @NotNull Predicate<Row> predicate) {

    return StreamUtils.stream(from.rows())
        .filter(predicate)
        .collect(Collectors.toList());
  }

  /**
   * Sorts the given {@link List} of {@link Row Rows} using the configured {@link Comparator order by} if present.
   *
   * @param rows {@link List} of {@link Row Rows} to sort.
   * @return the sorted {@link List} of {@link Row Rows}.
   * @see org.cp.elements.data.struct.tabular.Row
   * @see java.util.List
   * @see #getOrderBy()
   */
  private List<Row> sort(List<Row> rows) {

    getOrderBy().ifPresent(rows::sort);

    return rows;
  }

  /**
   * Builder method used to specify the {@link View} to query.
   *
   * @param view {@link View} to query; must not be {@literal null}.
   * @return this {@link Query}.
   * @throws IllegalArgumentException if the given {@link View} is {@literal null}.
   * @see org.cp.elements.data.struct.tabular.View
   */
  public @NotNull Query from(@NotNull View view) {
    this.from = ObjectUtils.requireObject(view, "View is required");
    return this;
  }

  /**
   * Builder method used to specify the {@link Comparator order criteria} used to {@literal sort}
   * the {@literal result set} of this {@link Query}.
   *
   * @param order {@link Comparator} used to define the {@literal order criteria} to {@literal sort}
   * the {@literal result set} of this {@link Query}.
   * @return this {@link Query}.
   * @see org.cp.elements.data.struct.tabular.Row
   * @see java.util.Comparator
   */
  public @NotNull Query orderBy(@Nullable Comparator<Row> order) {
    this.orderBy = order;
    return this;
  }

  /**
   * Builder method used to specify the {@link Predicate} to filter the {@link Row Rows} from the {@link View}.
   *
   * The {@link Predicate} effectively defines the {@literal where clause} of this {@link Query}.
   *
   * @param predicate {@link Predicate} used to filter {@link Row Rows} from the {@link View}.
   * @return this {@link Query}.
   * @see org.cp.elements.data.struct.tabular.Row
   * @see java.util.function.Predicate
   */
  public @NotNull Query where(@Nullable Predicate<Row> predicate) {
    this.predicate = predicate;
    return this;
  }

  /**
   * Returns a {@link String} representation of this {@link Query}.
   *
   * The {@link String} is written in {@literal ANSI SQL syntax}.
   *
   * @return a {@link String} describing this {@link Query}
   * @see java.lang.String
   */
  @Override
  public @NotNull String toString() {

    StringBuilder queryString =
      new StringBuilder(FormatUtils.format(SELECT_STATEMENT, getProjection(), getFrom().getName()));

    getPredicate().ifPresent(predicate -> queryString.append(FormatUtils.format(WHERE_CLAUSE, predicate)));
    getOrderBy().ifPresent(orderBy -> queryString.append(FormatUtils.format(ORDER_BY_CLAUSE, orderBy)));

    return queryString.toString();
  }
}
