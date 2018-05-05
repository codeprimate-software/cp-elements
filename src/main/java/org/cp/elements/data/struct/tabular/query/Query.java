/*
 * Copyright 2016 Author or Authors.
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

import static org.cp.elements.util.ArrayUtils.nullSafeArray;

import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import org.cp.elements.data.struct.tabular.AbstractView;
import org.cp.elements.data.struct.tabular.Column;
import org.cp.elements.data.struct.tabular.Row;
import org.cp.elements.data.struct.tabular.View;
import org.cp.elements.lang.Assert;
import org.cp.elements.util.CollectionUtils;

/**
 * The {@link Query} class is a Abstract Data Type (ADT) modeling a query (e.g. {@literal SELECT} statement)
 * on a tabular data structure.
 *
 * @author John Blum
 * @see java.lang.Runnable
 * @see java.util.Comparator
 * @see java.util.function.Predicate
 * @see org.cp.elements.data.struct.tabular.Column
 * @see org.cp.elements.data.struct.tabular.Row
 * @see org.cp.elements.data.struct.tabular.View
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class Query implements Runnable {

  private final AtomicReference<View> resultSet = new AtomicReference<>();

  private Comparator<Row> orderBy;

  private final List<Column> projection;

  private Predicate<Row> predicate;

  private View from;

  /**
   * Factory method used to define a {@link Query} with the selected {@link Column Columns}.
   *
   * @param columns array of selected {@link Column Columns}.
   * @return a new {@link Query} initialized with the given array of {@link Column Columns}
   * as the projection for the {@link Query}.
   * @throws IllegalArgumentException if the projection is {@literal null} or empty.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see #Query(List)
   */
  public static Query select(Column... columns) {
    return new Query(Arrays.asList(nullSafeArray(columns, Column.class)));
  }

  /**
   * Factory method used to define a {@link Query} with the selected {@link Column Columns}.
   *
   * @param columns {@link Iterable} of selected {@link Column Columns}.
   * @return a new {@link Query} initialized with the given {@link Iterable} of {@link Column Columns}
   * as the projection for the {@link Query}.
   * @throws IllegalArgumentException if the projection is {@literal null} or empty.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see java.lang.Iterable
   * @see #Query(List)
   */
  public static Query select(Iterable<Column> columns) {
    return new Query(CollectionUtils.asList(columns));
  }

  /**
   * Constructs a new {@link Query} initialized with the {@link List} of {@literal selected} {@link Column Columns}
   * that define the query's projection.
   *
   * @param projection {@link List} of {@literal selected} {@link Column Columns} defining this query's projection.
   * @throws IllegalArgumentException if the {@link List} of {@link Column Columns} (a.k.a. projection)
   * is {@literal null} or empty.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see java.util.List
   */
  private Query(List<Column> projection) {

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
  protected View getFrom() {

    Assert.state(this.from != null, "From clause is required");

    return this.from;
  }

  /**
   * Returns an {@link Optional} {@link Comparator} used to order the {@link Row result set} of this {@link Query}.
   *
   * @return an {@link Optional} {@link Comparator} used to order the {@link Row result set} of this {@link Query}.
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
  protected List<Column> getProjection() {
    return Collections.unmodifiableList(this.projection);
  }

  /**
   * Returns a {@link View} of the result set after executing this {@link Query}.
   *
   * @return a new {@link View} based on the result set of this {@link Query}.
   * @see org.cp.elements.data.struct.tabular.View
   * @see #run()
   */
  public synchronized View results() {

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
   * Executes this {@link Query} generating a new {@link View} from the result set.
   *
   * @return a new {@link View} based on the result set of this {@link Query}.
   * @throws IllegalStateException if the {@link View} is {@literal null}.
   * @throws IllegalArgumentException if the {@link View} does not contain all the {@link Column Columns}
   * in the selected projection defined by this {@link Query}.
   * @see org.cp.elements.data.struct.tabular.View
   * @see #run(View, Predicate)
   * @see #results()
   * @see #run()
   */
  public synchronized View execute() {

    this.resultSet.set(null);

    View from = getFrom();

    List<Column> projection = resolveProjection(from);

    Predicate<Row> predicate = resolvePredicate();

    List<Row> resultSet = sort(run(from, predicate));

    View view = AbstractView.of(projection, resultSet);

    this.resultSet.set(view);

    return view;
  }

  /**
   * Resolves the projection used in this {@link Query} by validating the {@link List} of {@literal selected}
   * {@link Column Columns} against the {@link Column Columns} contained in the {@link View}.
   *
   * @param from {@link View} to query.
   * @return the {@link List} of {@link Column Columns} constituting the projection of this {@link Query}.
   * @throws IllegalArgumentException if the projection/selected {@link Column Columns} are not contained
   * in the {@link View} to query.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see java.util.List
   * @see #getProjection()
   */
  private List<Column> resolveProjection(View from) {

    List<Column> projection = getProjection();

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
  private Predicate<Row> resolvePredicate() {
    return getPredicate().orElseGet(() -> row -> true);
  }

  /**
   * Runs this {@link Query} against the targeted {@link View} by filtering {@link Row Rows}
   * in the queried {@link View} using the given {@link Predicate}.
   *
   * @param from {@link View} to query.
   * @param predicate {@link Predicate} used to filter {@link Row Rows} in the queried {@link View}.
   * @return a {@link List} of {@link Row Rows} from the queried {@link View} matching the {@link Predicate}.
   * @see org.cp.elements.data.struct.tabular.View
   * @see org.cp.elements.data.struct.tabular.Row
   * @see java.util.function.Predicate
   * @see java.util.List
   */
  private List<Row> run(View from, Predicate<Row> predicate) {

    return StreamSupport.stream(from.rows().spliterator(), false)
        .filter(predicate)
        .collect(Collectors.toList());
  }

  /**
   * Sorts the given {@link List} of {@link Row Rows} using the configured order by {@link Comparator} if present.
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
   * Builder method specifying the {@link View} to query.
   *
   * @param view {@link View} to query; must not be {@literal null}.
   * @return this {@link Query}.
   * @throws IllegalArgumentException if {@link View} is {@literal null}.
   * @see org.cp.elements.data.struct.tabular.View
   */
  public Query from(View view) {

    Assert.notNull(view, "View is required");

    this.from = view;

    return this;
  }

  /**
   * Builder method specifying the {@link Comparator order criteria} used to sort the result set of this {@link Query}.
   *
   * @param order {@link Comparator} used to define the order criteria to sort the result set of this {@link Query}.
   * @return this {@link Query}.
   * @see org.cp.elements.data.struct.tabular.Row
   * @see java.util.Comparator
   */
  public Query orderBy(Comparator<Row> order) {
    this.orderBy = order;
    return this;
  }

  /**
   * Builder method specifying the {@link Predicate} used to filter {@link Row Rows} in the specified {@link View}.
   *
   * The {@link Predicate} effectively defines the {@literal where clause} of this {@link Query}.
   *
   * @param predicate {@link Predicate} used to filter {@link Row Rows} in the specified {@link View}.
   * @return this {@link Query}.
   * @see org.cp.elements.data.struct.tabular.Row
   * @see java.util.function.Predicate
   */
  public Query where(Predicate<Row> predicate) {
    this.predicate = predicate;
    return this;
  }

  /**
   * Returns a {@link String} representation of this {@link Query}.
   *
   * The {@link String} is written in ANSI SQL syntax.
   *
   * @return a {@link String} describing this {@link Query}
   * @see java.lang.String
   */
  @Override
  public String toString() {

    StringBuilder queryString =
      new StringBuilder(String.format("SELECT %1$s FROM %2$s", getProjection(), getFrom().getName()));

    getPredicate().ifPresent(predicate -> queryString.append(String.format(" WHERE %s", predicate.toString())));

    getOrderBy().ifPresent(orderBy -> queryString.append(String.format(" ORDER BY %s", orderBy)));

    return queryString.toString();
  }
}
