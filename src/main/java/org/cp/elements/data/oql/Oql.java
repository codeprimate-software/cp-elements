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
package org.cp.elements.data.oql;

import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalStateException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newUnsupportedOperationException;

import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BiFunction;
import java.util.function.BiPredicate;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.cp.elements.data.oql.support.Group;
import org.cp.elements.data.oql.support.Grouping;
import org.cp.elements.data.oql.support.OqlUtils;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Builder;
import org.cp.elements.lang.Constants;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.Alias;
import org.cp.elements.lang.annotation.Dsl;
import org.cp.elements.lang.annotation.FluentApi;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.service.loader.ServiceLoaderSupport;
import org.cp.elements.util.ArrayBuilder;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.CollectionUtils;
import org.cp.elements.util.stream.StreamUtils;
import org.cp.elements.util.stream.Streamable;

/**
 * Interface defining an {@literal Object Query Language (OQL)}
 * over a {@link Iterable collection} of {@link Object objects}.
 *
 * @author John Blum
 * @see org.cp.elements.lang.annotation.Dsl
 * @see org.cp.elements.lang.annotation.FluentApi
 * @see org.cp.elements.data.oql.BaseOql
 * @see org.cp.elements.data.oql.provider.SimpleOqlProvider
 * @since 2.0.0
 */
@FluentApi
@SuppressWarnings("unused")
public interface Oql extends BaseOql {

  String NO_FROM = "From not initialized";

  /**
   * Returns the default, configured {@link Oql} service provider implementation.
   *
   * @return the default, configured {@link Oql} service provider implementation.
   * @see org.cp.elements.data.oql.provider.SimpleOqlProvider
   * @see org.cp.elements.data.oql.Oql.Provider
   */
  static Oql defaultProvider() {
    return Oql.Provider.getLoader().getServiceInstance();
  }

  /**
   * Returns the {@literal OQL} service provider implementation for {@link Oql} {@link QueryExecutor},
   * used to execute {@link Query OQL queries}.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @return the {@link Oql} service provider implementation for {@literal OQL} {@link QueryExecutor}.
   * @see org.cp.elements.data.oql.provider.SimpleQueryExecutor
   * @see org.cp.elements.data.oql.QueryExecutor
   */
  <S, T> QueryExecutor<S, T> executor();

  /**
   * Queries the {@link Iterable collection} of {@link Object objects}.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
   * @param collection {@link Iterable} to query; required.
   * @return the {@link From} clause of an OQL query.
   * @throws IllegalArgumentException if {@link Iterable collection} is {@literal null}.
   * @see org.cp.elements.data.oql.Oql.Select#from(Iterable)
   * @see org.cp.elements.data.oql.Oql.Projection#star()
   * @see org.cp.elements.data.oql.Oql.From
   * @see #select(Projection)
   * @see java.lang.Iterable
   */
  @Dsl
  default <S> From<S, S> from(@NotNull Iterable<S> collection) {
    Assert.notNull(collection, "Iterable collection to query is required");
    return this.<S, S>select(Projection.star()).from(collection);
  }

  /**
   * Declares the {@link Select selected data} from the {@link Iterable collection} of {@link Object objects}.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @param projection {@link Projection} used to {@literal project} the result of {@link Object objects}
   * queried in the {@link Iterable collection}.
   * @return a {@link Select} object modeling the {@link Projection selected data}.
   * @see org.cp.elements.data.oql.Oql.Projection
   * @see org.cp.elements.data.oql.Oql.Select
   */
  @Dsl <S, T> Select<S, T> select(Projection<S, T> projection);

  /**
   * Abstract Data Type (ADT) modeling the {@literal projection} of an {@link Object}
   * from the queried {@link Iterable collection} as an instance of {@link T type}
   * mapped by a user-provided, configured {@link BiFunction mapping function}.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @see org.cp.elements.data.oql.Oql.ObjectMapper
   * @see org.cp.elements.data.oql.Oql.Select
   * @see java.lang.FunctionalInterface
   * @see #select(Projection)
   */
  @FunctionalInterface
  interface Projection<S, T> extends ObjectMapper<S, T> {

    /**
     * Factory method used to construct a new {@link Projection} as the given {@link Class type}.
     *
     * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
     * @param <T> {@link Class type} of the {@link Object projected objects}.
     * @param type {@link Class type} of the projection.
     * @return a new {@link ProjectionBuilder} used to build a {@link Projection} as the given {@link Class type}.
     * @throws IllegalArgumentException if {@link Class type} is {@literal null}.
     * @see ProjectionBuilder
     */
    @Dsl
    static <S, T> ProjectionBuilder<S, T> as(@NotNull Class<T> type) {
      Assert.notNull(type, "Type is required");
      return new ProjectionBuilder<>(type);
    }

    /**
     * Factory method used to construct a new {@literal star} {@link Projection}.
     *
     * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
     * @return a new {@literal star} {@link Projection} effectively returning the same type of {@link S object}
     * that is being queried.
     */
    @Dsl
    static <S> Projection<S, S> star() {

      return new Projection<>() {

        @Override
        public Class<S> getType() {
          return getFromType();
        }

        @Override
        public S map(QueryContext<S, S> queryContext, S target) {
          return target;
        }
      };
    }

    /**
     * Gets the {@link Class type} of the {@link T projected objects} in the query result.
     *
     * @return the {@link Class type} of the {@link T projected objects} in the query result.
     * @see #getFromType()
     */
    Class<T> getType();

    /**
     * Gets the {@link Class type} of {@link Object} from the {@link Iterable collection} being queried.
     * <p>
     * Defaults to the {@link Object} class.
     *
     * @return the {@link Class type} of {@link Object} from the {@link Iterable collection} being queried.
     * @see #getType()
     */
    @SuppressWarnings("unchecked")
    default Class<S> getFromType() {
      return (Class<S>) Object.class;
    }
  }

  /**
   * Abstract Data Type (ADT) modeling a {@link Projection} containing a series of {@link QueryFunction transformations}
   * on the {@link Select selected data} of the {@link T projected type}.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @see org.cp.elements.data.oql.Oql.Projection
   * @see org.cp.elements.data.oql.QueryFunction
   * @see org.cp.elements.util.stream.Streamable
   * @see java.lang.Iterable
   */
  interface TransformingProjection<S, T>
      extends Iterable<QueryFunction<T, ?>>, Projection<S, T>, Streamable<QueryFunction<T, ?>> {

    /**
     * Remaps the {@link QueryResult} into a {@link T projected result}.
     *
     * @param queryContext {@link QueryContext} containing metadata about the context
     * in which the OQL query is executed.
     * @param result {@link QueryResult} from the query result set.
     * @return the remapped {@link T projected result}.
     * @see QueryContext
     * @see QueryResult
     */
    T remap(QueryContext<S, T> queryContext, QueryResult<T> result);

    @Override
    @SuppressWarnings("all")
    default Iterator<QueryFunction<T, ?>> iterator() {
      return Collections.emptyIterator();
    }

    @Override
    default Stream<QueryFunction<T, ?>> stream() {
      return StreamUtils.stream(this);
    }
  }

  /**
   * Abstract Data Type (ADT) modeling the {@literal select clause} in a {@link Query}.
   * <p>
   * Specifically, {@link Select} identifies the {@literal selected data} from the {@link Object objects}
   * in the {@link Iterable collection} forming the result set.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @see org.cp.elements.data.oql.Oql.Projection
   * @see org.cp.elements.data.oql.Oql.From
   */
  interface Select<S, T> {

    boolean DEFAULT_DISTINCT = false;

    /**
     * Determines whether the result set of the {@link Query} should be unique.
     * <p>
     * Defaults to {@literal false}.
     *
     * @return a boolean value indicating whether the result set of the {@link Query} should be unique.
     */
    default boolean isDistinct() {
      return DEFAULT_DISTINCT;
    }

    /**
     * Returns the {@link Projection} of the {@literal selection} of {@link T elements}
     * from the {@link Iterable collection}.
     *
     * @return the {@link Projection} of the {@literal selection} of {@link T elements}
     * from the {@link Iterable collection}.
     * @see org.cp.elements.data.oql.Oql.Projection
     */
    Projection<S, T> getProjection();

    /**
     * Selects a {@link Distinct} result set.
     *
     * @return a new {@link Distinct} clause.
     * @throws UnsupportedOperationException by default.
     * @see Distinct
     */
    @Dsl
    default Distinct<S, T> distinct() {
      throw newUnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
    }

    /**
     * Selects {@link Object objects} from the given {@link Iterable collection} when building the result set
     * of the query.
     *
     * @param collection {@link Iterable collection} to query.
     * @return a new {@link From} clause.
     * @see java.lang.Iterable
     * @see From
     */
    @Dsl
    From<S, T> from(Iterable<S> collection);

  }

  /**
   * Abstract Data Type (ADT) modeling a {@literal distinct} OQL query.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @see org.cp.elements.data.oql.Oql.Select#distinct()
   * @see java.lang.FunctionalInterface
   */
  @FunctionalInterface
  interface Distinct<S, T> {

    /**
     * Selects {@literal distinct} @link Object objects} from the given {@link Iterable collection}
     * when building the result set of the query.
     *
     * @param collection {@link Iterable collection} to query.
     * @return a new {@link From} clause.
     * @see java.lang.Iterable
     * @see From
     */
    @Dsl
    From<S, T> from(Iterable<S> collection);

  }

  /**
   * Abstract Data Type (ADT) modeling the {@literal from clause} in a {@link Query}.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @see org.cp.elements.data.oql.Oql.Select#from(Iterable)
   * @see org.cp.elements.data.oql.Oql.ExecutableQuery
   * @see org.cp.elements.data.oql.Oql.GroupBySpec
   * @see org.cp.elements.data.oql.Oql.LimitSpec
   * @see org.cp.elements.data.oql.Oql.OrderBySpec
   * @see java.lang.FunctionalInterface
   */
  @FunctionalInterface
  interface From<S, T> extends ExecutableQuery<S, T>, GroupBySpec<S, T>, LimitSpec<S, T>, OrderBySpec<S, T> {

    /**
     * Returns the {@link Iterable collection} from which {@link Object objects} are {@link Select selected}.
     *
     * @return the {@link Iterable collection} from which {@link Object objects} are {@link Select selected}.
     * @see java.lang.Iterable
     */
    default Iterable<S> getCollection() {
      return Collections::emptyIterator;
    }

    /**
     * Returns {@literal this}.
     *
     * @return {@literal this}.
     */
    @Override
    default From<S, T> getFrom() {
      return this;
    }

    /**
     * Returns a {@link Select} object identifying the {@literal selected data} from the {@link Iterable collection}
     * returned in the query result set.
     *
     * @return an {@link Select} object identifying the {@literal selected data} from the {@link Iterable collection}
     * returned in the query result set.
     * @see org.cp.elements.data.oql.Oql.Select
     */
    Select<S, T> getSelection();

    /**
     * Gets the {@link Class type} of {@link T objects} in the {@link Iterable collection}.
     *
     * @return the {@link Class type} of {@link T objects} in the {@link Iterable collection}.
     * @see org.cp.elements.data.oql.Oql.Projection#getFromType()
     */
    @Alias(forMember = "Oql.Projection#getFromType")
    default Class<S> getType() {
      return getSelection().getProjection().getFromType();
    }

    /**
     * Gets an {@link Optional} {@link Where} clause of the {@link Query}.
     *
     * @return an {@link Optional} {@link Where} clause of the {@link Query}.
     * @see org.cp.elements.data.oql.Oql.Where
     * @see java.util.Optional
     */
    default Optional<Where<S, T>> getWhere() {
      return Optional.empty();
    }

    /**
     * Builder method used to construct the {@link Where} clause of the query given a {@link BiPredicate}.
     *
     * @param predicate {@link BiPredicate} forming the condition of the {@link Where} clause.
     * @return a new {@link Where} clause.
     * @throws UnsupportedOperationException by default.
     * @see java.util.function.BiPredicate
     * @see #where(Predicate)
     * @see Where
     */
    @Dsl
    default Where<S, T> where(BiPredicate<QueryArguments, S> predicate) {
      throw newUnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
    }

    /**
     * Builder method used to construct the {@link Where} clause of the query given a {@link Predicate}.
     *
     * @param predicate {@link Predicate} forming the condition of the {@link Where} clause.
     * @return a new {@link Where} clause.
     * @throws UnsupportedOperationException by default.
     * @see java.util.function.Predicate
     * @see #where(BiPredicate)
     * @see Where
     */
    @Dsl
    default Where<S, T> where(Predicate<S> predicate) {
      return where(OqlUtils.asBiPredicate(predicate));
    }

    /**
     * Limits the result of the OQL query.
     *
     * @param limit {@link Long} defining the maximum number of results returned in the query result set.
     * @return the {@link ExecutableQuery}.
     * @throws UnsupportedOperationException by default.
     * @see ExecutableQuery
     */
    @Dsl
    @Override
    default ExecutableQuery<S, T> limit(long limit) {
      throw newUnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
    }
  }

  /**
   * Abstract Data Type (ADT) modeling the {@literal where clause} in a {@link Query}.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @see org.cp.elements.data.oql.Oql.From#where(BiPredicate)
   * @see org.cp.elements.data.oql.Oql.ExecutableQuery
   * @see org.cp.elements.data.oql.Oql.GroupBySpec
   * @see org.cp.elements.data.oql.Oql.LimitSpec
   * @see org.cp.elements.data.oql.Oql.OrderBySpec
   * @see java.lang.FunctionalInterface
   */
  @FunctionalInterface
  interface Where<S, T> extends ExecutableQuery<S, T>, GroupBySpec<S, T>, LimitSpec<S, T>, OrderBySpec<S, T> {

    /**
     * Factory method used to compose a {@link Where} clause and a query {@link BiPredicate}.
     *
     * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
     * @param <T> {@link Class type} of the {@link Object projected objects}.
     * @param where {@link Where} clause composed of a query {@link Predicate}; required.
     * @param predicate {@link BiPredicate} specifying the query matching criteria; required.
     * @return a new {@link Where} clause composed with a query {@link BiPredicate}.
     * @throws IllegalArgumentException if the {@link Where} clause or {@link BiPredicate} are {@literal null}.
     * @see java.util.function.BiFunction
     */
    static <S, T> Where<S, T> compose(@NotNull Where<S, T> where, @NotNull BiPredicate<QueryArguments, S> predicate) {

      Assert.notNull(where, "Where clause is required");
      Assert.notNull(predicate, "Predicate is required");

      return new Where<>() {

        @Override
        public From<S, T> getFrom() {
          return where.getFrom();
        }

        @Override
        public BiPredicate<QueryArguments, S> getPredicate() {
          return predicate;
        }
      };
    }

    /**
     * Returns the {@link Predicate filtering criteria} of the {@link Where where clause}.
     *
     * @return the {@link Predicate filtering criteria} of the {@link Where where clause}.
     * @see java.util.function.BiPredicate
     */
    BiPredicate<QueryArguments, S> getPredicate();

    /**
     * Joins the {@link #getPredicate() predicate} of this {@link Where} clause with the given {@link BiPredicate}
     * using the {@literal AND} operator.
     *
     * @param predicate {@link BiPredicate} to join with this {@link Where} clause.
     * @return a new {@link Where} clause joining the {@link #getPredicate() predicate} from this {@link Where} clause
     * with the given {@link BiPredicate} using the {@literal AND} operator.
     * @see java.util.function.BiPredicate#and(BiPredicate)
     * @see #compose(Where, BiPredicate)
     * @see #getPredicate()
     */
    @Dsl
    default Where<S, T> and(@NotNull BiPredicate<QueryArguments, S> predicate) {
      return compose(this, getPredicate().and(predicate));
    }

    /**
     * Joins the {@link #getPredicate() predicate} of this {@link Where} clause with the given {@link Predicate}
     * using the {@literal AND} operator.
     *
     * @param predicate {@link Predicate} to join with this {@link Where} clause.
     * @return a new {@link Where} clause joining the {@link #getPredicate() predicate} from this {@link Where} clause
     * with the given {@link Predicate} using the {@literal AND} operator.
     * @see java.util.function.Predicate
     * @see #and(BiPredicate)
     */
    @Dsl
    default Where<S, T> and(@NotNull Predicate<S> predicate) {
      Assert.notNull(predicate, "Query Predicate is required");
      BiPredicate<QueryArguments, S> biPredicate = OqlUtils.asBiPredicate(predicate);
      return and(biPredicate);
    }

    /**
     * Joins the {@link #getPredicate() predicate} of this {@link Where} clause with the given {@link BiPredicate}
     * using the {@literal OR} operator.
     *
     * @param predicate {@link BiPredicate} to join with this {@link Where} clause.
     * @return a new {@link Where} clause joining the {@link #getPredicate() predicate} from this {@link Where} clause
     * with the given {@link BiPredicate} using the {@literal OR} operator.
     * @see java.util.function.BiPredicate#or(BiPredicate)
     * @see #compose(Where, BiPredicate)
     * @see #getPredicate()
     */
    @Dsl
    default Where<S, T> or(@NotNull BiPredicate<QueryArguments, S> predicate) {
      return compose(this, getPredicate().or(predicate));
    }

    /**
     * Joins the {@link #getPredicate() predicate} of this {@link Where} clause with the given {@link Predicate}
     * using the {@literal OR} operator.
     *
     * @param predicate {@link Predicate} to join with this {@link Where} clause.
     * @return a new {@link Where} clause joining the {@link #getPredicate() predicate} from this {@link Where} clause
     * with the given {@link Predicate} using the {@literal OR} operator.
     * @see java.util.function.Predicate
     * @see #or(BiPredicate)
     */
    @Dsl
    default Where<S, T> or(@NotNull Predicate<S> predicate) {
      Assert.notNull(predicate, "Query Predicate is required");
      BiPredicate<QueryArguments, S> biPredicate = OqlUtils.asBiPredicate(predicate);
      return or(biPredicate);
    }
  }

  /**
   * Abstract Data Type (ADT) modeling the {@literal order by clause} in a {@link Query}.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @see org.cp.elements.data.oql.Oql.ExecutableQuery
   * @see org.cp.elements.data.oql.Oql.LimitSpec
   * @see org.cp.elements.util.stream.Streamable
   * @see java.lang.FunctionalInterface
   * @see java.lang.Iterable
   */
  @FunctionalInterface
  interface OrderBy<S, T> extends ExecutableQuery<S, T>,
      Iterable<Comparator<T>>, LimitSpec<S, T>, Streamable<Comparator<T>> {

    /**
     * Factory method used to construct a new {@link OrderBy} with the given {@link From} clause
     * and array of {@link Comparator Comparators} applied to order the query results.
     *
     * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
     * @param <T> {@link Class type} of the {@link Object projected objects}.
     * @param from {@link From} clause linking this {@link OrderBy} to the {@link Query}; required.
     * @param comparators array of {@link Comparator Comparators} defining the order of the query results.
     * @return a new {@link OrderBy} clause with the given {@link Comparator Comparators}.
     * @throws IllegalArgumentException if the {@link From} clause is {@literal null}
     * or the array of {@link Comparator Comparators} are empty.
     * @see org.cp.elements.data.oql.Oql.From
     * @see java.util.Comparator
     */
    @SafeVarargs
    static <S, T> OrderBy<S, T> of(@NotNull From<S, T> from, Comparator<T>... comparators) {

      Assert.notNull(from, "From clause is required");
      Assert.notEmpty(comparators, "Comparators are required");

      return new OrderBy<>() {

        @Override
        public From<S, T> getFrom() {
          return from;
        }

        @Override
        @SuppressWarnings("all")
        public Iterator<Comparator<T>> iterator() {
          return ArrayUtils.asIterator(comparators);
        }
      };
    }

    /**
     * Return the {@link Comparator} used to {@literal sort} ({@literal order}) the {@link S elements}
     * in the query result set.
     *
     * @return the {@link Comparator} used to {@literal sort} ({@literal order}) the {@link S elements}
     * in the query result set.
     * @see java.util.Comparator
     */
    default Comparator<T> getOrder() {

      return stream()
        .reduce(Comparator::thenComparing)
        .orElseThrow(() -> newIllegalStateException("No Order Defined"));
    }

    /**
     * Sorts the query results in ascending order.
     *
     * @return this {@link OrderBy} clause.
     * @see #descending()
     */
    @Dsl
    default OrderBy<S, T> ascending() {
      return this;
    }

    /**
     * Sorts the query results in descending order.
     *
     * @return this {@link OrderBy} clause.
     * @see #ascending()
     */
    @Dsl
    default OrderBy<S, T> descending() {

      ArrayBuilder<Comparator<T>> comparatorArrayBuilder = ArrayBuilder.from(this);
      Comparator<T> comparator = comparatorArrayBuilder.remove();

      comparator = comparator.reversed();
      comparatorArrayBuilder.add(comparator);

      Comparator<T>[] comparators = comparatorArrayBuilder.build();

      return of(getFrom(), comparators);
    }

    @Override
    default Stream<Comparator<T>> stream() {
      return StreamUtils.stream(this);
    }

    /**
     * Joins the {@link Comparator} from this {@link OrderBy} clause with the given {@link Comparator}.
     *
     * @param comparator {@link Comparator} used to sort the query result set.
     * @return a new {@link OrderBy} clause joining the {@link Comparator} from this {@link OrderBy} clause
     * with the given {@link Comparator}.
     * @see #of(From, Comparator[])
     * @see java.util.Comparator
     */
    @Dsl
    default OrderBy<S, T> thenOrderBy(@NotNull Comparator<T> comparator) {
      return of(getFrom(), getOrder(), comparator);
    }

    /**
     * Uses the {@link Function} to get a {@link Comparable value} from the projected result
     * to further refine the sort order.
     *
     * @param <U> {@link Comparable value} used in the sort when ordering the query results.
     * @param function {@link Function} used to get a {@link Comparable value} from the projected result
     * and further refine the sort order.
     * @return a new {@link OrderBy} clause using the {@link Function} to get a value from the projected result
     * to further refine the sort order.
     * @see java.util.function.Function
     * @see #thenOrderBy(Comparator)
     * @see java.lang.Comparable
     */
    @Dsl
    default <U extends Comparable<U>> OrderBy<S, T> thenOrderBy(@NotNull Function<T, U> function) {
      return thenOrderBy(Comparator.comparing(function));
    }
  }

  /**
   * Interface defining a contract for {@literal OQL} components capable of defining an {@link OrderBy order}.
   *
   * @param <S> source {@link Class type}.
   * @param <T> target {@link Class type}.
   * @see org.cp.elements.data.oql.Oql.OrderBy
   * @see java.util.Comparator
   */
  interface OrderBySpec<S, T> {

    /**
     * Gets an {@link Optional} {@link OrderBy} clause of a {@link Query}.
     *
     * @return an {@link Optional} {@link OrderBy} clause of a {@link Query}.
     * @see org.cp.elements.data.oql.Oql.OrderBy
     * @see java.util.Optional
     */
    default Optional<OrderBy<S, T>> getOrderBy() {
      return Optional.empty();
    }

    /**
     * Builder method used to configure the {@link Function ordering function} applied to {@link S queried objects}
     * to get the values of the fields used in the ordering (sorting) operation.
     *
     * @param <U> {@link Comparable} {@link Class type} of value returned by the {@link Function ordering function}.
     * @param orderingFunction {@link Function} applied to the {@link S queried object} to get the values of fields
     * used in the ordering (sorting) operation.
     * @return the {@link Oql.OrderBy} clause in the OQL query.
     * @throws IllegalArgumentException if {@link Function ordering function} is {@literal null}.
     * @throws UnsupportedOperationException by default.
     * @see java.util.function.Function
     * @see java.lang.Comparable
     * @see Oql.OrderBy
     */
    @Dsl
    default <U extends Comparable<U>> OrderBy<S, T> orderBy(@NotNull Function<T, U> orderingFunction) {
      Assert.notNull(orderingFunction, "Function defining order is required");
      Comparator<T> comparator = Comparator.comparing(orderingFunction);
      return orderBy(comparator);
    }

    /**
     * Builder method used to configure the {@link Comparator} to order (sort) the query result set.
     *
     * @param comparator {@link Comparator} to order (sort) the query result set.
     * @return the {@link OrderBy} clause in the OQL query.
     * @throws UnsupportedOperationException by default.
     * @see java.util.Comparator
     * @see Oql.OrderBy
     */
    @Dsl
    default OrderBy<S, T> orderBy(Comparator<T> comparator) {
      throw newUnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
    }
  }

  /**
   * Interface defining a contract for {@literal OQL} components capable of {@literal limiting} the query result set.
   *
   * @param <S> source {@link Class type}.
   * @param <T> target {@link Class type}.
   * @see org.cp.elements.data.oql.Oql.FromReference
   */
  interface LimitSpec<S, T> extends FromReference<S, T> {

    long DEFAULT_LIMIT = Long.MAX_VALUE;

    /**
     * Returns the {@link Long limit} to the result set size.
     *
     * @return the {@link Long limit} to the result set size.
     */
    default long getLimit() {
      return DEFAULT_LIMIT;
    }

    /**
     * Builder method used to configure the {@link Long limit} on the number of results
     * returned in the query result set.
     *
     * @param limit {@link Long} limiting the results returned in the query result set.
     * @return an {@link Oql.ExecutableQuery}.
     * @see ExecutableQuery
     */
    @Dsl
    default ExecutableQuery<S, T> limit(long limit) {
      return getFrom().limit(limit);
    }
  }

  /**
   * Abstract Data Type (ADT) modeling the {@literal group by clause} in a {@link Query}.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @see org.cp.elements.data.oql.Oql.ExecutableQuery
   * @see org.cp.elements.data.oql.Oql.LimitSpec
   * @see org.cp.elements.data.oql.Oql.OrderBySpec
   * @see org.cp.elements.data.oql.support.Grouping
   * @see org.cp.elements.data.oql.support.Groups
   * @see org.cp.elements.data.oql.support.Group
   * @see java.lang.FunctionalInterface
   */
  @FunctionalInterface
  interface GroupBy<S, T> extends ExecutableQuery<S, T>, LimitSpec<S, T>, OrderBySpec<S, T> {

    /**
     * Factory method used to construct a new {@link GroupBy} clause with the {@link Grouping}
     * linked to the OQL {@link Query} with the {@link From} clause.
     *
     * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
     * @param <T> {@link Class type} of the {@link Object projected objects}.
     * @param from {@link From} clause linking the {@link GroupBy} clause to the OQL {@link Query}.
     * @param grouping {@link Grouping} defining the function to partition queried {@link Object objects} into groups.
     * @return a new {@link GroupBy} clause
     * @throws IllegalArgumentException if the {@link From} clause or {@link Grouping} are {@literal null}.
     * @see org.cp.elements.data.oql.support.Grouping
     * @see org.cp.elements.data.oql.Oql.From
     */
    static <S, T> GroupBy<S, T> of(@NotNull From<S, T> from, @NotNull Grouping<T> grouping) {

      Assert.notNull(from, "From clause is required");
      Assert.notNull(grouping, "Grouping is required");

      return new GroupBy<>() {

        @Override
        public From<S, T> getFrom() {
          return from;
        }

        @Override
        public Grouping<T> getGrouping() {
          return grouping;
        }
      };
    }

    /**
     * Gets the {@link Grouping} defining the {@literal groups} in the result set of the {@link Query}.
     *
     * @return the {@link Grouping} defining the {@literal groups} in the result set of the {@link Query}.
     * @see org.cp.elements.data.oql.support.Grouping
     */
    Grouping<T> getGrouping();

    /**
     * Gets the configured {@link Predicate} defining the condition declared in the {@literal having clause}
     * of {@link GroupBy}.
     *
     * @return the configured {@link Predicate} defining the condition declared in the {@literal having clause}
     * of {@link GroupBy}.
     * @see java.util.function.Predicate
     */
    @SuppressWarnings("unchecked")
    default BiPredicate<QueryArguments, T> getPredicate() {
      return (BiPredicate<QueryArguments, T>) OqlUtils.ACCEPT_ALL_QUERY_PREDICATE;
    }

    /**
     * Redefines the {@link GroupBy} clause by only returning groups of objects matching the given {@link BiPredicate}.
     *
     * @param predicate {@link BiPredicate} defining the criteria used to match groups of objects
     * returned in the query result set.
     * @return a new {@link GroupBy} clause with the given {@link BiPredicate}.
     * @throws IllegalArgumentException if {@link BiPredicate} is {@literal null}.
     * @see java.util.function.BiPredicate
     * @see #having(Predicate)
     */
    @Dsl
    default GroupBy<S, T> having(@NotNull BiPredicate<QueryArguments, T> predicate) {

      Assert.notNull(predicate, "GroupBy Predicate is required");

      return new GroupBy<>() {

        @Override
        public From<S, T> getFrom() {
          return GroupBy.this.getFrom();
        }

        @Override
        public Grouping<T> getGrouping() {
          return GroupBy.this.getGrouping();
        }

        @Override
        public BiPredicate<QueryArguments, T> getPredicate() {
          return predicate;
        }
      };
    }

    /**
     * Redefines the {@link GroupBy} clause by only returning groups of objects matching the given {@link Predicate}.
     *
     * @param predicate {@link Predicate} defining the criteria used to match groups of objects
     * returned in the query result set.
     * @return a new {@link GroupBy} clause with the given {@link Predicate}.
     * @throws IllegalArgumentException if {@link Predicate} is {@literal null}.
     * @see java.util.function.Predicate
     * @see #having(BiPredicate)
     */
    @Dsl
    default GroupBy<S, T> having(@NotNull Predicate<T> predicate) {
      Assert.notNull(predicate, "GroupBy Predicate is required");
      BiPredicate<QueryArguments, T> biPredicate = OqlUtils.asBiPredicate(predicate);
      return having(biPredicate);
    }
  }

  /**
   * Interface defining a contract for {@literal OQL} components capable of defining a {@link GroupBy}.
   *
   * @param <S> source {@link Class type}.
   * @param <T> target {@link Class type}.
   * @see org.cp.elements.data.oql.support.Grouping
   * @see org.cp.elements.data.oql.Oql.GroupBy
   */
  interface GroupBySpec<S, T> {

    /**
     * Gets an {@link Optional} {@link GroupBy} clause of the {@link Query}.
     *
     * @return an {@link Optional} {@link GroupBy} clause of the {@link Query}.
     * @see org.cp.elements.data.oql.Oql.GroupBy
     * @see java.util.Optional
     */
    default Optional<GroupBy<S, T>> getGroupBy() {
      return Optional.empty();
    }

    /**
     * Builder method used to configure the {@link Grouping logic} to form {@link Group Groups}
     * aggregated from the query result set.
     *
     * @param grouping {{@link Grouping} used to form {@link Group Groups} from the query result set.
     * @return thw {@link GroupBy} clause in the OQL query.
     * @throws UnsupportedOperationException by default.
     * @see #groupBy(Function[])
     * @see Oql.GroupBy
     * @see Grouping
     */
    @Dsl
    default GroupBy<S, T> groupBy(Grouping<T> grouping) {
      throw newUnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
    }

    /**
     * Builder method used to configure the {@link Function grouping function} to form {@link Group Groups}
     * aggregated from the query result set.
     *
     * @param groupFunctions {{@link Function} defining the {@link Grouping} applied to the query result set.
     * @return thw {@link GroupBy} clause in the OQL query.
     * @throws UnsupportedOperationException by default.
     * @see java.util.function.Function
     * @see #groupBy(Grouping)
     * @see Oql.GroupBy
     */
    @Dsl
    @SuppressWarnings("unchecked")
    default GroupBy<S, T> groupBy(Function<T, ?>... groupFunctions) {
      Grouping<T> grouping = Grouping.of(groupFunctions);
      return groupBy(grouping);
    }
  }

  /**
   * Interface defining a contract for an OQL statement that can be compiled into an OQL {@link Query}.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @see org.cp.elements.data.oql.Query
   * @see java.lang.FunctionalInterface
   */
  @FunctionalInterface
  interface Compiler<S, T> {

    /**
     * Compiles a OQL statement into a {@link Query}.
     *
     * @return the compiled OQL statement as a {@link Query}.
     * @see org.cp.elements.data.oql.Query
     */
    Query<S, T> compile();

  }

  /**
   * Interface defining a contract for an {@literal OQL} {@link Object} that can be executed or counted.
   *
   * @param <T> {@link Class type} of {@link Object objects} in the {@link Projection projected result set}.
   * @see java.lang.FunctionalInterface
   */
  @FunctionalInterface
  interface Executable<T> {

    /**
     * Counts the results in the query result set.
     *
     * @return a {@link Long value} with the number of results in the query result set.
     * @see #execute(QueryArgument[])
     */
    @SuppressWarnings("all")
    default Long count() {
      Iterable<T> results = execute();
      Stream<T> stream = StreamUtils.stream(CollectionUtils.nullSafeIterable(results));
      Long count = stream.count();
      return count;
    }

    /**
     * Executes the OQL query with the given array of {@link QueryArgument QueryArguments}.
     *
     * @param arguments array of {@link QueryArgument QueryArguments} passed to the OQL query.
     * @return the {@link Iterable result set} of executing the OQL query.
     * @see java.lang.Iterable
     * @see #execute(Iterable)
     * @see QueryArgument
     */
    default Iterable<T> execute(QueryArgument<?>... arguments) {
      return execute(QueryArguments.of(arguments));
    }

    /**
     * Executes the OQL query with the given {@link Iterable} of {@link QueryArgument QueryArguments}.
     *
     * @param arguments {@link Iterable} of {@link QueryArgument QueryArguments} passed to the OQL query.
     * @return the {@link Iterable result set} of executing the OQL query.
     * @see #execute(QueryArgument[])
     * @see java.lang.Iterable
     * @see QueryArgument
     */
    Iterable<T> execute(Iterable<QueryArgument<?>> arguments);

  }

  /**
   * Interface defining an {@literal OQL statement} as an {@link Executable} {@link Query}
   * with a {@link FromReference referecne} to the {@link From} clause.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @see org.cp.elements.data.oql.Oql.FromReference
   * @see org.cp.elements.data.oql.Oql.Executable
   * @see org.cp.elements.data.oql.Oql.Compiler
   * @see org.cp.elements.data.oql.Query
   */
  interface ExecutableQuery<S, T> extends Compiler<S, T>, Executable<T>, FromReference<S, T> {

    @Override
    default Query<S, T> compile() {
      return Query.from(getFrom());
    }

    @Override
    default Iterable<T> execute(Iterable<QueryArgument<?>> arguments) {
      return compile().execute(arguments);
    }
  }

  /**
   * Interface defining a {@literal reference} to an instance of {@link From}.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @see org.cp.elements.data.oql.Oql.From
   */
  interface FromReference<S, T> {

    /**
     * Returns a reference to the {@link From} clause in the OQL {@link Query}.
     *
     * @return a reference to the {@link From} clause in the OQL {@link Query}.
     * @see org.cp.elements.data.oql.Oql.From
     */
    default From<S, T> getFrom() {
      throw newIllegalStateException(NO_FROM);
    }
  }

  /**
   * {@link ServiceLoaderSupport} used to load an {@link Oql} service provider implementation.
   *
   * @see org.cp.elements.service.loader.ServiceLoaderSupport
   */
  interface Provider extends ServiceLoaderSupport<Oql> {

    AtomicReference<Oql.Provider> LOADER_REFERENCE = new AtomicReference<>();

    /**
     * Returns the {@link Oql.Provider} used to load the OQL service provider implementation.
     *
     * @return the {@link Oql.Provider} used to load the OQL service provider implementation.
     */
    static Oql.Provider getLoader() {
      return LOADER_REFERENCE.updateAndGet(loader -> loader != null ? loader : new Oql.Provider() { });
    }

    @Override
    default Class<Oql> getType() {
      return Oql.class;
    }
  }

  /**
   * Builder for an {@link Oql.Projection} based on a given {@link Class projected type}.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   */
  class ProjectionBuilder<S, T> {

    private final Class<T> projectionType;

    @SuppressWarnings("unchecked")
    private Class<S> fromType = (Class<S>) Object.class;

    ProjectionBuilder(@NotNull Class<T> projectionType) {
      this.projectionType = ObjectUtils.requireObject(projectionType, "Projection type is required");
    }

    /**
     * Builder method used to configure the {@link Class type} of {@link S object} being queried and projected.
     *
     * @param fromType {@link Class type} of {@link S object} being queried and projected; required.
     * @return this {@link ProjectionBuilder}.
     * @throws IllegalArgumentException if {@link Class fromType} is {@literal null}.
     */
    @Dsl
    public ProjectionBuilder<S, T> fromType(@NotNull Class<S> fromType) {
      Assert.notNull(fromType, "From type is required");
      this.fromType = fromType;
      return this;
    }

    /**
     * Builder method used to configure the {@link BiFunction mapping} for the projected query result set.
     *
     * @param mapper {@link BiPredicate} used for mapping; required.
     * @return a new {@link TransformingProjectionBuilder}.
     * @throws IllegalArgumentException if {@link BiFunction mapping} is {@literal null}.
     * @see java.util.function.BiFunction
     * @see TransformingProjectionBuilder
     * @see #mappedWith(Function)
     */
    @Dsl
    public TransformingProjectionBuilder<S, T> mappedWith(@NotNull BiFunction<QueryContext<S, T>, S, T> mapper) {
      Assert.notNull(mapper, "Object mapping function is required");
      return new TransformingProjectionBuilder<>(this.projectionType, this.fromType, mapper);
    }

    /**
     * Builder method used to configure the {@link Function mapping} for the projected query result set.
     *
     * @param mapper {@link Function} used for mapping; required.
     * @return a new {@link TransformingProjectionBuilder}.
     * @throws IllegalArgumentException if {@link Function mapping} is {@literal null}.
     * @see TransformingProjectionBuilder
     * @see java.util.function.Function
     * @see #mappedWith(BiFunction)
     */
    @Dsl
    public TransformingProjectionBuilder<S, T> mappedWith(@NotNull Function<S, T> mapper) {
      Assert.notNull(mapper, "Object mapping function is required");
      BiFunction<QueryContext<S, T>, S, T> function = OqlUtils.asBiFunction(mapper);
      return mappedWith(function);
    }
  }

  /**
   * {@link Builder} for an {@link Oql.Projection} with transformation.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @see org.cp.elements.lang.Builder
   * @see Oql.Projection
   */
  class TransformingProjectionBuilder<S, T> implements Builder<Projection<S, T>> {

    private final BiFunction<QueryContext<S, T>, S, T> mapper;

    private final Class<S> fromType;
    private final Class<T> projectionType;

    private Iterable<QueryFunction<T, ?>> transformations;

    TransformingProjectionBuilder(@NotNull Class<T> projectionType, @NotNull Class<S> fromType,
        @NotNull BiFunction<QueryContext<S, T>, S, T> mapper) {

      this.projectionType = ObjectUtils.requireObject(projectionType, "Projection type is required");
      this.mapper = ObjectUtils.requireObject(mapper, "Object mapping function is required");
      this.fromType = ObjectUtils.requireObject(fromType, "From type is required");
    }

    /**
     * Builder method used to configure the given array of {@link QueryFunction transformations}
     * applied to the query result set.
     *
     * @param transformations array of {@link QueryFunction QueryFunctions} performing the transformations
     * on the query result set.
     * @return this {@link TransformingProjectionBuilder}.
     * @see #apply(Iterable)
     * @see QueryFunction
     */
    @Dsl
    @SuppressWarnings("unchecked")
    public TransformingProjectionBuilder<S, T> apply(QueryFunction<T, ?>... transformations) {
      return apply(ArrayUtils.asIterable(ArrayUtils.nullSafeArray(transformations)));
    }

    /**
     * Builder method used to configure the given {@link Iterable} of {@link QueryFunction transformations}
     * applied to the query result set.
     *
     * @param transformations {@link Iterable} of {@link QueryFunction QueryFunctions} performing the transformations
     * on the query result set.
     * @return this {@link TransformingProjectionBuilder}.
     * @see QueryFunction
     */
    @Dsl
    public TransformingProjectionBuilder<S, T> apply(Iterable<QueryFunction<T, ?>> transformations) {
      this.transformations = CollectionUtils.nullSafeIterable(transformations);
      return this;
    }

    /**
     * Builder method used to remap the {@link QueryResult projected grouping} as an {@link Object} of type {@link T}.
     *
     * @param mapper {@link Function} used to perform the {@link QueryResult projected group result}
     * to {@link T object} mapping; required.
     * @return a new {@link TransformingProjection}.
     * @throws IllegalArgumentException if {@link Function} is {@literal null}.
     * @see java.util.function.Function
     * @see #remappedWith(BiFunction)
     * @see TransformingProjection
     */
    @Dsl
    public TransformingProjection<S, T> remappedWith(Function<QueryResult<T>, T> mapper) {
      Assert.notNull(mapper, "Object remapping function is required");
      BiFunction<QueryContext<S, T>, QueryResult<T>, T> function = (queryContext, result) -> mapper.apply(result);
      return remappedWith(function);
    }

    /**
     * Builder method used to remap the {@link QueryResult projected grouping} as an {@link Object} of type {@link T}.
     *
     * @param mapper {@link BiFunction} used to perform the {@link QueryResult projected group result}
     * to {@link T object} mapping; required.
     * @return a new {@link TransformingProjection}.
     * @throws IllegalArgumentException if {@link BiFunction} is {@literal null}.
     * @throws IllegalStateException if the {@link Iterable} of {@link QueryFunction QueryFunctions}
     * used in the transformation is {@literal null} or {@literal empty}.
     * @see java.util.function.BiFunction
     * @see #remappedWith(Function)
     * @see TransformingProjection
     */
    @Dsl
    public TransformingProjection<S, T> remappedWith(BiFunction<QueryContext<S, T>, QueryResult<T>, T> mapper) {

      Assert.state(CollectionUtils.isNotEmpty(this.transformations), "No transformations defined");
      Assert.notNull(mapper, "Object remapping function is required");

      return new TransformingProjection<>() {

        @Override
        public Class<T> getType() {
          return TransformingProjectionBuilder.this.projectionType;
        }

        @Override
        public Class<S> getFromType() {
          return TransformingProjectionBuilder.this.fromType;
        }

        @Override
        @SuppressWarnings("all")
        public Iterator<QueryFunction<T, ?>> iterator() {
          return TransformingProjectionBuilder.this.transformations.iterator();
        }

        @Override
        public T map(QueryContext<S, T> queryContext, S target) {
          return TransformingProjectionBuilder.this.mapper.apply(queryContext, target);
        }

        @Override
        public T remap(QueryContext<S, T> queryContext, QueryResult<T> result) {
          return mapper.apply(queryContext, result);
        }
      };
    }

    @Override
    public Projection<S, T> build() {

      Assert.state(CollectionUtils.isEmpty(this.transformations),
        "Using transformations requires remapping; you must call remappedWith(..)");

      return new Projection<>() {

        @Override
        public Class<T> getType() {
          return TransformingProjectionBuilder.this.projectionType;
        }

        @Override
        public Class<S> getFromType() {
          return TransformingProjectionBuilder.this.fromType;
        }

        @Override
        public T map(@NotNull QueryContext<S, T> queryContext, S target) {
          return TransformingProjectionBuilder.this.mapper.apply(queryContext, target);
        }
      };
    }
  }
}
