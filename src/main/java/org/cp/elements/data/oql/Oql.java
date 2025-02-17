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

import org.cp.elements.data.oql.support.Grouping;
import org.cp.elements.function.CannedPredicates;
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
   * @param collection {@link Iterable} to query.
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
   */
  @FunctionalInterface
  interface Projection<S, T> extends ObjectMapper<S, T> {

    @Dsl
    static <S, T> ProjectionBuilder<S, T> as(@NotNull Class<T> type) {
      Assert.notNull(type, "Type is required");
      return new ProjectionBuilder<>(type);
    }

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
   * @see org.cp.elements.data.oql.QueryResult
   * @see org.cp.elements.util.stream.Streamable
   * @see java.lang.Iterable
   */
  interface TransformingProjection<S, T>
    extends Iterable<QueryFunction<T, ?>>, Projection<S, T>, Streamable<QueryFunction<T, ?>>
  {

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
   */
  interface Select<S, T> {

    boolean DEFAULT_DISTINCT = false;

    /**
     * Determines whether the result set of the {@link Query} should be unique.
     * <p>
     * Defaults to {@link false}.
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

    @Dsl
    default Distinct<S, T> distinct() {
      throw newUnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
    }

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

    @Dsl From<S, T> from(Iterable<S> collection);
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

    @Dsl
    default Where<S, T> where(BiPredicate<QueryArguments, S> predicate) {
      throw newUnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
    }

    @Dsl
    default Where<S, T> where(Predicate<S> predicate) {
      return where(Where.asBiPredicate(predicate));
    }

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

    BiPredicate<QueryArguments, ?> ACCEPT_ALL_QUERY_PREDICATE = (queryArguments, target) -> true;

    static <S> BiPredicate<QueryArguments, S> asBiPredicate(@NotNull Predicate<S> predicate) {
      return (queryArguments, target) -> predicate.test(target);
    }

    static <S> Predicate<S> asPredicate(@NotNull BiPredicate<QueryArguments, S> predicate) {
      return asPredicate(predicate, QueryArguments.empty());
    }

    static <S> Predicate<S> asPredicate(@NotNull BiPredicate<QueryArguments, S> predicate, QueryArguments arguments) {
      Assert.notNull(predicate, "Predicate is required");
      return target -> predicate.test(arguments, target);
    }

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
     * @see java.util.function.Predicate
     */
    BiPredicate<QueryArguments, S> getPredicate();

    @Dsl
    default Where<S, T> and(@NotNull BiPredicate<QueryArguments, S> predicate) {
      return compose(this, getPredicate().and(predicate));
    }

    @Dsl
    default Where<S, T> and(@NotNull Predicate<S> predicate) {
      Assert.notNull(predicate, "Query Predicate is required");
      BiPredicate<QueryArguments, S> biPredicate = Where.asBiPredicate(predicate);
      return and(biPredicate);
    }

    @Dsl
    default Where<S, T> or(@NotNull BiPredicate<QueryArguments, S> predicate) {
      return compose(this, getPredicate().or(predicate));
    }

    @Dsl
    default Where<S, T> or(@NotNull Predicate<S> predicate) {
      Assert.notNull(predicate, "Query Predicate is required");
      BiPredicate<QueryArguments, S> biPredicate = Where.asBiPredicate(predicate);
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

    @Dsl
    default OrderBy<S, T> ascending() {
      return this;
    }

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

    @Dsl
    default OrderBy<S, T> thenOrderBy(@NotNull Comparator<T> comparator) {
      return of(getFrom(), getOrder(), comparator);
    }

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

    @Dsl
    default <U extends Comparable<U>> OrderBy<S, T> orderBy(@NotNull Function<T, U> orderingFunction) {
      Assert.notNull(orderingFunction, "Function defining order is required");
      Comparator<T> comparator = Comparator.comparing(orderingFunction);
      return orderBy(comparator);
    }

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
     * @see Grouping
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
    default Predicate<T> getPredicate() {
      return (Predicate<T>) CannedPredicates.ACCEPT_ALL;
    }

    @Dsl
    default GroupBy<S, T> having(@NotNull Predicate<T> predicate) {

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
        public Predicate<T> getPredicate() {
          return predicate;
        }
      };
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

    @Dsl
    default GroupBy<S, T> groupBy(Grouping<T> grouping) {
      throw newUnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
    }

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

    @SuppressWarnings("all")
    default Long count() {
      Iterable<T> results = execute();
      Stream<T> stream = StreamUtils.stream(CollectionUtils.nullSafeIterable(results));
      Long count = stream.count();
      return count;
    }

    default Iterable<T> execute(QueryArgument<?>... arguments) {
      return execute(QueryArguments.of(arguments));
    }

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

    static Oql.Provider getLoader() {
      return LOADER_REFERENCE.updateAndGet(loader -> loader != null ? loader
        : new Oql.Provider() {
      });
    }

    @Override
    default Class<Oql> getType() {
      return Oql.class;
    }
  }

  class ProjectionBuilder<S, T> {

    private final Class<T> projectionType;

    @SuppressWarnings("unchecked")
    private Class<S> fromType = (Class<S>) Object.class;

    ProjectionBuilder(@NotNull Class<T> projectionType) {
      this.projectionType = ObjectUtils.requireObject(projectionType, "Projection type is required");
    }

    @Dsl
    public ProjectionBuilder<S, T> fromType(@NotNull Class<S> fromType) {
      Assert.notNull(fromType, "From type is required");
      this.fromType = fromType;
      return this;
    }

    @Dsl
    public ProjectionTransformationBuilder<S, T> mappedWith(@NotNull BiFunction<QueryContext<S, T>, S, T> mapper) {
      Assert.notNull(mapper, "Object mapping function is required");
      return new ProjectionTransformationBuilder<>(this.projectionType, this.fromType, mapper);
    }

    @Dsl
    public ProjectionTransformationBuilder<S, T> mappedWith(@NotNull Function<S, T> mapper) {
      Assert.notNull(mapper, "Object mapping function is required");
      BiFunction<QueryContext<S, T>, S, T> function = (queryContext, target) -> mapper.apply(target);
      return mappedWith(function);
    }
  }

  class ProjectionTransformationBuilder<S, T> implements Builder<Projection<S, T>> {

    private final BiFunction<QueryContext<S, T>, S, T> mapper;

    private final Class<S> fromType;
    private final Class<T> projectionType;

    private Iterable<QueryFunction<T, ?>> transformations;

    protected ProjectionTransformationBuilder(@NotNull Class<T> projectionType, @NotNull Class<S> fromType,
      @NotNull BiFunction<QueryContext<S, T>, S, T> mapper)
    {

      this.projectionType = ObjectUtils.requireObject(projectionType, "Projection type is required");
      this.mapper = ObjectUtils.requireObject(mapper, "Object mapping function is required");
      this.fromType = ObjectUtils.requireObject(fromType, "From type is required");
    }

    @Dsl
    @SuppressWarnings("unchecked")
    public <V> ProjectionTransformationBuilder<S, T> apply(QueryFunction<T, ?>... transformations) {
      return apply(ArrayUtils.asIterable(ArrayUtils.nullSafeArray(transformations)));
    }

    @Dsl
    public ProjectionTransformationBuilder<S, T> apply(Iterable<QueryFunction<T, ?>> transformations) {
      this.transformations = CollectionUtils.nullSafeIterable(transformations);
      return this;
    }

    @Dsl
    public TransformingProjection<S, T> remappedWith(Function<QueryResult<T>, T> mapper) {
      Assert.notNull(mapper, "Object remapping function is required");
      BiFunction<QueryContext<S, T>, QueryResult<T>, T> function = ((queryContext, result) -> mapper.apply(result));
      return remappedWith(function);
    }

    @Dsl
    public TransformingProjection<S, T> remappedWith(BiFunction<QueryContext<S, T>, QueryResult<T>, T> mapper) {

      Assert.state(CollectionUtils.isNotEmpty(this.transformations), "No transformations defined");
      Assert.notNull(mapper, "Object remapping function is required");

      return new TransformingProjection<>() {

        @Override
        public Class<T> getType() {
          return ProjectionTransformationBuilder.this.projectionType;
        }

        @Override
        public Class<S> getFromType() {
          return ProjectionTransformationBuilder.this.fromType;
        }

        @Override
        @SuppressWarnings("all")
        public Iterator<QueryFunction<T, ?>> iterator() {
          return ProjectionTransformationBuilder.this.transformations.iterator();
        }

        @Override
        public T map(QueryContext<S, T> queryContext, S target) {
          return ProjectionTransformationBuilder.this.mapper.apply(queryContext, target);
        }

        @Override
        public T remap(QueryContext<S, T> queryContext, QueryResult<T> result) {
          return mapper.apply(queryContext, result);
        }
      };
    }

    @Override
    public Projection<S, T> build() {

      Assert.state(CollectionUtils.isEmpty(this.transformations), "Using transformations requires remapping;"
        + " you must call remappedWith(..)");

      return new Projection<>() {

        @Override
        public Class<T> getType() {
          return ProjectionTransformationBuilder.this.projectionType;
        }

        @Override
        public Class<S> getFromType() {
          return ProjectionTransformationBuilder.this.fromType;
        }

        @Override
        public T map(@NotNull QueryContext<S, T> queryContext, S target) {
          return ProjectionTransformationBuilder.this.mapper.apply(queryContext, target);
        }
      };
    }
  }
}
