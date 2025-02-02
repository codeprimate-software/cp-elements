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

import java.util.Comparator;
import java.util.Iterator;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.cp.elements.data.oql.support.OqlUtils;
import org.cp.elements.data.oql.support.OqlUtils.ArrayBuilder;
import org.cp.elements.function.CannedPredicates;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Constants;
import org.cp.elements.lang.annotation.Dsl;
import org.cp.elements.lang.annotation.FluentApi;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.service.loader.ServiceLoaderSupport;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.CollectionUtils;
import org.cp.elements.util.stream.StreamUtils;
import org.cp.elements.util.stream.Streamable;

/**
 * Interface defining an {@literal Object Query Language (OQL)}
 * over a {@link Iterable collection} of {@link Object objects}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.BaseOql
 * @see org.cp.elements.lang.annotation.Dsl
 * @see org.cp.elements.lang.annotation.FluentApi
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
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} being queried.
   * @param <T> {@link Class type} of the {@link Object projected elements}.
   * @return the {@link Oql} service provider implementation for {@literal OQL} {@link QueryExecutor}.
   * @see org.cp.elements.data.oql.provider.SimpleQueryExecutor
   * @see QueryExecutor
   */
  <S, T> QueryExecutor<S, T> executor();

  /**
   * Declares the data {@link Select selected} from the {@link Iterable collection} of {@link Object objects}.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} being queried.
   * @param <T> {@link Class type} of the {@link Object projected elements}.
   * @param projection {@link Projection} used to {@literal project} the result of {@link Object objects}
   * queried in the {@link Iterable collection}.
   * @return a {@link Select} object modeling the {@link Projection selected data}.
   * @see org.cp.elements.data.oql.Oql.Projection
   * @see org.cp.elements.data.oql.Oql.Select
   */
  @Dsl
  <S, T> Select<S, T> select(Projection<S, T> projection);

  /**
   * Abstract Data Type (ADT) modeling the {@literal projection} of an {@link Object}
   * from the {@link Iterable collection} as an instance of {@link T type} mapped by
   * the configured {@link BiFunction object mapping} using {@link #mappedWith(BiFunction)}.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} being queried.
   * @param <T> {@link Class type} of the {@link Object projected elements}.
   * @see org.cp.elements.data.oql.Oql.ObjectMapper
   * @see org.cp.elements.data.oql.Oql.Select
   * @see java.lang.FunctionalInterface
   */
  @FunctionalInterface
  interface Projection<S, T> extends ObjectMapper<S, T> {

    @Dsl
    static <S, T> Projection<S, T> as(@NotNull Class<T> type) {
      Assert.notNull(type, "Type is required");
      return () -> type;
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

        @Override
        public Projection<S, S> mappedWith(BiFunction<QueryContext<S, S>, S, S> mapper) {
          throw newUnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
        }

        @Override
        public Projection<S, S> mappedWith(Function<S, S> mapper) {
          throw newUnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
        }
      };
    }

    /**
     * Gets the {@link Class type} of the {@link T projected elements} in the query result.
     *
     * @return the {@link Class type} of the {@link T projected elements} in the query result.
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

    @Dsl
    default Projection<S, T> fromType(@NotNull Class<S> type) {

      Assert.notNull(type, "From type is required");

      return new Projection<>() {

        @Override
        public Class<T> getType() {
          return Projection.this.getType();
        }

        @Override
        public Class<S> getFromType() {
          return type;
        }

        @Override
        public T map(QueryContext<S, T> queryContext, S target) {
          return Projection.this.map(queryContext, target);
        }
      };
    }

    @Dsl
    default Projection<S, T> mappedWith(@NotNull BiFunction<QueryContext<S, T>, S, T> mapper) {

      Assert.notNull(mapper, "Object mapping function is required");

      return new Projection<>() {

        @Override
        public Class<T> getType() {
          return Projection.this.getType();
        }

        @Override
        public Class<S> getFromType() {
          return Projection.this.getFromType();
        }

        @Override
        public T map(QueryContext<S, T> queryContext, S target) {
          return mapper.apply(queryContext, target);
        }
      };
    }

    @Dsl
    default Projection<S, T> mappedWith(@NotNull Function<S, T> mapper) {
      Assert.notNull(mapper, "Object mapping function is required");
      BiFunction<QueryContext<S, T>, S, T> function = (queryContext, target) -> mapper.apply(target);
      return mappedWith(function);
    }
  }

  /**
   * Abstract Data Type (ADT) modeling the {@literal select clause} in a {@link Query}.
   * <p>
   * Specifically, {@link Select} identifies the {@literal selected data} from the {@link Object objects}
   * in the {@link Iterable collection} forming the result set.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} being queried.
   * @param <T> {@link Class type} of the {@link Object projected elements}.
   * @see org.cp.elements.data.oql.Oql.Projection
   */
  interface Select<S, T> {

    /**
     * Determines whether the result set of the {@link Query} should be unique.
     * <p>
     * Defaults to {@link false}.
     *
     * @return a boolean value indicating whether the result set of the {@link Query} should be unique.
     */
    default boolean isDistinct() {
      return false;
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

  @FunctionalInterface
  interface Distinct<S, T> {
    @Dsl From<S, T> from(Iterable<S> collection);
  }

  /**
   * Abstract Data Type (ADT) modeling the {@literal from clause} in a {@link Query}.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} being queried.
   * @param <T> {@link Class type} of the {@link Object projected elements}.
   * @see org.cp.elements.data.oql.Oql.ExecutableQuery
   * @see org.cp.elements.data.oql.Oql.GroupBySpec
   * @see org.cp.elements.data.oql.Oql.LimitSpec
   * @see org.cp.elements.data.oql.Oql.OrderBySpec
   */
  interface From<S, T> extends ExecutableQuery<S, T>, GroupBySpec<S, T>, LimitSpec<S, T>, OrderBySpec<S, T> {

    /**
     * Returns the {@link Iterable collection} from which {@link Object objects} are {@link Select selected}.
     *
     * @return the {@link Iterable collection} from which {@link Object objects} are {@link Select selected}.
     * @see java.lang.Iterable
     */
    Iterable<S> getCollection();

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
     * Returns a {@link Select} object identify the {@literal selected data} from the {@link Iterable collection}
     * returned in the query result set.
     *
     * @return an {@link Select} object identify the {@literal selected data} from the {@link Iterable collection}
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
    default Where<S, T> where(Predicate<S> predicate) {
      throw newUnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
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
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} being queried.
   * @param <T> {@link Class type} of the {@link Object projected elements}.
   * @see org.cp.elements.data.oql.Oql.ExecutableQuery
   * @see org.cp.elements.data.oql.Oql.GroupBySpec
   * @see org.cp.elements.data.oql.Oql.LimitSpec
   * @see org.cp.elements.data.oql.Oql.OrderBySpec
   * @see java.lang.FunctionalInterface
   */
  @FunctionalInterface
  interface Where<S, T> extends ExecutableQuery<S, T>, GroupBySpec<S, T>, LimitSpec<S, T>, OrderBySpec<S, T> {

    static <S, T> Where<S, T> compose(@NotNull Where<S, T> where, @NotNull Predicate<S> predicate) {

      Assert.notNull(where, "Where clause is required");
      Assert.notNull(predicate, "Predicate is required");

      return new Where<>() {

        @Override
        public From<S, T> getFrom() {
          return where.getFrom();
        }

        @Override
        public Predicate<S> getPredicate() {
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
    Predicate<S> getPredicate();

    @Dsl
    default Where<S, T> and(@NotNull Predicate<S> predicate) {
      return compose(this, getPredicate().and(predicate));
    }

    @Dsl
    default Where<S, T> or(@NotNull Predicate<S> predicate) {
      return compose(this, getPredicate().or(predicate));
    }
  }

  /**
   * Abstract Data Type (ADT) modeling the {@literal order by clause} in a {@link Query}.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} being queried.
   * @param <T> {@link Class type} of the {@link Object projected elements}.
   * @see org.cp.elements.data.oql.Oql.ExecutableQuery
   * @see org.cp.elements.data.oql.Oql.LimitSpec
   * @see org.cp.elements.util.stream.Streamable
   * @see java.lang.FunctionalInterface
   * @see java.lang.Iterable
   */
  @FunctionalInterface
  interface OrderBy<S, T> extends ExecutableQuery<S, T>, Iterable<Comparator<T>>, LimitSpec<S, T>,
      Streamable<Comparator<T>> {

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

      ArrayBuilder<Comparator<T>> comparatorArrayBuilder = OqlUtils.asArray(this);

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
   * Interface defining a contract for {@literal OQL} components capable of defining a {@literal limit}
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
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} being queried.
   * @param <T> {@link Class type} of the {@link Object projected elements}.
   * @see org.cp.elements.data.oql.Oql.ExecutableQuery
   * @see org.cp.elements.data.oql.Oql.LimitSpec
   * @see org.cp.elements.data.oql.Oql.OrderBySpec
   * @see java.lang.FunctionalInterface
   */
  @FunctionalInterface
  interface GroupBy<S, T> extends ExecutableQuery<S, T>, LimitSpec<S, T>, OrderBySpec<S, T> {

    static <S, T> GroupBy<S, T> of(@NotNull From<S, T> from, @NotNull Grouping<S> grouping) {

      Assert.notNull(from, "From clause is required");
      Assert.notNull(grouping, "Grouping is required");

      return new GroupBy<>() {

        @Override
        public From<S, T> getFrom() {
          return from;
        }

        @Override
        public Grouping<S> getGrouping() {
          return grouping;
        }
      };
    }

    /**
     * Gets the {@link Grouping} defining the {@literal groups} in the result set of the {@link Query}.
     *
     * @return the {@link Grouping} defining the {@literal groups} in the result set of the {@link Query}.
     * @see org.cp.elements.data.oql.Oql.Grouping
     */
    Grouping<S> getGrouping();

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

    default S group(S target) {
      throw newUnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
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
        public Grouping<S> getGrouping() {
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
   * @see org.cp.elements.data.oql.Oql.GroupBy
   * @see org.cp.elements.data.oql.Oql.Grouping
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
    default GroupBy<S, T> groupBy(Grouping<S> grouping) {
      throw newUnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
    }
  }

  /**
   * Interface defining a contract for an {@literal OQL} {@link Object} that can be executed or counted.
   *
   * @param <T> {@link Class type} of {@link Object objects} in the {@link Projection projected result set}.
   * @see java.lang.FunctionalInterface
   */
  @FunctionalInterface
  interface Executable<T> {

    default Long count() {
      Iterable<T> results = execute();
      Stream<T> stream = StreamUtils.stream(CollectionUtils.nullSafeIterable(results));
      return stream.count();
    }

    Iterable<T> execute();

  }

  /**
   * Interface defining an {@literal OQL statement} as an {@link Executable} {@link Query}
   * with a {@link FromReference referecne} to the {@link From} clause.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} being queried.
   * @param <T> {@link Class type} of the {@link Object projected elements}.
   * @see org.cp.elements.data.oql.Oql.FromReference
   * @see org.cp.elements.data.oql.Oql.Executable
   * @see org.cp.elements.data.oql.Query
   */
  interface ExecutableQuery<S, T> extends Executable<T>, FromReference<S, T> {

    default Query<S, T> asQuery() {
      return Query.from(getFrom());
    }

    @Override
    default Iterable<T> execute() {
      return asQuery().execute();
    }
  }

  /**
   * Interface defining a {@literal reference} to an instance of {@link From}.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} being queried.
   * @param <T> {@link Class type} of the {@link Object projected elements}.
   * @see org.cp.elements.data.oql.Oql.From
   */
  interface FromReference<S, T> {

    default From<S, T> getFrom() {
      throw newIllegalStateException(NO_FROM);
    }
  }

  /**
   * Interface defining a {@literal group} of similar {@link Object objects} from a {@link Iterable collection}.
   *
   * @param <S> {@link Class type} of {@link Object} from which the {@link Object value}
   * used in the {@literal grouping} is calculated.
   * @see org.cp.elements.util.stream.Streamable
   * @see java.lang.FunctionalInterface
   * @see java.lang.Iterable
   */
  @FunctionalInterface
  interface Grouping<S> extends Iterable<Function<S, ?>>, Streamable<Function<S, ?>> {

    @SafeVarargs
    static <S> Grouping<S> of(Function<S, ?>... functions) {
      return ArrayUtils.asIterable(functions)::iterator;
    }

    default int group(S target) {

      return stream()
        .map(function -> function.apply(target))
        .map(Object::hashCode)
        .reduce(Integer::sum)
        .orElse(0);
    }

    default Stream<Function<S, ?>> stream() {
      return StreamUtils.stream(this);
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
        : new Oql.Provider() { });
    }

    @Override
    default Class<Oql> getType() {
      return Oql.class;
    }
  }
}
