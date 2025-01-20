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
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.cp.elements.data.mapping.UndefinedMappingException;
import org.cp.elements.function.CannedPredicates;
import org.cp.elements.function.FunctionUtils;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Constants;
import org.cp.elements.lang.DslExtension;
import org.cp.elements.lang.FluentApiExtension;
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
 * @see java.lang.Iterable
 * @see java.util.Comparator
 * @see java.util.function.Function
 * @see java.util.function.Predicate
 * @see org.cp.elements.lang.DslExtension
 * @see org.cp.elements.lang.FluentApiExtension
 * @see org.cp.elements.lang.annotation.Dsl
 * @see org.cp.elements.lang.annotation.FluentApi
 * @since 2.0.0
 */
@FluentApi
@SuppressWarnings("unused")
public interface Oql extends DslExtension, FluentApiExtension {

  String NO_FROM = "From not initialized";

  /**
   * Returns the default, configured {@link Oql} service provider implementation.
   *
   * @return the default, configured {@link Oql} service provider implementation.
   * @see org.cp.elements.data.oql.provider.SimpleOqlProvider
   */
  static Oql defaultProvider() {
    return Oql.Provider.getLoader().getServiceInstance();
  }

  /**
   * Returns the {@literal OQL} service provider implementation of the {@link Oql} {@link QueryExecutor}.
   *
   * @param <S> {@link Class type} of {@link Object elements} in the {@link Iterable collection} being queried.
   * @param <T> {@link Class type} of the projected {@link Object elements}.
   * @return the {@link Oql} service provider implementation of the {@literal OQL} {@link QueryExecutor}.
   * @see org.cp.elements.data.oql.provider.SimpleQueryExecutor
   * @see org.cp.elements.data.oql.Oql.QueryExecutor
   */
  <S, T> QueryExecutor<S, T> executor();

  /**
   * Declares the data {@link Select selected } from the {@link Iterable collection} of {@link Object elements}.
   *
   * @param <S> {@link Class type} of {@link Object elements} in the {@link Iterable collection} being queried.
   * @param <T> {@link Class type} of the projected {@link Object elements}.
   * @param projection {@link Projection} used to {@literal project} the result of {@link Object elements}
   * queried in the {@link Iterable collection}.
   * @return a {@link Select object} modeling the data selected with the given {@link Projection}.
   * @see org.cp.elements.data.oql.Oql.Select
   */
  @Dsl
  <S, T> Select<S, T> select(Projection<S, T> projection);

  /**
   * Interface defining a contract to {@literal project} an {@link Object element} from the {@link Iterable collection}
   * as an {@link Object} of the declared {@link T type} mapped by the configured {@link Function object mapping}
   * using {@link #mappedWith(BiFunction)}.
   *
   * @param <S> {@link Class type} of {@link Object elements} in the {@link Iterable collection} being queried.
   * @param <T> {@link Class type} of the projected {@link Object elements}.
   * @see org.cp.elements.data.oql.Oql.ObjectMapper
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
          throw newUnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
        }
      };
    }

    /**
     * Gets the {@link Class type} of the {@link Object elements} in the {@link Iterable collection} being queried.
     * <p>
     * Defaults to {@link Object} class.
     *
     * @return the {@link Class type} of the {@link Object elements} in the {@link Iterable collection} being queried.
     */
    @SuppressWarnings("unchecked")
    default Class<S> getFromType() {
      return (Class<S>) Object.class;
    }

    /**
     * Gets the {@link Class type} of the {@link T projected elements} in the query result.
     *
     * @return the {@link Class type} of the {@link T projected elements } in the query result.
     */
    Class<T> getType();

    @Dsl
    default Projection<S, T> fromType(@NotNull Class<S> type) {

      Assert.notNull(type, "From type is required");

      return new Projection<>() {

        @Override
        public Class<S> getFromType() {
          return type;
        }

        @Override
        public Class<T> getType() {
          return Projection.this.getType();
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
        public Class<S> getFromType() {
          return Projection.this.getFromType();
        }

        @Override
        public Class<T> getType() {
          return Projection.this.getType();
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
      return mappedWith((queryContext, target) -> mapper.apply(target));
    }
  }

  /**
   * Abstract Data Type (ADT) modeling the {@literal select clause} in the {@link Query}.
   * <p>
   * Specifically, {@link Select} identifies the {@literal data selected} from the {@link Object elements}
   * in the {@link Iterable collection} to form the result set.
   *
   * @param <S> {@link Class type} of {@link Object elements} in the {@link Iterable collection} being queried.
   * @param <T> {@link Class type} of {@link Objects} in the {@link Projection projected result set}.
   */
  interface Select<S, T> {

    /**
     * Determines whether the {@link Select selection} of results from the {@link Query} should be unique.
     * <p>
     * Defaults to {@link false}.
     *
     * @return a boolean value indicating whether the {@link Select selection} of results from the {@link Query}
     * should be unique.
     */
    default boolean isDistinct() {
      return false;
    }

    /**
     * Returns the {@link Projection} of the {@literal selection} of {@link T elements}
     * in the {@link Iterable collection}.
     *
     * @return the {@link Projection} of the {@literal selection} of {@link T elements}
     * in the {@link Iterable collection}.
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
   * Abstract Data Type (ADT) modeling the {@literal from clause} in the {@link Query}.
   *
   * @param <S> {@link Class type} of {@link Object elements} in the {@link Iterable collection} being queried.
   * @param <T> {@link Class type} of {@link Objects} in the {@link Projection projected result set}.
   * @see org.cp.elements.data.oql.Oql.ExecutableQuery
   * @see org.cp.elements.data.oql.Oql.Limited
   */
  interface From<S, T> extends ExecutableQuery<S, T>, Limited<S, T> {

    /**
     * Returns the {@link Iterable collection} from which the {@link Object elements} are {@link Select selected}.
     *
     * @return the {@link Iterable collection} from which the {@link Object elements} are {@link Select selected}.
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
     * Returns an {@link Select} object identify the {@literal data selected} from the {@link Iterable collection}
     * and returned in the query result set.
     *
     * @return an {@link Select} object identify the {@literal data selected} from the {@link Iterable collection}
     * and returned in the query result set.
     * @see org.cp.elements.data.oql.Oql.Select
     */
    Select<S, T> getSelection();

    /**
     * Gets the {@link Class type} of the {@link T projected elements} in the query result.
     *
     * @return the {@link Class type} of the {@link T projected elements } in the query result.
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

    /**
     * Gets an {@link Optional} {@link OrderBy} clause of the {@link Query}.
     *
     * @return an {@link Optional} {@link OrderBy} clause of the {@link Query}.
     * @see org.cp.elements.data.oql.Oql.OrderBy
     * @see java.util.Optional
     */
    default Optional<OrderBy<S, T>> getOrderBy() {
      return Optional.empty();
    }

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
    default Where<S, T> where(Predicate<S> predicate) {
      throw newUnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
    }

    @Dsl
    default OrderBy<S, T> orderBy(Comparator<S> comparator) {
      throw newUnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
    }

    @Dsl
    default ExecutableQuery<S, T> limit(long limit) {
      throw newUnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
    }

    @Dsl
    default GroupBy<S, T> groupBy(Grouping<S> grouping) {
      throw newUnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
    }
  }

  /**
   * Abstract Data Type (ADT) modeling the {@literal where clause} in the {@link Query}.
   *
   * @param <S> {@link Class type} of {@link Object elements} in the {@link Iterable collection} being queried.
   * @param <T> {@link Class type} of {@link Objects} in the {@link Projection projected result set}.
   * @see org.cp.elements.data.oql.Oql.ExecutableQuery
   * @see org.cp.elements.data.oql.Oql.Limited
   */
  @FunctionalInterface
  interface Where<S, T> extends ExecutableQuery<S, T>, Limited<S, T> {

    static <S, T> Where<S, T> compose(@NotNull Where<S, T> where, @NotNull Predicate<S> predicate) {

      Assert.notNull(where, "Where is required");
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

    @Dsl
    default OrderBy<S, T> orderBy(Comparator<S> comparator) {
      throw newUnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
    }

    @Dsl
    default GroupBy<S, T> groupBy(Grouping<S> grouping) {
      throw newUnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
    }
  }

  /**
   * Abstract Data Type (ADT) modeling the {@literal order by clause} in the {@link Query}.
   *
   * @param <S> {@link Class type} of {@link Object elements} in the {@link Iterable collection} being queried.
   * @param <T> {@link Class type} of {@link Objects} in the {@link Projection projected result set}.
   * @see org.cp.elements.data.oql.Oql.ExecutableQuery
   * @see org.cp.elements.data.oql.Oql.Limited
   */
  @FunctionalInterface
  interface OrderBy<S, T> extends ExecutableQuery<S, T>, Limited<S, T> {

    static <S, T> OrderBy<S, T> of(@NotNull From<S, T> from, @NotNull Comparator<S> comparator) {

      Assert.notNull(from, "From is required");
      Assert.notNull(comparator, "Comparator is required");

      return new OrderBy<>() {

        @Override
        public From<S, T> getFrom() {
          return from;
        }

        @Override
        public Comparator<S> getOrder() {
          return comparator;
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
    Comparator<S> getOrder();

    @Dsl
    default OrderBy<S, T> ascending() {
      return this;
    }

    @Dsl
    default OrderBy<S, T> descending() {
      return of(getFrom(), getOrder().reversed());
    }

    @Dsl
    default OrderBy<S, T> thenOrderBy(Comparator<S> comparator) {
      return of(getFrom(), this.getOrder().thenComparing(comparator));
    }
  }

  /**
   * Abstract Data Type (ADT) modeling the {@literal limit clause} in the {@link Query}.
   *
   * @param <S> {@link Class type} of {@link Object elements} in the {@link Iterable collection} being queried.
   * @param <T> {@link Class type} of {@link Objects} in the {@link Projection projected result set}.
   * @see org.cp.elements.data.oql.Oql.FromReference
   */
  interface Limited<S, T> extends FromReference<S, T> {

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
   * Abstract Data Type (ADT) modeling the {@literal group by clause} in the {@link Query}.
   *
   * @param <S> {@link Class type} of {@link Object elements} in the {@link Iterable collection} being queried.
   * @param <T> {@link Class type} of {@link Objects} in the {@link Projection projected result set}.
   * @see org.cp.elements.data.oql.Oql.ExecutableQuery
   * @see org.cp.elements.data.oql.Oql.Limited
   */
  @FunctionalInterface
  interface GroupBy<S, T> extends ExecutableQuery<S, T>, Limited<S, T> {

    static <S, T> GroupBy<S, T> of(@NotNull From<S, T> from, @NotNull Grouping<S> grouping) {

      Assert.notNull(from, "From is required");
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

    Grouping<S> getGrouping();

    @SuppressWarnings("unchecked")
    default Predicate<T> getPredicate() {
      return (Predicate<T>) CannedPredicates.ACCEPT_ALL;
    }

    default S compute(S target) {
      throw newUnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
    }

    @Dsl
    default GroupBy<S, T> having(Predicate<T> predicate) {

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
          return FunctionUtils.nullSafePredicateMatchingAll(predicate);
        }
      };
    }

    @Dsl
    default OrderBy<S, T> orderBy(Comparator<S> comparator) {
      throw newUnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
    }
  }

  /**
   * Interface defining a contract for an {@literal OQL} {@link Query} that can be executed or counted.
   *
   * @param <T> {@link Class type} of the result.
   */
  interface Executable<T> {

    default Long count() {
      Iterable<T> results = execute();
      return StreamUtils.stream(CollectionUtils.nullSafeIterable(results)).count();
    }

    default Iterable<T> execute() {
      throw newUnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
    }
  }

  /**
   * Interface defining an {@literal OQL} statement as an {@link Executable} {@link Query}
   * with a {@link FromReference referecne} to the {@link From} clause.
   *
   * @param <S> source {@link Class type}.
   * @param <T> target {@link Class type}.
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
   * @param <S> source {@link Class type}.
   * @param <T> target {@link Class type}.
   * @see org.cp.elements.data.oql.Oql.From
   */
  interface FromReference<S, T> {

    default From<S, T> getFrom() {
      throw newIllegalStateException(NO_FROM);
    }
  }

  /**
   * Interface defining a {@literal group} of similar {@link Object elements} from a {@link Iterable collection}.
   *
   * @param <S> {@link Class type} of {@link Object} from which the {@link Object value}
   * used in the grouping is calculated.
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
   * Interface defining a contract for an {@literal OQL} component capable of mapping an {@link Object}
   * from one {@link Class type} to another {@link Class type}.
   *
   * @param <S> source {@link Class type}.
   * @param <T> target {@link Class type}.
   * @see org.cp.elements.data.oql.QueryContext
   */
  interface ObjectMapper<S, T> {

    default T map(QueryContext<S, T> queryContext, S target) {
      throw UndefinedMappingException.INSTANCE;
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

  /**
   * {@literal OQL} component capable of executing an {@literal OQL} {@link Query}.
   *
   * @param <S> {@link Class type} defining the {@link Object elements} in the {@link Iterable collection} to query.
   * @param <T> {@link Class type} of {@link Object project elements} in the {@link Iterable result}.
   * @see org.cp.elements.data.oql.Query
   * @see java.lang.Iterable
   */
  interface QueryExecutor<S, T> {

    default Iterable<T> execute(Query<S, T> query) {
      throw newUnsupportedOperationException(Constants.NOT_IMPLEMENTED);
    }
  }
}
