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
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.function.Predicate;

import org.cp.elements.data.mapping.UndefinedMappingException;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Constants;
import org.cp.elements.lang.DslExtension;
import org.cp.elements.lang.FluentApiExtension;
import org.cp.elements.lang.annotation.Dsl;
import org.cp.elements.lang.annotation.FluentApi;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.service.loader.ServiceLoaderSupport;
import org.cp.elements.util.CollectionUtils;
import org.cp.elements.util.stream.StreamUtils;

/**
 * Interface defining an {@literal Object Query Language (OQL)}
 * over a {@link Iterable collection} of {@link Object objects}.
 *
 * @author John Blum
 * @see java.lang.Iterable
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
   * Returns the default configured {@link Oql} service provider implementation.
   *
   * @return the default configured {@link Oql} service provider implementation.
   * @see org.cp.elements.data.oql.provider.SimpleOqlProvider
   */
  static Oql defaultProvider() {
    return Oql.Provider.getLoader().getServiceInstance();
  }

  /**
   * Returns the {@link Oql} service provider implementation of the {@literal OQL} {@link QueryExecutor}.
   *
   * @param <S> {@link Class type} of {@link Object elements} in the {@link Iterable collection} being queried.
   * @param <T> {@link Class type} of the {@link Projection}.
   * @return the {@link Oql} service provider implementation of the {@literal OQL} {@link QueryExecutor}.
   * @see org.cp.elements.data.oql.provider.SimpleQueryExecutor
   * @see org.cp.elements.data.oql.Oql.QueryExecutor
   */
  <S, T> QueryExecutor<S, T> executor();

  /**
   * Declares the {@link Select selected data} from the {@link Iterable collection} of {@link Object elements}.
   *
   * @param <S> {@link Class type} of {@link Object elements} in the {@link Iterable collection} being queried.
   * @param <T> {@link Class type} of the {@link Projection}.
   * @param projection {@link Projection} used to {@literal project} the {@link Object elements}
   * in the {@link Iterable collection}.
   * @return a {@link Select object} modeling the selection with the {@link Projection}.
   * @see org.cp.elements.data.oql.Oql.Select
   */
  @Dsl
  <S, T> Select<S, T> select(Projection<S, T> projection);

  /**
   * Interface defining a contract to {@literal project} an {@link Object element} from the {@link Iterable collection}
   * into an {@link Object} of the declared {@link Class type} as defined by the {@link ObjectMapper object mapping}.
   *
   * @param <S> {@link Class type} of {@link Object elements} in the {@link Iterable collection} being queried.
   * @param <T> {@link Class type} of the {@link Projection}.
   * @see org.cp.elements.data.oql.Oql.ObjectMapper
   */
  @FunctionalInterface
  interface Projection<S, T> extends ObjectMapper<S, T> {

    @Dsl
    static <S, T> Projection<S, T> of(Class<T> type) {
      Assert.notNull(type, "Type is required");
      return () -> type;
    }

    @Dsl
    static <S> Projection<S, S> star() {

      return new Projection<>() {

        private final Function<S, S> mapper = Function.identity();

        @Override
        public Class<S> getType() {
          return getFromType();
        }

        @Override
        public S map(S target) {
          return this.mapper.apply(target);
        }

        @Override
        public Projection<S, S> mappedWith(Function<S, S> mapper) {
          throw newUnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
        }
      };
    }

    @SuppressWarnings("unchecked")
    default Class<S> getFromType() {
      return (Class<S>) Object.class;
    }

    Class<T> getType();

    @Dsl
    default Projection<S, T> fromType(Class<S> type) {

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
        public T map(S target) {
          return Projection.this.map(target);
        }
      };
    }

    @Dsl
    default Projection<S, T> mappedWith(@NotNull Function<S, T> mapper) {

      Assert.notNull(mapper, "Mapping Function is required");

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
        public T map(S target) {
          return mapper.apply(target);
        }
      };
    }
  }

  interface Select<S, T> {

    default boolean isDistinct() {
      return false;
    }

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

  interface From<S, T> extends Executable<T> {

    Iterable<S> getCollection();

    Select<S, T> getSelection();

    default Class<S> getType() {
      return getSelection().getProjection().getFromType();
    }

    default Optional<Where<S, T>> getWhere() {
      return Optional.empty();
    }

    default Optional<OrderBy<S, T>> getOrderBy() {
      return Optional.empty();
    }

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
    default GroupBy<S, T> groupBy(Grouping grouping) {
      throw newUnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
    }

    @Override
    default Iterable<T> execute() {
      return asQuery().execute();
    }

    default Query<S, T> asQuery() {
      return Query.from(this);
    }
  }

  @FunctionalInterface
  interface Where<S, T> extends Executable<T> {

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

    default From<S, T> getFrom() {
      throw newIllegalStateException(NO_FROM);
    }

    Predicate<S> getPredicate();

    @Dsl
    default Where<S, T> and(Predicate<S> predicate) {
      return compose(this, getPredicate().and(predicate));
    }

    @Dsl
    default Where<S, T> or(Predicate<S> predicate) {
      return compose(this, getPredicate().or(predicate));
    }

    @Dsl
    default OrderBy<S, T> orderBy(Comparator<S> comparator) {
      throw newUnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
    }

    @Dsl
    default GroupBy<S, T> groupBy(Grouping grouping) {
      throw newUnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
    }

    @Override
    default Iterable<T> execute() {
      return getFrom().execute();
    }
  }

  @FunctionalInterface
  interface OrderBy<S, T> extends Executable<T> {

    static <S, T> OrderBy<S, T> of(From<S, T> from, Comparator<S> comparator) {

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

    default From<S, T> getFrom() {
      throw newIllegalStateException(NO_FROM);
    }

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

    @Override
    default Iterable<T> execute() {
      return getFrom().execute();
    }
  }

  @FunctionalInterface
  interface GroupBy<S, T> extends Executable<T> {

    default From<S, T> getFrom() {
      throw newIllegalStateException(NO_FROM);
    }

    @Dsl
    GroupBy<S, T> having(Predicate<T> predicate);

    @Dsl
    default OrderBy<S, T> orderBy(Comparator<S> comparator) {
      throw newUnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
    }

    @Override
    default Iterable<T> execute() {
      return getFrom().execute();
    }
  }

  interface Executable<T> {

    default Long count() {
      Iterable<T> results = execute();
      return StreamUtils.stream(CollectionUtils.nullSafeIterable(results)).count();
    }

    default Iterable<T> execute() {
      throw newUnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
    }
  }

  interface Grouping {
  }

  interface ObjectMapper<S, T> {

    default T map(S target) {
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

  interface QueryExecutor<S, T> {

    default Iterable<T> execute(Query<S, T> query) {
      throw newUnsupportedOperationException(Constants.NOT_IMPLEMENTED);
    }
  }
}
