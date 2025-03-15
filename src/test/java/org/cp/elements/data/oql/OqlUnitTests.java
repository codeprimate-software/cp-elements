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

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.assertThatIllegalStateException;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Collections;
import java.util.Map;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.BiPredicate;
import java.util.function.Function;

import org.junit.jupiter.api.Test;

import org.cp.elements.data.oql.Oql.From;
import org.cp.elements.data.oql.Oql.Projection;
import org.cp.elements.data.oql.Oql.Select;
import org.cp.elements.data.oql.Oql.TransformingProjection;
import org.cp.elements.data.oql.Oql.TransformingProjectionBuilder;
import org.cp.elements.data.oql.Oql.Where;
import org.cp.elements.data.oql.provider.SimpleOqlProvider;
import org.cp.elements.lang.Constants;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.security.model.User;
import org.cp.elements.util.CollectionUtils;

/**
 * Unit Tests for {@link Oql}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.Oql
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 2.0.0
 */
@SuppressWarnings({ "rawtypes", "unchecked", "unused" })
public class OqlUnitTests {

  private <S, T> BiFunction<QueryContext<S, T>, QueryResult<T>, T> mockBiFunction() {
    return mock(BiFunction.class);
  }

  private <S, T> Function<S, T> mockFunction() {
    return mock(Function.class);
  }

  private <S, T> QueryFunction<S, T> mockQueryFunction() {
    return mock(QueryFunction.class);
  }

  @Test
  void defaultProviderIsSimpleOqlProvider() {
    assertThat(Oql.defaultProvider()).isInstanceOf(SimpleOqlProvider.class);
  }

  @Test
  void fromConfiguresFromClause() {

    Oql mockOql = mock(Oql.class);

    Select<User<Integer>, User<Integer>> mockSelect = mock(Select.class);
    From<User<Integer>, User<Integer>> mockFrom = mock(From.class);

    doReturn(mockSelect).when(mockOql).select(any(Projection.class));
    doReturn(mockFrom).when(mockSelect).from(any(Iterable.class));
    doCallRealMethod().when(mockOql).from(any(Iterable.class));

    Iterable<User<Integer>> users = Collections.singleton(User.<Integer>named("jonDoe").identifiedBy(1));

    From<User<Integer>, User<Integer>> from = mockOql.from(users);

    assertThat(from).isSameAs(mockFrom);

    verify(mockOql, times(1)).from(eq(users));
    verify(mockOql, times(1)).select(isA(Projection.class));
    verify(mockSelect, times(1)).from(eq(users));
    verifyNoMoreInteractions(mockOql, mockSelect);
    verifyNoInteractions(mockFrom);
  }

  @Test
  void fromNullIterable() {

    Oql mockOql = mock(Oql.class);

    doCallRealMethod().when(mockOql).from(any());

    assertThatIllegalArgumentException()
      .isThrownBy(() -> mockOql.from(null))
      .withMessage("Iterable collection to query is required")
      .withNoCause();
  }

  @Test
  void projectAsTypeReturnsType() {

    Oql.Projection<Object, ?> projection = Oql.Projection.as(User.class)
      .mappedWith(mockFunction())
      .build();

    assertThat(projection).isNotNull();
    assertThat(projection.getType()).isEqualTo(User.class);
    assertThat(projection.getFromType()).isEqualTo(Object.class);
  }

  @Test
  void projectAsNullTypeThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Oql.Projection.as(null))
      .withMessage("Type is required")
      .withNoCause();
  }

  @Test
  void projectionFromType() {

    Oql.Projection<String, User> projection = Oql.Projection.<String, User>as(User.class)
      .fromType(String.class)
      .mappedWith(mockFunction())
      .build();

    assertThat(projection).isNotNull();
    assertThat(projection.getType()).isEqualTo(User.class);
    assertThat(projection.getFromType()).isEqualTo(String.class);
  }

  @Test
  void projectionFromNullType() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Projection.as(User.class).fromType(null))
      .withMessage("From type is required")
      .withNoCause();
  }

  @Test
  void projectionMappedWithBiFunction() {

    Function<String, String> function = StringUtils::reverse;

    Oql.Projection<String, String> projection = Oql.Projection.<String, String>as(String.class)
      .fromType(String.class)
      .mappedWith(function)
      .build();

    QueryContext<String, String> mockQueryContext = mock(QueryContext.class);

    assertThat(projection).isNotNull();
    assertThat(projection.getFromType()).isEqualTo(String.class);
    assertThat(projection.getType()).isEqualTo(String.class);
    assertThat(projection.map(mockQueryContext, "DOG")).isEqualTo("GOD");

    verifyNoInteractions(mockQueryContext);
  }

  @Test
  void projectionMappedWithNullBiFunction() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Oql.Projection.as(Object.class).mappedWith((BiFunction) null))
      .withMessage("Object mapping function is required")
      .withNoCause();
  }

  @Test
  void projectionMappedWithFunction() {

    Oql.ProjectionBuilder<Object, User> projectionBuilder = spy(new Oql.ProjectionBuilder<>(User.class));

    Function<Object, User> mockFunction = mockFunction();

    assertThat(projectionBuilder.mappedWith(mockFunction)).isInstanceOf(TransformingProjectionBuilder.class);

    verify(projectionBuilder, times(1)).mappedWith(isA(Function.class));
    verify(projectionBuilder, times(1)).mappedWith(isA(BiFunction.class));
    verifyNoMoreInteractions(projectionBuilder);
    verifyNoInteractions(mockFunction);
  }

  @Test
  void projectionMappedWithNullFunction() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Oql.Projection.as(Object.class).mappedWith((Function) null))
      .withMessage("Object mapping function is required")
      .withNoCause();
  }

  @Test
  void projectionRemappedWithBiFunction() {

    Oql.Projection<String, User> projection = Oql.Projection.<String, User>as(User.class)
      .fromType(String.class)
      .mappedWith(User::named)
      .apply(encodedUsername(User::getName))
      .remappedWith(queryResult -> User.named(queryResult.get("username")));

    assertThat(projection).isInstanceOf(TransformingProjection.class);

    Oql.TransformingProjection<String, User> transformingProjection =
      (TransformingProjection<String, User>) projection;

    QueryContext<String, User> mockQueryContext = mock(QueryContext.class);

    User jonDoe = transformingProjection.map(mockQueryContext, "jonDoe");

    assertThat(jonDoe).isNotNull();
    assertThat(jonDoe.getName()).isEqualTo("jonDoe");

    String encodedUsername = transformingProjection.stream().findFirst()
      .map(transformer -> transformer.apply(jonDoe))
      .map(String::valueOf)
      .orElse(StringUtils.EMPTY_STRING);

    assertThat(encodedUsername).isNotBlank();
    assertThat(encodedUsername).isEqualTo("<JONDOE/>");

    QueryResult<User> queryResult = QueryResult.<User>builder()
      .withMap(Map.of("username", encodedUsername))
      .build();

    User user = transformingProjection.remap(mockQueryContext, queryResult);

    assertThat(user).isNotNull();
    assertThat(user).isNotSameAs(jonDoe);
    assertThat(user.getName()).isEqualTo(encodedUsername);

    verifyNoInteractions(mockQueryContext);
  }

  private QueryFunction<User, Object> encodedUsername(Function<User, Object> userFunction) {

    return new QueryFunction<>() {

      @Override
      public Object apply(Iterable<User> users) {
        return "<%s/>".formatted(userFunction.apply(users.iterator().next())).toUpperCase();
      }

      @Override
      public String getName() {
        return "username";
      }
    };
  }

  @Test
  void projectionRemappedWithNoTransformations() {

    assertThatIllegalStateException()
      .isThrownBy(() -> Oql.Projection.as(User.class).mappedWith(mockFunction()).remappedWith(mockFunction()))
      .withMessage("No transformations defined")
      .withNoCause();
  }

  @Test
  void projectionRemappedWithNullBiFunction() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Oql.Projection.<String, User>as(User.class)
        .fromType(String.class)
        .mappedWith(mockFunction())
        .apply(encodedUsername(User::getName))
        .remappedWith((BiFunction<QueryContext<String, User>, QueryResult<User>, User>) null))
      .withMessage("Object remapping function is required")
      .withNoCause();
  }

  @Test
  void projectionRemappedWithNullFunction() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Oql.Projection.<String, User>as(User.class)
        .fromType(String.class)
        .mappedWith(mockFunction())
        .apply(encodedUsername(User::getName))
        .remappedWith((Function<QueryResult<User>, User>) null))
      .withMessage("Object remapping function is required")
      .withNoCause();
  }

  @Test
  void projectionRemappedWithTransformationsThenBuilt() {

    assertThatIllegalStateException()
      .isThrownBy(() -> Oql.Projection.as(User.class)
        .mappedWith(mockFunction())
        .apply(encodedUsername(User::getName))
        .build())
      .withMessage("Using transformations requires remapping; you must call remappedWith(..)")
      .withNoCause();
  }

  @Test
  void projectionStarReturnsProjectionMappingTargetingSelf() {

    Oql.Projection<Object, Object> projection = Oql.Projection.star();

    QueryContext<Object, Object> mockQueryContext = mock(QueryContext.class);

    assertThat(projection).isNotNull();
    assertThat(projection.map(mockQueryContext, "TEST")).isEqualTo("TEST");

    verifyNoInteractions(mockQueryContext);
  }

  @Test
  void selectDistinctIsUnsupportedByDefault() {

    Oql.Select<?, ?> select = mock(Oql.Select.class);

    doCallRealMethod().when(select).distinct();

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> select.distinct().from(CollectionUtils.emptyIterable()))
      .withMessage(Constants.UNSUPPORTED_OPERATION)
      .withNoCause();

    verify(select, times(1)).distinct();
    verify(select, never()).from(any());
    verifyNoMoreInteractions(select);
  }

  @Test
  void fromTypeReturnsSelectionProjectionFromType() {

    Oql.Projection<?, ?> projection = mock(Oql.Projection.class);
    Oql.Select<?, ?> select = mock(Oql.Select.class);
    Oql.From<? ,?> from = mock(Oql.From.class);

    doReturn(User.class).when(projection).getFromType();
    doReturn(projection).when(select).getProjection();
    doReturn(select).when(from).getSelection();
    doCallRealMethod().when(from).getType();

    assertThat(from.getType()).isEqualTo(User.class);

    verify(from, times(1)).getType();
    verify(from, times(1)).getSelection();
    verify(select, times(1)).getProjection();
    verify(projection, times(1)).getFromType();
    verifyNoMoreInteractions(projection, select, from);
  }

  @Test
  void fromClauseHasNoWhere() {

    Oql.From<Object, Object> from = mock(Oql.From.class);

    doCallRealMethod().when(from).getWhere();

    Optional<Oql.Where<Object, Object>> where = from.getWhere();

    assertThat(where).isNotNull();
    assertThat(where).isNotPresent();
  }

  @Test
  void whereConditionAnd() {

    BiPredicate<QueryArguments, Object> mockPredicateOne = mock(BiPredicate.class);
    BiPredicate<QueryArguments, Object> mockPredicateTwo = mock(BiPredicate.class);
    BiPredicate<QueryArguments, Object> mockPredicateThree = mock(BiPredicate.class);

    Oql.Where<Object, Object> where = mock(Oql.Where.class);

    doReturn(mockPredicateOne).when(where).getPredicate();
    doReturn(mockPredicateThree).when(mockPredicateOne).and(any(BiPredicate.class));
    doCallRealMethod().when(where).and(any(BiPredicate.class));

    Oql.Where<?, ?> newWhere = where.and(mockPredicateTwo);

    assertThat(newWhere).isNotNull();
    assertThat(newWhere).isNotSameAs(where);
    assertThat(newWhere.getPredicate()).isEqualTo(mockPredicateThree);

    verify(mockPredicateOne, times(1)).and(eq(mockPredicateTwo));
    verifyNoInteractions(mockPredicateTwo, mockPredicateThree);
    verifyNoMoreInteractions(mockPredicateOne);
  }

  @Test
  void whereConditionOr() {

    BiPredicate<QueryArguments, Object> mockPredicateOne = mock(BiPredicate.class);
    BiPredicate<QueryArguments, Object> mockPredicateTwo = mock(BiPredicate.class);
    BiPredicate<QueryArguments, Object> mockPredicateThree = mock(BiPredicate.class);

    Oql.Where<Object, Object> where = mock(Oql.Where.class);

    doReturn(mockPredicateOne).when(where).getPredicate();
    doReturn(mockPredicateThree).when(mockPredicateOne).or(any(BiPredicate.class));
    doCallRealMethod().when(where).or(any(BiPredicate.class));

    Oql.Where<?, ?> newWhere = where.or(mockPredicateTwo);

    assertThat(newWhere).isNotNull();
    assertThat(newWhere).isNotSameAs(where);
    assertThat(newWhere.getPredicate()).isEqualTo(mockPredicateThree);

    verify(mockPredicateOne, times(1)).or(eq(mockPredicateTwo));
    verifyNoInteractions(mockPredicateTwo, mockPredicateThree);
    verifyNoMoreInteractions(mockPredicateOne);
  }

  @Test
  void whereComposeWithNullWhere() {

    BiPredicate<QueryArguments, ?> mockPredicate = mock(BiPredicate.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Where.compose(null, mockPredicate))
      .withMessage("Where clause is required")
      .withNoCause();

    verifyNoInteractions(mockPredicate);
  }

  @Test
  void whereComposeWithNullPredicate() {

    Where<?, ?> mockWhere = mock(Where.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Where.compose(mockWhere, null))
      .withMessage("Predicate is required")
      .withNoCause();

    verifyNoInteractions(mockWhere);
  }
}
