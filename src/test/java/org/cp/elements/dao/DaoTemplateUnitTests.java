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
package org.cp.elements.dao;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;

import org.cp.elements.lang.Identifiable;
import org.cp.elements.util.CollectionUtils;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentMatchers;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;

/**
 * Unit Tests for {@link DaoTemplate}
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.dao.DaoTemplate
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class DaoTemplateUnitTests {

  @Mock
  private DaoTemplate<User> mockDao;

  @Test
  @SuppressWarnings("unchecked")
  public void createWithFunctionCallsCreate() {

    User jonDoe = User.of("Jon Doe").identifiedBy(1L);

    Function<User, User> mockFunction = mock(Function.class);

    doReturn(jonDoe).when(this.mockDao).create();
    doCallRealMethod().when(this.mockDao).create(any(Function.class));
    doReturn(jonDoe).when(mockFunction).apply(isA(User.class));

    assertThat(this.mockDao.create(mockFunction)).isEqualTo(jonDoe);

    verify(this.mockDao).create();
    verify(mockFunction, times(1)).apply(eq(jonDoe));

    verifyNoMoreInteractions(mockFunction);
  }

  @Test(expected = IllegalArgumentException.class)
  public void createWithNullFunctionThrowsIllegalArgumentException() {

    doCallRealMethod().when(this.mockDao).create(any());

    try {
      this.mockDao.create(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Callback Function is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(this.mockDao, never()).create();
    }
  }

  @Test
  public void findAllReturnsEmptyListByDefault() {

    doCallRealMethod().when(this.mockDao).findAll();

    List<User> users = this.mockDao.findAll();

    assertThat(users).isNotNull();
    assertThat(users).isEmpty();

    verify(this.mockDao, times(1)).findAll();

    verifyNoMoreInteractions(this.mockDao);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void findAllWithComparatorCallsFindAllReturnsOrderedListOfBeans() {

    User jonDoe = User.of("Jon Doe").identifiedBy(1L);
    User janeDoe = User.of("Jane Doe").identifiedBy(2L);
    User bobDoe = User.of("Bob Doe").identifiedBy(3L);
    User cookieDoe = User.of("Cookie Doe").identifiedBy(4L);
    User dillDoe = User.of("Dill Doe").identifiedBy(5L);
    User froDoe = User.of("Fro Doe").identifiedBy(6L);
    User hoeDoe = User.of("Hoe Doe").identifiedBy(7L);
    User joeDoe = User.of("Joe Doe").identifiedBy(8L);
    User moeDoe = User.of("Moe Doe").identifiedBy(9L);
    User pieDoe = User.of("Pie Doe").identifiedBy(10L);
    User poeDoe = User.of("Pie Doe").identifiedBy(11L);
    User sourDoe = User.of("Sour Doe").identifiedBy(12L);

    List<User> users = new ArrayList<>(Arrays.asList(
      jonDoe, janeDoe, bobDoe, cookieDoe, dillDoe, froDoe, hoeDoe, joeDoe, moeDoe, pieDoe, poeDoe, sourDoe
    ));

    Comparator<User> orderBy = Comparator.naturalOrder();

    doReturn(users).when(this.mockDao).findAll();
    doCallRealMethod().when(this.mockDao).findAll(isA(Comparator.class));

    List<User> usersFound = this.mockDao.findAll(orderBy);

    assertThat(usersFound).isNotNull();
    assertThat(usersFound).hasSize(users.size());
    assertThat(usersFound).containsExactly(bobDoe, cookieDoe, dillDoe, froDoe, hoeDoe, janeDoe, joeDoe, jonDoe, moeDoe,
      pieDoe, poeDoe, sourDoe);

    verify(this.mockDao, times(1)).findAll(eq(orderBy));
    verify(this.mockDao, times(1)).findAll();

    verifyNoMoreInteractions(this.mockDao);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void findAllWithComparatorIsNullSafe() {

    Comparator<User> mockComparator = mock(Comparator.class);

    doReturn(null).when(this.mockDao).findAll();
    doCallRealMethod().when(this.mockDao).findAll(isA(Comparator.class));

    List<User> users = this.mockDao.findAll(mockComparator);

    assertThat(users).isNotNull();
    assertThat(users).isEmpty();

    verify(this.mockDao, times(1)).findAll(eq(mockComparator));
    verify(this.mockDao, times(1)).findAll();

    verifyNoMoreInteractions(this.mockDao);

    verifyNoInteractions(mockComparator);
  }

  @Test(expected = IllegalArgumentException.class)
  public void findAllWithNullComparatorThrowsIllegalArgumentException() {

    doCallRealMethod().when(this.mockDao).findAll(ArgumentMatchers.<Comparator<User>>any());

    try {
      this.mockDao.findAll((Comparator<User>) null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Comparator used to order (sort) the beans is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(this.mockDao, never()).findAll();
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void findAllWithPredicatedReturnsFilteredListOfBeans() {

    User jonDoe = User.of("Jon Doe").identifiedBy(1L);
    User janeDoe = User.of("Jane Doe").identifiedBy(2L);
    User cookieDoe = User.of("Cookie Doe").identifiedBy(3L);
    User pieDoe = User.of("Pie Doe").identifiedBy(4L);
    User sourDoe = User.of("Sour Doe").identifiedBy(5L);

    Predicate<User> mockPredicate = mock(Predicate.class);

    List<User> users = new ArrayList<>(Arrays.asList(jonDoe, janeDoe, cookieDoe, pieDoe, sourDoe));

    doReturn(users).when(this.mockDao).findAll();
    doCallRealMethod().when(this.mockDao).findAll(any(Predicate.class));

    users.forEach(user -> doReturn(Arrays.asList(cookieDoe, pieDoe, sourDoe).contains(user))
      .when(mockPredicate).test(eq(user)));

    List<User> foundUsers = this.mockDao.findAll(mockPredicate);

    assertThat(foundUsers).isNotNull();
    assertThat(foundUsers).containsExactly(cookieDoe, pieDoe, sourDoe);

    users.forEach(user -> verify(mockPredicate, times(1)).test(eq(user)));

    verify(this.mockDao, times(1)).findAll(eq(mockPredicate));
    verify(this.mockDao, times(1)).findAll();

    verifyNoMoreInteractions(this.mockDao, mockPredicate);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void findAllWithPredicateIsNullSafe() {

    Predicate<User> mockPredicate = mock(Predicate.class);

    doReturn(null).when(this.mockDao).findAll();
    doCallRealMethod().when(this.mockDao).findAll(isA(Predicate.class));

    List<User> users = this.mockDao.findAll(mockPredicate);

    assertThat(users).isNotNull();
    assertThat(users).isEmpty();

    verify(this.mockDao, times(1)).findAll(eq(mockPredicate));
    verify(this.mockDao, times(1)).findAll();

    verifyNoMoreInteractions(this.mockDao);

    verifyNoInteractions(mockPredicate);
  }

  @Test(expected = IllegalArgumentException.class)
  public void findAllWithNullPredicateThrowsIllegalArgumentException() {

    doCallRealMethod().when(this.mockDao).findAll(ArgumentMatchers.<Predicate<User>>any());

    try {
      this.mockDao.findAll((Predicate<User>) null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Query Predicate is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(this.mockDao, never()).findAll();
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void findAllWithPredicateAndComparatorCallsFindAllReturnsOrderedFilteredListOfBeans() {

    User jonDoe = User.of("Jon Doe").identifiedBy(1L);
    User janeDoe = User.of("Jane Doe").identifiedBy(2L);
    User cookieDoe = User.of("Cookie Doe").identifiedBy(3L);
    User pieDoe = User.of("Pie Doe").identifiedBy(4L);
    User sourDoe = User.of("Sour Doe").identifiedBy(5L);

    Predicate<User> mockPredicate = mock(Predicate.class);

    List<User> users = new ArrayList<>(Arrays.asList(jonDoe, janeDoe, cookieDoe, pieDoe, sourDoe));

    Comparator<User> orderBy = Comparator.naturalOrder();

    doReturn(users).when(this.mockDao).findAll(eq(mockPredicate));
    doCallRealMethod().when(this.mockDao).findAll(isA(Predicate.class), isA(Comparator.class));

    assertThat(this.mockDao.findAll(mockPredicate, orderBy))
      .containsExactly(cookieDoe, janeDoe, jonDoe, pieDoe, sourDoe);

    verify(this.mockDao, times(1)).findAll(eq(mockPredicate), eq(orderBy));
    verify(this.mockDao, times(1)).findAll(eq(mockPredicate));

    verifyNoMoreInteractions(this.mockDao);

    verifyNoInteractions(mockPredicate);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void findAllWithPredicateAndComparatorIsNullSafe() {

    Comparator<User> mockComparator = mock(Comparator.class);

    Predicate<User> mockPredicate = mock(Predicate.class);

    doReturn(null).when(this.mockDao).findAll(eq(mockPredicate));
    doCallRealMethod().when(this.mockDao).findAll(isA(Predicate.class), isA(Comparator.class));

    List<User> users = this.mockDao.findAll(mockPredicate, mockComparator);

    assertThat(users).isNotNull();
    assertThat(users).isEmpty();

    verify(this.mockDao, times(1)).findAll(eq(mockPredicate), eq(mockComparator));
    verify(this.mockDao, times(1)).findAll(eq(mockPredicate));

    verifyNoMoreInteractions(this.mockDao);

    verifyNoInteractions(mockPredicate, mockComparator);
  }

  @SuppressWarnings("unchecked")
  @Test(expected = IllegalArgumentException.class)
  public void findAllWithNullPredicateAndComparatorThrowsIllegalArgumentException() {

    Comparator<User> mockComparator = mock(Comparator.class);

    doCallRealMethod().when(this.mockDao).findAll(any(), isA(Comparator.class));

    try {
      this.mockDao.findAll(null, mockComparator);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Query Predicate is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(this.mockDao, never()).findAll(isA(Predicate.class));
      verifyNoInteractions(mockComparator);
    }
  }

  @SuppressWarnings("unchecked")
  @Test(expected = IllegalArgumentException.class)
  public void findAllWithPredicateAndNullComparatorThrowsIllegalArgumentException() {

    Predicate<User> mockPredicate = mock(Predicate.class);

    doCallRealMethod().when(this.mockDao).findAll(isA(Predicate.class), any());

    try {
      this.mockDao.findAll(mockPredicate, null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Comparator used to order (sort) the beans is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(this.mockDao, never()).findAll(eq(mockPredicate));
      verifyNoInteractions(mockPredicate);
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void findByReturnsOptionalEmptyByDefault() {

    Predicate<User> mockPredicate = mock(Predicate.class);

    doCallRealMethod().when(this.mockDao).findBy(isA(Predicate.class));

    Optional<User> user = this.mockDao.findBy(mockPredicate);

    assertThat(user).isNotNull();
    assertThat(user.isPresent()).isFalse();

    verify(this.mockDao, times(1)).findBy(eq(mockPredicate));

    verifyNoMoreInteractions(this.mockDao);

    verifyNoInteractions(mockPredicate);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void removeAllWithPredicateCallsFindAllAndRemoveAll() {

    User jonDoe = User.of("Jon Doe").identifiedBy(1L);
    User janeDoe = User.of("Jane Doe").identifiedBy(2L);
    User cookieDoe = User.of("Cookie Doe").identifiedBy(3L);
    User pieDoe = User.of("Pie Doe").identifiedBy(4L);
    User sourDoe = User.of("Sour Doe").identifiedBy(5L);

    Predicate<User> mockPredicate = mock(Predicate.class);

    List<User> users = Arrays.asList(jonDoe, janeDoe, cookieDoe, pieDoe, sourDoe);

    doReturn(users).when(this.mockDao).findAll(eq(mockPredicate));
    doReturn(true).when(this.mockDao).removeAll(CollectionUtils.asSet(users));
    doCallRealMethod().when(this.mockDao).removeAll(isA(Predicate.class));

    assertThat(this.mockDao.removeAll(mockPredicate)).isTrue();

    verify(this.mockDao, times(1)).removeAll(eq(mockPredicate));
    verify(this.mockDao, times(1)).findAll(eq(mockPredicate));
    verify(this.mockDao, times(1)).removeAll(eq(CollectionUtils.asSet(users)));

    verifyNoMoreInteractions(this.mockDao);

    verifyNoInteractions(mockPredicate);
  }

  @SuppressWarnings("unchecked")
  @Test(expected = IllegalArgumentException.class)
  public void removeAllWithNullPredicateThrowsIllegalArgumentException() {

    doCallRealMethod().when(this.mockDao).removeAll(ArgumentMatchers.<Predicate<User>>any());

    try {
      this.mockDao.removeAll((Predicate<User>) null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Predicate identifying beans to remove is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(this.mockDao, never()).findAll(any(Predicate.class));
      verify(this.mockDao, never()).removeAll(any(Set.class));
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void removeAllWithSetCallsRemove() {

    User jonDoe = User.of("Jon Doe").identifiedBy(1L);
    User janeDoe = User.of("Jane Doe").identifiedBy(2L);
    User cookieDoe = User.of("Cookie Doe").identifiedBy(3L);
    User pieDoe = User.of("Pie Doe").identifiedBy(4L);
    User sourDoe = User.of("Sour Doe").identifiedBy(5L);

    Set<User> users = CollectionUtils.asSet(jonDoe, janeDoe, cookieDoe, pieDoe, sourDoe);

    users.forEach(user -> doReturn(true).when(this.mockDao).remove(eq(user)));

    doCallRealMethod().when(this.mockDao).removeAll(eq(users));

    assertThat(this.mockDao.removeAll(users)).isTrue();

    users.forEach(user -> verify(this.mockDao, times(1)).remove(eq(user)));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void removeAllWithSetCallsRemoveWithOnlyNonNullBeans() {

    User cookieDoe = User.of("Cookie Doe").identifiedBy(1L);
    User pieDoe = User.of("Pie Doe").identifiedBy(2L);
    User sourDoe = User.of("Sour Doe").identifiedBy(3L);

    Set<User> users = CollectionUtils.asSet(cookieDoe, null, pieDoe, null, null, sourDoe, null);

    users.stream()
      .filter(Objects::nonNull)
      .forEach(user -> doReturn(true).when(this.mockDao).remove(eq(user)));

    doCallRealMethod().when(this.mockDao).removeAll(eq(users));

    assertThat(this.mockDao.removeAll(users)).isTrue();

    users.stream()
      .filter(Objects::nonNull)
      .forEach(user -> verify(this.mockDao, times(1)).remove(eq(user)));

    verify(this.mockDao, never()).remove(eq(null));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void removeAllWithSetIsNullSafe() {

    doCallRealMethod().when(this.mockDao).removeAll(ArgumentMatchers.<Set<User>>any());

    assertThat(this.mockDao.removeAll((Set<User>) null)).isFalse();

    verify(this.mockDao, never()).remove(any());
  }

  @Test
  public void removeAllWithSetReturnsFalse() {

    User cookieDoe = User.of("Cookie Doe").identifiedBy(1L);
    User pieDoe = User.of("Pie Doe").identifiedBy(2L);
    User sourDoe = User.of("Sour Doe").identifiedBy(3L);

    Set<User> users = CollectionUtils.asSet(cookieDoe, pieDoe, sourDoe);

    doReturn(false).when(this.mockDao).remove(any());
    doCallRealMethod().when(this.mockDao).removeAll(eq(users));

    assertThat(this.mockDao.removeAll(users)).isFalse();

    users.stream()
      .filter(Objects::nonNull)
      .forEach(user -> verify(this.mockDao, times(1)).remove(eq(user)));
  }

  @Test
  public void removeAllWithSetReturnsTrueIfJustOneBeansIsRemoved() {

    User cookieDoe = User.of("Cookie Doe").identifiedBy(1L);
    User pieDoe = User.of("Pie Doe").identifiedBy(2L);
    User sourDoe = User.of("Sour Doe").identifiedBy(3L);

    Set<User> users = CollectionUtils.asSet(cookieDoe, pieDoe, sourDoe);

    users.stream()
      .filter(Objects::nonNull)
      .forEach(user -> doReturn(Arrays.asList(cookieDoe, pieDoe).contains(user)).when(this.mockDao).remove(eq(user)));

    doCallRealMethod().when(this.mockDao).removeAll(eq(users));

    assertThat(this.mockDao.removeAll(users)).isTrue();

    users.stream()
      .filter(Objects::nonNull)
      .forEach(user -> verify(this.mockDao, times(1)).remove(eq(user)));
  }

  @Test
  public void saveAllCallsSave() {

    User jonDoe = User.of("Jon Doe").identifiedBy(1L);
    User janeDoe = User.of("Jane Doe").identifiedBy(2L);
    User cookieDoe = User.of("Cookie Doe").identifiedBy(3L);
    User pieDoe = User.of("Pie Doe").identifiedBy(4L);
    User sourDoe = User.of("Sour Doe").identifiedBy(5L);

    Set<User> users = CollectionUtils.asSet(jonDoe, janeDoe, cookieDoe, pieDoe, sourDoe);

    users.forEach(user -> doReturn(user).when(this.mockDao).save(eq(user)));

    doCallRealMethod().when(this.mockDao).saveAll(eq(users));

    Set<User> savedUsers = this.mockDao.saveAll(users);

    assertThat(savedUsers).isNotNull();
    assertThat(savedUsers).isEqualTo(users);
    assertThat(savedUsers).isNotSameAs(users);

    users.forEach(user -> verify(this.mockDao, times(1)).save(eq(user)));
  }

  @Test
  public void saveAllSavesOnlyNonNullBeans() {

    User cookieDoe = User.of("Cookie Doe").identifiedBy(1L);
    User pieDoe = User.of("Pie Doe").identifiedBy(2L);
    User sourDoe = User.of("Sour Doe").identifiedBy(3L);

    Set<User> users = CollectionUtils.asSet(cookieDoe, null, pieDoe, null, null, sourDoe, null);

    users.stream()
      .filter(Objects::nonNull)
      .forEach(user -> doReturn(user).when(this.mockDao).save(eq(user)));

    doCallRealMethod().when(this.mockDao).saveAll(eq(users));

    assertThat(this.mockDao.saveAll(users)).containsExactlyInAnyOrder(cookieDoe, pieDoe, sourDoe);

    users.stream()
      .filter(Objects::nonNull)
      .forEach(user -> verify(this.mockDao, times(1)).save(eq(user)));

    verify(this.mockDao, never()).save(eq(null));
  }

  @Test
  public void saveAllIsNullSafe() {

    doCallRealMethod().when(this.mockDao).saveAll(any());

    Set<User> users = this.mockDao.saveAll(null);

    assertThat(users).isNotNull();
    assertThat(users).isEmpty();

    verify(this.mockDao, never()).save(eq(null));
  }

  @Getter
  @ToString(of = "name")
  @EqualsAndHashCode(of = "name")
  @RequiredArgsConstructor(staticName = "of")
  static class User implements Comparable<User>, Identifiable<Long> {

    @Setter
    private Long id;

    @lombok.NonNull
    private final String name;

    @Override
    public int compareTo(User that) {
      return this.getName().compareTo(that.getName());
    }
  }
}
