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
package org.cp.elements.lang.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Comparator;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.Test;

import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.Nameable;
import org.cp.elements.lang.Orderable;
import org.cp.elements.lang.Ordered;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Order;
import org.cp.elements.lang.support.SmartComparator.ComparableComparator;
import org.cp.elements.lang.support.SmartComparator.ComparatorDescriptor;
import org.cp.elements.lang.support.SmartComparator.HashCodeComparator;
import org.cp.elements.util.stream.StreamUtils;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;

/**
 * Unit Tests for {@link SmartComparator}.
 *
 * @author John Blum
 * @see java.util.Comparator
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.support.SmartComparator
 * @since 1.0.0
 */
public class SmartComparatorUnitTests {

  @Test
  public void newSmartComparatorReturnsDistinctNewInstances() {

    SmartComparator comparator = SmartComparator.newSmartComparator();

    assertThat(comparator).isNotNull();
    assertThat(comparator).isNotSameAs(SmartComparator.newSmartComparator());
  }

  @Test
  public void compareWithSingleNullObjectIsNullSafeReturnsNumericValue() {

    Object target = new Object();

    SmartComparator comparator = SmartComparator.newSmartComparator();

    assertThat(comparator.compare(target, null)).isLessThan(0);
    assertThat(comparator.compare(null, target)).isGreaterThan(0);
  }

  @Test
  @SuppressWarnings("all")
  public void compareWithNullObjectsIsNullSafeReturnsDefaultOrder() {
    assertThat(SmartComparator.newSmartComparator().compare(null, null)).isEqualTo(Ordered.DEFAULT);
  }

  @Test
  @SuppressWarnings("all")
  public void compareWithNonOrderDeclaringNonComparableObjects() {

    Object target = new Object();

    SmartComparator comparator = SmartComparator.newSmartComparator();

    assertThat(comparator.compare(target, target)).isZero();
    assertThat(comparator.compare(User.as("JonDoe"), User.as("JaneDoe"))).isZero();
  }

  @Test
  @SuppressWarnings("all")
  public void compareWithNonOrderDeclaringComparableObjects() {

    SmartComparator comparator = SmartComparator.newSmartComparator();

    assertThat(comparator.compare('x', 'x')).isZero();
    assertThat(comparator.compare('a', 'x')).isLessThan(0);
    assertThat(comparator.compare('x', 'a')).isGreaterThan(0);
    assertThat(comparator.compare(1, 1)).isZero();
    assertThat(comparator.compare(1, 2)).isLessThan(0);
    assertThat(comparator.compare(2, 1)).isGreaterThan(0);
    assertThat(comparator.compare(3.14159f, 3.14159f)).isZero();
    assertThat(comparator.compare(3.14159d, Math.PI)).isLessThan(0);
    assertThat(comparator.compare(Math.PI, 3.14159d)).isGreaterThan(0);
    assertThat(comparator.compare("test", "test")).isZero();
    assertThat(comparator.compare("mock", "test")).isLessThan(0);
    assertThat(comparator.compare("test", "mock")).isGreaterThan(0);
  }

  @Test
  public void compareWithStringAndUser() {

    SmartComparator comparator = SmartComparator.newSmartComparator();

    assertThat(comparator.compare(User.as("jonDoe"), "pieDoe")).isLessThan(0);
    assertThat(comparator.compare("sourDoe", User.as("jonDoe"))).isGreaterThan(0);
  }

  @Test
  @SuppressWarnings("all")
  public void compareWithStringAndInteger() {

    SmartComparator comparator = SmartComparator.newSmartComparator();

    assertThat(comparator.compare("2", "2")).isZero();
    assertThat(comparator.compare("1", 2)).isLessThan(0);
    assertThat(comparator.compare(2, "1")).isGreaterThan(0);
  }

  @Test
  @SuppressWarnings("all")
  public void compareWithStringAndCharacter() {

    SmartComparator comparator = SmartComparator.newSmartComparator();

    assertThat(comparator.compare("2", '2')).isZero();
    assertThat(comparator.compare("1", '2')).isLessThan(0);
    assertThat(comparator.compare('2', "1")).isGreaterThan(0);
  }

  @Test
  public void compareWithDoubleAndInteger() {

    SmartComparator comparator = SmartComparator.newSmartComparator();

    assertThat(comparator.compare(2, 3.14159d)).isLessThan(0);
    assertThat(comparator.compare(3.14159d, 2)).isGreaterThan(0);
  }

  // COMPARABLE TESTS

  @Test
  public void compareWithComparableUsers() {
    assertThat(SmartComparator.newSmartComparator()
      .compare(ComparableUser.toComparableUser("JonDoe"), ComparableUser.toComparableUser("BobDoe")))
        .isGreaterThan(0);
  }

  @Test
  public void compareWithComparableUserAndOrderDeclaringUser() {

    ComparableUser comparableUser = ComparableUser.toComparableUser("JonDoe");

    SmartComparator comparator = SmartComparator.newSmartComparator();

    assertThat(comparator.compare(comparableUser, JonDoe.newInstance())).isZero();
    assertThat(comparator.compare(comparableUser, OrderedUser.toOrderedUser("BobDoe"))).isGreaterThan(0);
    assertThat(comparator.compare(comparableUser, OrderableUser.toOrderableUser("PieDoe"))).isLessThan(0);
  }

  // @Order annotated tests

  @Test
  public void compareWithOrderAnnotatedUser() {

    OrderAnnotatedUser janeDoe = OrderAnnotatedUser.toOrderAnnotatedUser("JaneDoe");

    SmartComparator comparator = SmartComparator.newSmartComparator();

    assertThat(comparator.compare(janeDoe, OrderAnnotatedUser.toOrderAnnotatedUser("Jane Doe"))).isZero();
    assertThat(comparator.compare(janeDoe, OrderAnnotatedUser.toOrderAnnotatedUser("BobDoe"))).isZero();
    assertThat(comparator.compare(janeDoe, OrderAnnotatedUser.toOrderAnnotatedUser("JonDoe"))).isZero();
    assertThat(comparator.compare(janeDoe, OrderAnnotatedUser.toOrderAnnotatedUser("PieDoe"))).isZero();
    assertThat(comparator.compare(janeDoe, JonDoe.newInstance())).isGreaterThan(0);
    assertThat(comparator.compare(janeDoe, CookieDoe.newInstance())).isLessThan(0);
    assertThat(comparator.compare(janeDoe, OrderableUser.toOrderableUser("DillDoe"))).isLessThan(0);

    OrderedUser froDoe = OrderedUser.toOrderedUser("FroDoe");

    assertThat(comparator.compare(janeDoe, froDoe)).isLessThan(0);

    froDoe.setIndex(-8);

    assertThat(comparator.compare(janeDoe, froDoe)).isZero();

    froDoe.setIndex(-10);

    assertThat(comparator.compare(janeDoe, froDoe)).isGreaterThan(0);
  }

  // Orderable implementation tests

  @Test
  public void compareWithOrderableUsers() {

    OrderableUser lanDoe = OrderableUser.toOrderableUser("LanDoe");

    SmartComparator comparator = SmartComparator.newSmartComparator();

    assertThat(comparator.compare(lanDoe, OrderableUser.toOrderableUser("lanDoe"))).isZero();
    assertThat(comparator.compare(lanDoe, OrderableUser.toOrderableUser("BobDoe"))).isZero();
    assertThat(comparator.compare(lanDoe, OrderableUser.toOrderableUser("JonDoe"))).isZero();
    assertThat(comparator.compare(lanDoe, OrderableUser.toOrderableUser("PieDoe"))).isZero();
    assertThat(comparator.compare(lanDoe, JonDoe.newInstance())).isGreaterThan(0);

    OrderedUser froDoe = OrderedUser.toOrderedUser("FroDoe");

    assertThat(comparator.compare(lanDoe, froDoe)).isLessThan(0);

    froDoe.setIndex(-4);

    assertThat(comparator.compare(lanDoe, froDoe)).isZero();

    froDoe.setIndex(-10);

    assertThat(comparator.compare(lanDoe, froDoe)).isGreaterThan(0);
  }

  // Ordered implementation tests

  @Test
  public void compareWithOrderedUsers() {

    OrderedUser dillDoe = OrderedUser.toOrderedUser("DillDoe");
    OrderedUser hoeDoe = OrderedUser.toOrderedUser("HoeDoe");

    SmartComparator comparator = SmartComparator.newSmartComparator();

    assertThat(comparator.compare(dillDoe, OrderedUser.toOrderedUser("dill doe"))).isZero();
    assertThat(comparator.compare(dillDoe, hoeDoe)).isZero();
    assertThat(comparator.compare(hoeDoe, dillDoe)).isZero();
    assertThat(comparator.compare(dillDoe, CookieDoe.newInstance())).isGreaterThan(0);
    assertThat(comparator.compare(hoeDoe, OrderableUser.toOrderableUser("FRO DOE"))).isGreaterThan(0);

    dillDoe.setIndex(-3);

    assertThat(comparator.compare(dillDoe, hoeDoe)).isLessThan(0);
    assertThat(comparator.compare(hoeDoe, dillDoe)).isGreaterThan(0);

    hoeDoe.setIndex(-4);

    assertThat(comparator.compare(dillDoe, hoeDoe)).isGreaterThan(0);
    assertThat(comparator.compare(hoeDoe, dillDoe)).isLessThan(0);
  }

  // COMPARATOR TESTS

  @Test
  @SuppressWarnings("unchecked")
  public void compareWithExactNameableComparator() {

    ComparableUser pieDoe = ComparableUser.toComparableUser("PieDoe").identifiedBy(2);

    Nameable<String> sourDoe = mock(Nameable.class);

    doReturn("SourDoe").when(sourDoe).getName();

    SmartComparator comparator = SmartComparator.newSmartComparator()
      .withRegistrationFor(NameableComparator.INSTANCE);

    assertThat(comparator).isNotNull();

    Optional<ComparatorDescriptor> nameableComparatorDescriptor =
      comparator.findByComparator(NameableComparator.INSTANCE);

    assertThat(nameableComparatorDescriptor).isNotNull();
    assertThat(nameableComparatorDescriptor).isPresent();
    assertThat(nameableComparatorDescriptor
      .map(ComparatorDescriptor::getComparator)
      .orElse(null))
      .isSameAs(NameableComparator.INSTANCE);

    // run comparison
    assertThat(comparator.compare(pieDoe, sourDoe)).isLessThan(0);

    verify(sourDoe, times(1)).getName();
    verifyNoMoreInteractions(sourDoe);
  }

  @Test
  public void compareWithExactUserComparator() {

    ComparableUser pieDoe = ComparableUser.toComparableUser("PieDoe").identifiedBy(2);
    ComparableUser sourDoe = ComparableUser.toComparableUser("SourDoe").identifiedBy(1);

    SmartComparator comparator = SmartComparator.newSmartComparator()
      .withRegistrationFor(UserComparator.INSTANCE);

    assertThat(comparator).isNotNull();

    Optional<ComparatorDescriptor> userComparatorDescriptor = comparator.findByComparator(UserComparator.INSTANCE);

    assertThat(userComparatorDescriptor).isNotNull();
    assertThat(userComparatorDescriptor).isPresent();
    assertThat(userComparatorDescriptor
      .map(ComparatorDescriptor::getComparator)
      .orElse(null))
      .isSameAs(UserComparator.INSTANCE);

    // run comparison
    assertThat(comparator.compare(pieDoe, sourDoe)).isGreaterThan(0);
  }

  @Test
  public void compareWithCompatibleComparator() {

    User bobDoe = User.as("BobDoe");
    ComparableUser cookieDoe = ComparableUser.toComparableUser("CookieDoe");
    OrderedUser dillDoe = OrderedUser.toOrderedUser("DillDoe");
    OrderableUser froDoe = OrderableUser.toOrderableUser("FroDoe");
    OrderAnnotatedUser hoeDoe = OrderAnnotatedUser.toOrderAnnotatedUser("HoeDoe");

    SmartComparator comparator = SmartComparator.newSmartComparator()
      .withRegistrationFor(NameableComparator.INSTANCE);

    Optional<ComparatorDescriptor> nameableComparatorDescriptor =
      comparator.findByComparator(NameableComparator.INSTANCE);

    assertThat(nameableComparatorDescriptor).isNotNull();
    assertThat(nameableComparatorDescriptor).isPresent();
    assertThat(nameableComparatorDescriptor
      .map(ComparatorDescriptor::getComparator)
      .orElse(null))
      .isSameAs(NameableComparator.INSTANCE);

    // run comparisons
    assertThat(comparator.compare(bobDoe, cookieDoe)).isLessThan(0);
    assertThat(comparator.compare(dillDoe, cookieDoe)).isGreaterThan(0);
    assertThat(comparator.compare(dillDoe, froDoe)).isLessThan(0);
    assertThat(comparator.compare(froDoe, hoeDoe)).isLessThan(0);
  }

  @Test
  public void findByNonNullNonRegisteredComparator() {

    Optional<ComparatorDescriptor> comparatorDescriptor =
      SmartComparator.newSmartComparator().findByComparator(NameableComparator.INSTANCE);

    assertThat(comparatorDescriptor).isNotNull();
    assertThat(comparatorDescriptor).isNotPresent();
  }

  @Test
  public void findByNullComparatorIsNullSafe() {

    Optional<ComparatorDescriptor> comparatorDescriptor = SmartComparator.newSmartComparator()
      .findByComparator(null);

    assertThat(comparatorDescriptor).isNotNull();
    assertThat(comparatorDescriptor).isNotPresent();
  }

  @Test
  public void findByNullCompatibleTypeIsNullSafe() {

    Optional<Comparator<?>> comparator = SmartComparator.newSmartComparator().findByCompatibleType(null);

    assertThat(comparator).isNotNull();
    assertThat(comparator).isNotPresent();
  }

  @Test
  public void findByNullExactTypeIsNullSafe() {

    Optional<Comparator<?>> comparator = SmartComparator.newSmartComparator().findByExactType(null);

    assertThat(comparator).isNotNull();
    assertThat(comparator).isNotPresent();
  }

  @Test
  public void iteratorIsCorrect() {

    SmartComparator comparator = SmartComparator.newSmartComparator()
      .withRegistrationFor(NameableComparator.INSTANCE)
      .withRegistrationFor(UserComparator.INSTANCE);

    assertThat(comparator).isNotNull();
    assertThat(comparator).hasSize(2);

    Set<Comparator<?>> registeredComparators = StreamUtils.stream(comparator)
      .map(ComparatorDescriptor::getComparator)
      .collect(Collectors.toSet());

    assertThat(registeredComparators).isNotNull();
    assertThat(registeredComparators).hasSize(2);
    assertThat(registeredComparators).containsExactlyInAnyOrder(NameableComparator.INSTANCE, UserComparator.INSTANCE);
  }

  @Test
  public void registerAndUnregisterComparator() {

    SmartComparator comparator = SmartComparator.newSmartComparator()
      .withRegistrationFor(UserComparator.INSTANCE);

    assertThat(comparator).isNotNull();

    Optional<ComparatorDescriptor> userComparatorDescriptor = comparator.findByComparator(UserComparator.INSTANCE);

    assertThat(userComparatorDescriptor).isNotNull();
    assertThat(userComparatorDescriptor).isPresent();
    assertThat(userComparatorDescriptor.map(ComparatorDescriptor::getComparator).orElse(null))
      .isSameAs(UserComparator.INSTANCE);
    assertThat(comparator.unregister(UserComparator.INSTANCE)).isTrue();

    userComparatorDescriptor = comparator.findByComparator(UserComparator.INSTANCE);

    assertThat(userComparatorDescriptor).isNotNull();
    assertThat(userComparatorDescriptor).isNotPresent();
  }

  @Test
  public void unregisterNonRegisteredComparator() {

    SmartComparator comparator = SmartComparator.newSmartComparator();

    Optional<ComparatorDescriptor> userComparatorDescriptor = comparator.findByComparator(UserComparator.INSTANCE);

    assertThat(userComparatorDescriptor).isNotNull();
    assertThat(userComparatorDescriptor).isNotPresent();
    assertThat(comparator.unregister(comparator)).isFalse();
  }

  @Test
  public void newComparatorDescriptor() {

    ComparatorDescriptor comparatorDescriptor = new ComparatorDescriptor(UserComparator.INSTANCE);

    assertThat(comparatorDescriptor).isNotNull();
    assertThat(comparatorDescriptor.getComparator()).isSameAs(UserComparator.INSTANCE);
    assertThat(comparatorDescriptor.getType()).isEqualTo(User.class);
  }

  @Test
  public void fromComparatorDescriptor() {

    ComparatorDescriptor comparatorDescriptor = new ComparatorDescriptor(NameableComparator.INSTANCE);

    assertThat(comparatorDescriptor).isNotNull();
    assertThat(comparatorDescriptor.getComparator()).isSameAs(NameableComparator.INSTANCE);
    assertThat(comparatorDescriptor.getType()).isEqualTo(Nameable.class);
  }

  @Test
  public void fromComparatorDescriptorWithNullComparator() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ComparatorDescriptor.from(null))
      .withMessage("Comparator is required")
      .withNoCause();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void compareWithComparableComparatorIsCorrect() {

    assertThat(ComparableComparator.INSTANCE.compare(2, 1)).isGreaterThan(0);
    assertThat(ComparableComparator.INSTANCE.compare("mock", "test")).isLessThan(0);
    assertThat(ComparableComparator.INSTANCE
      .compare(ComparableUser.toComparableUser("JonDoe"), ComparableUser.toComparableUser("JonDoe"))).isZero();
  }

  @Test
  public void compareWithHashCodeComparatorIsCorrect() {

    assertThat(HashCodeComparator.INSTANCE.compare(2, 1)).isGreaterThan(0);
    assertThat(HashCodeComparator.INSTANCE.compare("mock", "test")).isLessThan(0);
    assertThat(HashCodeComparator.INSTANCE
      .compare(ComparableUser.toComparableUser("JonDoe"), ComparableUser.toComparableUser("JonDoe"))).isZero();
  }

  @Getter
  @ToString(of = "name")
  @EqualsAndHashCode(of = "name")
  @RequiredArgsConstructor(staticName = "as")
  static class User implements Identifiable<Integer>, Nameable<String> {

    @Setter
    private Integer id;

    @lombok.NonNull
    private final String name;

  }

  static class ComparableUser extends User implements Comparable<User> {

    static @NotNull ComparableUser toComparableUser(@NotNull String name) {
      return new ComparableUser(name);
    }

    ComparableUser(@NotNull String name) {
      super(StringUtils.requireText(name, "Name [%s] is required"));
    }

    @Override
    public int compareTo(@NotNull User that) {
      return this.getName().compareTo(that.getName());
    }
  }

  @Order(-7)
  static class CookieDoe extends OrderAnnotatedUser {

    static @NotNull CookieDoe newInstance() {
      return new CookieDoe();
    }

    CookieDoe() {
      super("CookieDoe");
    }
  }

  @Order(-9)
  static class JonDoe extends OrderAnnotatedUser {

    static @NotNull JonDoe newInstance() {
      return new JonDoe();
    }

    JonDoe() {
      super("JonDoe");
    }
  }

  @Order(-8)
  static class OrderAnnotatedUser extends User {

    static @NotNull OrderAnnotatedUser toOrderAnnotatedUser(@NotNull String name) {
      return new OrderAnnotatedUser(name);
    }

    OrderAnnotatedUser(@NotNull String name) {
      super(StringUtils.requireText(name, "Name [%s] is required"));
    }
  }

  static class OrderableUser extends User implements Orderable<Integer>  {

    static @NotNull OrderableUser toOrderableUser(@NotNull String name) {
      return new OrderableUser(name);
    }

    OrderableUser(@NotNull String name) {
      super(StringUtils.requireText(name, "Name [%s] is required"));
    }

    @Override
    public @NotNull Integer getOrder() {
      return -4;
    }
  }

  static class OrderedUser extends User implements Ordered {

    static @NotNull OrderedUser toOrderedUser(@NotNull String name) {
      return new OrderedUser(name);
    }

    private int index = -2;

    OrderedUser(@NotNull String name) {
      super(StringUtils.requireText(name, "Name [%s] is required"));
    }

    @Override
    public int getIndex() {
      return this.index;
    }

    @Override
    public void setIndex(int index) {
      this.index = index;
    }
  }

  static class UserComparator implements Comparator<User> {

    static final UserComparator INSTANCE = new UserComparator();

    @Override
    public int compare(@NotNull User userOne, @NotNull User userTwo) {
      return userOne.getId().compareTo(userTwo.getId());
    }
  }

  static class NameableComparator implements Comparator<Nameable<String>> {

    static final NameableComparator INSTANCE = new NameableComparator();

    @Override
    public int compare(@NotNull Nameable<String> objectOne, @NotNull Nameable<String> objectTwo) {
      return objectOne.getName().compareTo(objectTwo.getName());
    }
  }
}
