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
package org.cp.elements.security.model;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link Users}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.security.model.Users
 * @since 2.0.0
 */
public class UsersUnitTests {

  @Test
  void emptyUsers() {

    Users empty = Users.empty();

    assertThat(empty).isNotNull();
    assertThat(empty).isEmpty();
  }

  @Test
  void arrayOfUsers() {

    Users users = Users.of(User.named("A"), User.named("B"));

    assertThat(users).isNotNull();
    assertThat(users).hasSize(2);
    assertThat(users.stream().map(User::getName).toList()).containsExactly("A", "B");
  }

  @Test
  void arrayOfSingleUser() {

    Users users = Users.of(User.named("A"));

    assertThat(users).isNotNull();
    assertThat(users).hasSize(1);
    assertThat(users.stream().findFirst().map(User::getName).orElse(null)).isEqualTo("A");
  }

  @Test
  void arrayOfNoUsers() {

    Users users = Users.of();

    assertThat(users).isNotNull();
    assertThat(users).isEmpty();
  }

  @Test
  void listOfUsers() {

    Users users = Users.of(List.of(User.named("A"), User.named("B")));

    assertThat(users).isNotNull();
    assertThat(users).hasSize(2);
    assertThat(users.stream().map(User::getName).toList()).containsExactly("A", "B");
  }

  @Test
  void setOfUsers() {

    Users users = Users.of(Set.of(User.named("A"), User.named("B"), User.named("C")));

    assertThat(users).isNotNull();
    assertThat(users).hasSize(3);
    assertThat(users.stream().map(User::getName).toList()).containsExactlyInAnyOrder("A", "B", "C");
  }

  @Test
  void setOfSingleUser() {

    Users users = Users.of(Set.of(User.named("A")));

    assertThat(users).isNotNull();
    assertThat(users).hasSize(1);
    assertThat(users.stream().map(User::getName).toList()).containsExactly("A");
  }

  @Test
  void setOfNoUsers() {

    Users users = Users.of(Collections.emptySet());

    assertThat(users).isNotNull();
    assertThat(users).isEmpty();
  }

  @Test
  void ofNullArray() {

    Users users = Users.of((User<?>[]) null);

    assertThat(users).isNotNull();
    assertThat(users).isEmpty();
  }

  @Test
  void ofNullIterable() {

    Users users = Users.of((Iterable<User<?>>) null);

    assertThat(users).isNotNull();
    assertThat(users).isEmpty();
  }

  @Test
  void findByPredicateReturnsMatchingUsers() {

    User<?> jonDoe = User.named("jonDoe");
    User<?> janeDoe = User.named("janeDoe");
    User<?> johnBlum = User.named("johnBlum");

    Users users = Users.of(jonDoe, janeDoe, johnBlum);

    assertThat(users).isNotNull().hasSize(3);

    Users matchingUsers = users.findBy(user -> user.getName().toLowerCase().endsWith("doe"));

    assertThat(matchingUsers).isNotNull().isNotSameAs(users).hasSize(2);
    assertThat(matchingUsers).containsExactlyInAnyOrder(jonDoe, janeDoe);
  }

  @Test
  void findByPredicateReturnsNoUsers() {

    User<?> jonDoe = User.named("jonDoe");
    User<?> johnBlum = User.named("johnBlum");

    Users users = Users.of(jonDoe, johnBlum);

    assertThat(users).isNotNull().hasSize(2);

    Users noUsers = users.findBy(user -> false);

    assertThat(noUsers).isNotNull().isNotSameAs(users).isEmpty();
  }

  @Test
  void findByNullPredicate() {

    User<?> jonDoe = User.named("jonDoe");

    Users users = Users.of(jonDoe);

    assertThat(users).isNotNull().hasSize(1);

    Users matchingUsers = users.findBy(null);

    assertThat(matchingUsers).isNotNull().isNotSameAs(users).isEmpty();
  }

  @Test
  void findByName() {

    User<?> jonDoe = User.named("jonDoe");
    User<?> johnBlum = User.named("johnBlum");

    Users users = Users.of(jonDoe, johnBlum);

    assertThat(users).isNotNull().hasSize(2);

    Optional<User<?>> matchingUser = users.findByName("johnBlum");

    assertThat(matchingUser).isNotNull().isPresent();
    assertThat(matchingUser.orElse(null)).isEqualTo(johnBlum);
  }

  @Test
  void findByNonMatchingName() {

    User<?> johnBlum = User.named("johnBlum");

    Users users = Users.of(johnBlum);

    assertThat(users).isNotNull().hasSize(1);

    Optional<User<?>> matchingUser = users.findByName("jonBloom");

    assertThat(matchingUser).isNotNull().isNotPresent();
  }

  @Test
  void findOneMatchesMultipleUsers() {

    User<?> cookieDoe = User.named("cookieDoe");
    User<?> pieDoe = User.named("pieDoe");
    User<?> sourDoe = User.named("sourDoe");

    Users users = Users.of(pieDoe, sourDoe, cookieDoe).sort(Comparator.comparing(User::getName));

    assertThat(users).isNotNull().hasSize(3);

    Optional<User<?>> matchingUser = users.findOne(user -> user.getName().toLowerCase().endsWith("doe"));

    assertThat(matchingUser).isNotNull().isPresent();
    assertThat(matchingUser.orElse(null)).isEqualTo(cookieDoe);
  }

  @Test
  void findOneMatchesNone() {

    User<?> cookieDoe = User.named("cookieDoe");
    User<?> pieDoe = User.named("pieDoe");
    User<?> sourDoe = User.named("sourDoe");

    Users users = Users.of(cookieDoe, pieDoe, sourDoe);

    assertThat(users).isNotNull().hasSize(3);

    Optional<User<?>> matchingUser = users.findOne(user -> user.getName().toLowerCase().startsWith("dill"));

    assertThat(matchingUser).isNotNull().isNotPresent();
  }

  @Test
  void requiresOneReturnsOne() {

    User<?> lanDoe = User.named("lanDoe");

    Users users = Users.of(lanDoe);

    assertThat(users).isNotNull().hasSize(1);
    assertThat(users.requireOne(user -> user.equals(lanDoe))).isEqualTo(lanDoe);
  }

  @Test
  void requiresOneThrowsUserNotFoundException() {

    Predicate<User<?>> nonMatchingUserPredicate = user -> false;

    assertThatExceptionOfType(UserNotFoundException.class)
      .isThrownBy(() -> Users.of(User.named("froDoe")).requireOne(nonMatchingUserPredicate))
      .withMessage("No User was found matching Predicate [%s]", nonMatchingUserPredicate)
      .withNoCause();

  }

  @Test
  void sizeNone() {
    assertThat(Users.empty().size()).isZero();
  }

  @Test
  void sizeOne() {
    assertThat(Users.of(User.named("A")).size()).isOne();
  }

  @Test
  void sizeTwo() {
    assertThat(Users.of(User.named("A"), User.named("B")).size()).isEqualTo(2);
  }

  @Test
  void sortUnorderedList() {

    Users users = Users.of(List.of(User.named("C"), User.named("A"), User.named("B")));

    assertThat(users).isNotNull();
    assertThat(users).hasSize(3);
    assertThat(users.stream().map(User::getName).toList()).containsExactly("C", "A", "B");

    Users sortedUsers = users.sort(Comparator.comparing(User::getName));

    assertThat(sortedUsers).isNotNull().isNotSameAs(users);
    assertThat(sortedUsers).hasSameSizeAs(users);
    assertThat(sortedUsers.stream().map(User::getName).toList()).containsExactly("A", "B", "C");
  }

  @Test
  void sortSet() {

    User<Integer> A = User.<Integer>named("A").identifiedBy(3);
    User<Integer> B = User.<Integer>named("B").identifiedBy(2);
    User<Integer> C = User.<Integer>named("C").identifiedBy(1);

    Users users = Users.of(Set.of(A, B, C));

    assertThat(users).isNotNull();
    assertThat(users).hasSize(3);
    assertThat(users).containsExactlyInAnyOrder(A, B, C);

    Users sortedUsers = users.sort(Comparator.comparing(User::getId));

    assertThat(sortedUsers).isNotNull().isNotSameAs(users);
    assertThat(sortedUsers).hasSameSizeAs(users);
    assertThat(sortedUsers).containsExactly(C, B, A);
  }

  @Test
  void streamOfUsers() {

    Stream<User<?>> userStream = Users.of(User.named("A"), User.named("B")).stream();

    assertThat(userStream).isNotNull();
    assertThat(userStream.map(User::getName).toList()).containsExactly("A", "B");
  }
}
