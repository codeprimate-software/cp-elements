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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyNoInteractions;

import java.util.Comparator;
import java.util.Set;
import java.util.function.Function;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Nameable;
import org.cp.elements.util.stream.StreamUtils;

import lombok.AccessLevel;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

/**
 * Integration Tests for {@link Oql}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.Oql
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 2.0.0
 */
public class OqlIntegrationTests {

  private static final Set<Person> PEOPLE = Set.of(
    Person.named("Jon", "Doe").withAge(42).asMale(),
    Person.named("Jane", "Doe").withAge(48).asFemale(),
    Person.named("Bob", "Doe").withAge(24).asMale(),
    Person.named("Cookie", "Doe").withAge(8).asFemale(),
    Person.named("Dill", "Doe").withAge(51).asMale(),
    Person.named("Fro", "Doe").withAge(21).asMale(),
    Person.named("Hoe", "Doe").withAge(33).asFemale(),
    Person.named("Joe", "Doe").withAge(12).asMale(),
    Person.named("Lan", "Doe").withAge(47).asMale(),
    Person.named("Moe", "Doe").withAge(19).asMale(),
    Person.named("Pie", "Doe").withAge(16).asFemale(),
    Person.named("Sour", "Doe").withAge(13).asFemale()
  );

  private static final Person[] PEOPLE_ARRAY = PEOPLE.toArray(new Person[0]);

  @SuppressWarnings("unchecked")
  private <S, T> Function<S, T> mockFunction() {
    return mock(Function.class);
  }

  @SuppressWarnings("unchecked")
  private <S, T> QueryContext<S, T> mockQueryContext() {
    return mock(QueryContext.class);
  }

  private String[] toStrings(Iterable<?> iterable) {

    return StreamUtils.stream(iterable)
      .map(Object::toString)
      .toArray(String[]::new);
  }

  @Test
  void projection() {

    Oql.Projection<Person, String> projection = Oql.Projection.<Person, String>as(String.class)
      .fromType(Person.class)
      .mappedWith(Person::getName)
      .build();

    QueryContext<Person, String> mockQueryContext = mockQueryContext();

    assertThat(projection).isNotNull();
    assertThat(projection.getFromType()).isEqualTo(Person.class);
    assertThat(projection.getType()).isEqualTo(String.class);
    assertThat(projection.map(mockQueryContext, Person.named("Jon", "Doe"))).isEqualTo("Jon Doe");

    verifyNoInteractions(mockQueryContext);
  }

  @Test
  void projectionWithSelection() {

    Oql.Projection<Person, String> projection = Oql.Projection.<Person, String>as(String.class)
      .mappedWith(mockFunction())
      .build();

    Query<Person, String> query = Oql.defaultProvider()
      .select(projection)
      .from(PEOPLE)
      .asQuery();

    assertThat(query).isNotNull();

    Oql.Projection<Person, String> queryProjection = query.projection();

    assertThat(queryProjection).isNotNull();
    assertThat(queryProjection.getType()).isEqualTo(String.class);
    assertThat(queryProjection.getFromType()).isEqualTo(Person.class);
  }

  @Test
  void projectionWithoutSelection() {

    Oql.Projection<Person, Person> projection = Oql.Projection.star();

    assertThat(projection).isNotNull();
    assertThat(projection.getType()).isEqualTo(Object.class);
    assertThat(projection.getFromType()).isEqualTo(Object.class);
  }

  @Test
  void queryAll() {

    Set<Person> people = Set.of(
      Person.named("Jane", "Doe"),
      Person.named("Jon", "Doe"),
      Person.named("Pie", "Doe")
    );

    Iterable<Person> result = Oql.defaultProvider()
      .select(Oql.Projection.<Person>star())
      .from(people)
      .execute();

    assertThat(result).isNotNull();
    assertThat(result).isNotSameAs(people);
    assertThat(result).containsAll(people);
  }

  @Test
  void queryLimit() {

    Iterable<Person> result = Oql.defaultProvider()
      .select(Oql.Projection.<Person>star())
      .from(PEOPLE)
      .limit(1)
      .execute();

    assertThat(result).isNotNull();
    assertThat(result).hasSize(1);
    assertThat(PEOPLE).containsAll(result);
  }

  @Test
  void queryProjection() {

    Set<Person> people = Set.of(
      Person.named("Pie", "Doe"),
      Person.named("Jon", "Doe"),
      Person.named("Jane", "Doe")
    );

    Oql.Projection<Person, String> projection = Oql.Projection.<Person, String>as(String.class)
      .mappedWith(Person::getName)
      .build();

    Iterable<String> result = Oql.defaultProvider()
      .select(projection)
      .from(people)
      .execute();

    assertThat(result).isNotNull();
    assertThat(result).containsExactlyInAnyOrder("Jane Doe", "Jon Doe", "Pie Doe");
  }

  @Test
  void queryProjectionWithOrdering() {

    Set<Person> people = Set.of(
      Person.named("Jane", "Doe").withAge(48),
      Person.named("Pie", "Doe").withAge(16),
      Person.named("Jon", "Doe").withAge(42)
    );

    Oql.Projection<Person, NameAgeView> projection = Oql.Projection.<Person, NameAgeView>as(NameAgeView.class)
      .mappedWith(NameAgeView::from)
      .build();

    Iterable<NameAgeView> results = Oql.defaultProvider()
      .select(projection)
      .from(people)
      .orderBy(NameAgeView::getAge)
      .execute();

    assertThat(results).isNotNull();
    assertThat(toStrings(results)).containsExactly("Pie Doe", "Jon Doe", "Jane Doe");
  }

  @Test
  void queryProjectionWithComplexOrdering() {

    Set<Person> people = Set.of(
      Person.named("Bill", "Hill").withAge(9),
      Person.named("Jack", "Hill").withAge(8),
      Person.named("Jill", "Hill").withAge(7),
      Person.named("Jack", "Handy").withAge(8),
      Person.named("Mandy", "Handy").withAge(7),
      Person.named("Randy", "Handy").withAge(8)
    );

    Oql.Projection<Person, FirstNameLastNameAgeView> projection =
      Oql.Projection.<Person, FirstNameLastNameAgeView>as(FirstNameLastNameAgeView.class)
        .mappedWith(FirstNameLastNameAgeView::from)
        .build();

    Iterable<FirstNameLastNameAgeView> results = Oql.defaultProvider()
      .select(projection)
      .from(people)
      .orderBy(FirstNameLastNameAgeView::getAge)
      .thenOrderBy(FirstNameLastNameAgeView::getFirstName).descending()
      .thenOrderBy(FirstNameLastNameAgeView::getLastName).ascending()
      .execute();

    assertThat(results).isNotNull();
    assertThat(toStrings(results))
      .containsExactly("Mandy Handy", "Jill Hill", "Randy Handy", "Jack Handy", "Jack Hill", "Bill Hill");
  }

  @Test
  void queryProjectionWithOrderingAndLimit() {

    Oql.Projection<Person, NameAgeView> projection = Oql.Projection.<Person, NameAgeView>as(NameAgeView.class)
      .mappedWith(NameAgeView::from)
      .build();

    Iterable<NameAgeView> result = Oql.defaultProvider()
      .select(projection)
      .from(PEOPLE)
      .orderBy(NameAgeView::getAge).descending()
      .limit(4)
      .execute();

    assertThat(result).isNotNull();
    assertThat(toStrings(result)).containsExactly("Dill Doe", "Jane Doe", "Lan Doe", "Jon Doe");
  }

  @Test
  void queryProjectionWithOrderingAndFilter() {

    Set<Person> people = Set.of(
      Person.named("Pie", "Doe").withAge(16),
      Person.named("Jon", "Doe").withAge(42),
      Person.named("Jane", "Doe").withAge(48),
      Person.named("Jack", "Handy").withAge(51),
      Person.named("Sandy", "Handy").withAge(47)
    );

    Oql.Projection<Person, NameAgeView> projection = Oql.Projection.<Person, NameAgeView>as(NameAgeView.class)
      .mappedWith(NameAgeView::from)
      .build();

    Iterable<NameAgeView> result = Oql.defaultProvider()
      .select(projection)
      .from(people)
      .where(person -> "doe".equalsIgnoreCase(person.getLastName()))
      .and(person -> person.getAge() > 40)
      .orderBy(NameAgeView::getAge).descending()
      .execute();

    assertThat(result).isNotNull();
    assertThat(toStrings(result)).containsExactly("Jane Doe", "Jon Doe");
  }

  @Test
  void queryProjectionWithFilteringAndLimit() {

    Iterable<Person> sortedPeople = PEOPLE.stream()
      .sorted(Comparator.comparing(Person::getAge))
      .toList();

    Oql.Projection<Person, String> projection = Oql.Projection.<Person, String>as(String.class)
      .mappedWith(Person::getName)
      .build();

    Iterable<String> result = Oql.defaultProvider()
      .select(projection)
      .from(sortedPeople)
      .where(Person::isFemale)
      .and(person -> person.getAge() < 18)
      .limit(2)
      .execute();

    assertThat(result).isNotNull();
    assertThat(result).containsExactly("Cookie Doe", "Sour Doe");
  }

  @Test
  void queryProjectionWithFilterOrConditionAndOrdering() {

    Oql.Projection<Person, NameAgeView> projection = Oql.Projection.<Person, NameAgeView>as(NameAgeView.class)
      .mappedWith(NameAgeView::from)
      .build();

    Iterable<NameAgeView> result = Oql.defaultProvider()
      .select(projection)
      .from(PEOPLE)
      .where(person -> person.getAge() < 13)
      .or(person -> person.getAge() > 50)
      .orderBy(NameAgeView::getAge)
      .execute();

    assertThat(result).isNotNull();
    assertThat(toStrings(result)).containsExactly("Cookie Doe", "Joe Doe", "Dill Doe");
  }

  @Test
  void countAll() {

    Long count = Oql.defaultProvider()
      .select(Oql.Projection.<Person>star())
      .from(PEOPLE)
      .count();

    assertThat(count).isNotNull();
    assertThat(count).isEqualTo(PEOPLE.size());
  }

  @Test
  void countFemales() {

    Long count = Oql.defaultProvider()
      .select(Oql.Projection.<Person>star())
      .from(PEOPLE)
      .where(Person::isFemale)
      .count();

    assertThat(count).isNotNull();
    assertThat(count).isEqualTo(5L);
  }

  @Test
  void countMatureMales() {

    Long count = Oql.defaultProvider()
      .select(Oql.Projection.<Person>star())
      .from(PEOPLE)
      .where(Person::isMale)
      .and(person -> person.getAge() > 17)
      .count();

    assertThat(count).isNotNull();
    assertThat(count).isEqualTo(6L);
  }

  @Test
  void countOne() {

    Long count = Oql.defaultProvider()
      .select(Oql.Projection.<Person>star())
      .from(PEOPLE)
      .where(person -> person.getName().equalsIgnoreCase("jon doe"))
      .count();

    assertThat(count).isNotNull();
    assertThat(count).isOne();
  }

  @Test
  void countTeenagers() {

    Long count = Oql.defaultProvider()
      .select(Oql.Projection.<Person>star())
      .from(PEOPLE)
      .where(person -> person.getAge() > 12)
      .and(person -> person.getAge() < 17)
      .count();

    assertThat(count).isNotNull();
    assertThat(count).isEqualTo(2L);
  }

  @Test
  void countZero() {

    Long count = Oql.defaultProvider()
      .select(Oql.Projection.<Person>star())
      .from(PEOPLE)
      .where(Person::isNonBinary)
      .count();

    assertThat(count).isNotNull();
    assertThat(count).isZero();
  }

  @Test
  void oqlAsQuery() {

    Query<Person, NameAgeView> query = Oql.defaultProvider()
      .select(Oql.Projection.<Person, NameAgeView>as(NameAgeView.class).mappedWith(NameAgeView::from).build())
      .from(PEOPLE)
      .where(person -> "doe".equalsIgnoreCase(person.getLastName()))
      .orderBy(NameAgeView::getAge).descending()
      .limit(10L)
      .asQuery();

    assertThat(query).isNotNull();
    assertThat(query.selection()).isNotNull();
    assertThat(query.selection().isDistinct()).isFalse();
    assertThat(query.projection()).isNotNull();
    assertThat(query.projection().getType()).isEqualTo(NameAgeView.class);
    assertThat(query.projection().getFromType()).isEqualTo(Person.class);
    assertThat(query.getFrom()).isNotNull();
    assertThat(query.getFrom().getCollection()).containsExactlyInAnyOrder(PEOPLE_ARRAY);
    assertThat(query.predicate()).isPresent();
    assertThat(query.orderBy()).isPresent();
    assertThat(query.limit()).isEqualTo(10L);
    assertThat(query.groupBy()).isNotPresent();
  }

  @Getter
  @ToString
  @EqualsAndHashCode
  @RequiredArgsConstructor(access = AccessLevel.PACKAGE)
  @SuppressWarnings("unused")
  static class Person implements Comparable<Person>, Nameable<String> {

    static final String NAME_FORMAT = "%s %s";

    static Person named(String firstName, String lastName) {
      return new Person(firstName, lastName);
    }

    private final String firstName;
    private final String lastName;

    private Gender gender;

    private int age;

    @Override
    public int compareTo(Person that) {
      return this.getName().compareTo(that.getName());
    }

    @Override
    public String getName() {
      return NAME_FORMAT.formatted(getFirstName(), getLastName());
    }

    boolean isFemale() {
      return Gender.isFemale(getGender());
    }

    boolean isMale() {
      return Gender.isMale(getGender());
    }

    boolean isNonBinary() {
      return getGender() == null;
    }

    Person asFemale() {
      this.gender = Gender.FEMALE;
      return this;
    }

    Person asMale() {
      this.gender = Gender.MALE;
      return this;
    }

    Person asNonBinary() {
      this.gender = null;
      return this;
    }

    Person withAge(int age) {
      Assert.isTrue(age > 0, "Person's age must be greater than 0");
      this.age = age;
      return this;
    }
  }

  interface NameAgeView {

    static NameAgeView from(Person person) {

      return new NameAgeView() {

        @Override
        public String getName() {
          return person.getName();
        }

        @Override
        public int getAge() {
          return person.getAge();
        }

        @Override
        public String toString() {
          return getName();
        }
      };
    }

    String getName();

    int getAge();

  }

  interface FirstNameLastNameAgeView extends NameAgeView {

    static FirstNameLastNameAgeView from(Person person) {

      return new FirstNameLastNameAgeView() {

        @Override
        public String getFirstName() {
          return person.getFirstName();
        }

        @Override
        public String getLastName() {
          return person.getLastName();
        }

        @Override
        public String getName() {
          return person.getName();
        }

        @Override
        public int getAge() {
          return person.getAge();
        }

        @Override
        public String toString() {
          return getName();
        }
      };
    }

    String getFirstName();

    String getLastName();

  }

  enum Gender {

    FEMALE, MALE;

    static boolean isFemale(Gender gender) {
      return FEMALE.equals(gender);
    }

    static boolean isMale(Gender gender) {
      return MALE.equals(gender);
    }
  }
}
