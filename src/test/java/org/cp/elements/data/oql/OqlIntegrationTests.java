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
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;

import org.junit.jupiter.api.Test;

import org.cp.elements.data.oql.Oql.Projection;
import org.cp.elements.data.oql.functions.Count;
import org.cp.elements.data.oql.functions.Identity;
import org.cp.elements.data.oql.functions.Max;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Nameable;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.util.stream.StreamUtils;

import lombok.AccessLevel;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

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

  private static final Set<Person> HANDY_FAMILY = Set.of(
    Person.named("Jack", "Handy").withAge(44).asMale(),
    Person.named("Mandy", "Handy").withAge(36).asFemale(),
    Person.named("Andy", "Handy").withAge(16).asMale(),
    Person.named("Sandy", "Handy").withAge(19).asFemale(),
    Person.named("Tandy", "Handy").withAge(21).asMale()
  );

  private static final Set<Person> SMITH_FAMILY = Set.of(
    Person.named("Agent", "Smith").withAge(48).asMale(),
    Person.named("Dan", "Smith").withAge(44).asMale(),
    Person.named("Tool", "Smith").withAge(42).asMale(),
    Person.named("Will", "Smith").withAge(56).asMale()
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
  void fromAll() {

    Iterable<Person> people = Oql.defaultProvider().from(PEOPLE).execute();

    assertThat(people).isNotNull();
    assertThat(people).containsExactlyInAnyOrder(PEOPLE_ARRAY);
  }

  @Test
  void fromSelectPeople() {

    Iterable<Person> people = Oql.defaultProvider().from(PEOPLE)
      .where(Person::isFemale)
      .and(person -> person.getAge() < 18)
      .and(person -> !"Sour".equals(person.getFirstName()))
      .orderBy(Person::getFirstName).descending()
      .execute();

    assertThat(people).isNotNull();
    assertThat(people).hasSize(2);
    assertThat(toStrings(people)).containsExactly("Pie Doe", "Cookie Doe");
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
      .compile();

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
  void queryDistinct() {

    Set<Person> people = Set.of(
      Person.named("Jack", "Black").withAge(42),
      Person.named("Jon", "Doe").withAge(21),
      Person.named("Jack", "Handy").withAge(35)
    );

    Oql.Projection<Person, FirstNameView> projection = Oql.Projection.<Person, FirstNameView>as(FirstNameView.class)
      .mappedWith(FirstNameView::from)
      .build();

    Iterable<FirstNameView> result = Oql.defaultProvider()
      .select(projection)
      .distinct()
      .from(people)
      .where(person -> person.getAge() >= 35)
      .execute();

    assertThat(result).isNotNull();
    assertThat(result).hasSize(1);
    assertThat(toStrings(result)).containsExactly("Jack");
  }

  @Test
  void queryFilterWithArguments() {

    Query<Person, Person> query = Oql.defaultProvider()
      .select(Oql.Projection.<Person>star())
      .from(PEOPLE)
      .where((queryArguments, person) -> person.getAge() > queryArguments.<Integer>requireBy("age").value())
      .compile();

    assertThat(query).isNotNull();

    Iterable<Person> results = query.execute(QueryArgument.from("age", 40));

    assertThat(results).isNotNull();
    assertThat(toStrings(results)).containsExactlyInAnyOrder("Jon Doe", "Jane Doe", "Dill Doe", "Lan Doe");

    results = query.execute(QueryArgument.from("age", 50));

    assertThat(results).isNotNull();
    assertThat(toStrings(results)).containsExactly("Dill Doe");
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
  void queryProjectionWithSimpleOrdering() {

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
  @SuppressWarnings("unchecked")
  void queryGroupByFamilyCountMembers() {

    Oql.Projection<Person, GroupByFamilyView> projection =
      Oql.Projection.<Person, GroupByFamilyView>as(GroupByFamilyView.class)
        .mappedWith(GroupByFamilyView::map)
        .apply(Identity.of(GroupByFamilyView::getLastName).named("LastName"), Count.all())
        .remappedWith(GroupByFamilyView::remap);

    Iterable<GroupByFamilyView> results = Oql.defaultProvider()
      .select(projection)
      .from(PEOPLE)
      .groupBy(GroupByFamilyView::getLastName)
      .execute();

    assertThat(results).isNotNull();
    assertThat(results).hasSize(1);
    assertThat(results).containsExactly(GroupByFamilyView.from("Doe", 12));
  }

  @Test
  @SuppressWarnings("unchecked")
  void queryGroupByFamilyCountByGender() {

    Set<Person> people = new HashSet<>(PEOPLE);

    people.addAll(HANDY_FAMILY);
    people.addAll(SMITH_FAMILY);

    Iterable<GroupByLastNameGenderView> results = Oql.defaultProvider()
      .select(groupByLastNameGenderProjection())
      .from(people)
      .where(Person::isAdult)
      .groupBy(GroupByLastNameGenderView::getLastName, GroupByLastNameGenderView::getGender)
      .orderBy(GroupByLastNameGenderView::getLastName)
      .thenOrderBy(GroupByLastNameGenderView::getGender)
      .execute();

    assertThat(results).isNotNull();
    assertThat(results).hasSize(5);
    assertThat(results).containsExactly(
      GroupByLastNameGenderView.from("Doe", Gender.FEMALE, 48, 2),
      GroupByLastNameGenderView.from("Doe", Gender.MALE, 51, 6),
      GroupByLastNameGenderView.from("Handy", Gender.FEMALE, 36, 2),
      GroupByLastNameGenderView.from("Handy", Gender.MALE, 44, 2),
      GroupByLastNameGenderView.from("Smith", Gender.MALE, 56, 4)
    );
  }

  @Test
  @SuppressWarnings("unchecked")
  void queryGroupByFamilyCountByGenderHavingCondition() {

    Set<Person> people = new HashSet<>(PEOPLE);

    people.addAll(HANDY_FAMILY);
    people.addAll(SMITH_FAMILY);

    Query<Person, GroupByLastNameGenderView> query = Oql.defaultProvider()
      .select(groupByLastNameGenderProjection())
      .from(people)
      .groupBy(GroupByLastNameGenderView::getLastName, GroupByLastNameGenderView::getGender)
      .having((queryArguments, view) -> view.getMaxAge() > queryArguments.<Integer>requireBy("age").value())
      .orderBy(GroupByLastNameGenderView::getLastName)
      .thenOrderBy(GroupByLastNameGenderView::getGender)
      .compile();

    QueryArguments queryArguments = QueryArguments.of(QueryArgument.from("age", 50));

    Iterable<GroupByLastNameGenderView> queryResults = query.execute(queryArguments);

    assertThat(queryResults).isNotNull();
    assertThat(queryResults).hasSize(2);
    assertThat(queryResults).containsExactly(
      GroupByLastNameGenderView.from("Doe", Gender.MALE, 51, 7),
      GroupByLastNameGenderView.from("Smith", Gender.MALE, 56, 4)
    );

    queryArguments = QueryArguments.of(QueryArgument.from("age", 40));
    queryResults = query.execute(queryArguments);

    assertThat(queryResults).isNotNull();
    assertThat(queryResults).hasSize(4);
    assertThat(queryResults).containsExactly(
      GroupByLastNameGenderView.from("Doe", Gender.FEMALE, 48, 5),
      GroupByLastNameGenderView.from("Doe", Gender.MALE, 51, 7),
      GroupByLastNameGenderView.from("Handy", Gender.MALE, 44, 3),
      GroupByLastNameGenderView.from("Smith", Gender.MALE, 56, 4)
    );
  }

  @SuppressWarnings("unchecked")
  private Projection<Person, GroupByLastNameGenderView> groupByLastNameGenderProjection() {

    return Oql.Projection.<Person, GroupByLastNameGenderView>as(GroupByLastNameGenderView.class)
      .mappedWith(GroupByLastNameGenderView::map)
      .apply(Identity.of(GroupByLastNameGenderView::getLastName).named("LastName"),
        Identity.of(GroupByLastNameGenderView::getGender).named("Gender"),
        Max.of(GroupByLastNameGenderView::getMaxAge).named("MaxAge"),
        Count.all())
      .remappedWith(GroupByLastNameGenderView::remap);
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
      .compile();

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
    public String getName() {
      return NAME_FORMAT.formatted(getFirstName(), getLastName());
    }

    boolean isAdult() {
      return getAge() >= 18;
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

    @Override
    public int compareTo(Person that) {
      return this.getName().compareTo(that.getName());
    }

    Person withAge(int age) {
      Assert.isTrue(age > 0, "Person's age must be greater than 0");
      this.age = age;
      return this;
    }

    @Override
    public String toString() {
      return getName();
    }
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

  @FunctionalInterface
  @SuppressWarnings("unused")
  interface FirstNameView {

    static FirstNameView from(Person person) {

      return new FirstNameView() {

        @Override
        public String getFirstName() {
          return person.getFirstName();
        }

        @Override
        public boolean equals(Object obj) {

          if (obj == this) {
            return true;
          }

          if (!(obj instanceof FirstNameView firstNameView)) {
            return false;
          }

          return this.getFirstName().equals(firstNameView.getFirstName());
        }

        @Override
        public int hashCode() {
          return Objects.hash(getFirstName());
        }

        @Override
        public String toString() {
          return getFirstName();
        }
      };
    }

    String getFirstName();

  }

  interface GroupByFamilyView {

    static GroupByFamilyView map(Person person) {
      Assert.notNull(person, "Person is required");
      return person::getLastName;
    }

    static GroupByFamilyView remap(QueryResult<GroupByFamilyView> result) {

      Assert.notNull(result, "QueryResult is required");

      return new AbstractGroupByFamilyView() {

        @Override
        public String getLastName() {
          return result.get("LastName");
        }

        @Override
        public long count() {
          return result.get("Count");
        }
      };
    }

    static GroupByFamilyView from(String lastName, int count) {

      return new AbstractGroupByFamilyView() {

        @Override
        public String getLastName() {
          return lastName;
        }

        @Override
        public long count() {
          return count;
        }
      };
    }

    String getLastName();

    default long count() {
      return 1L;
    }
  }

  static abstract class AbstractGroupByFamilyView implements GroupByFamilyView {

    @Override
    public boolean equals(Object obj) {

      if (obj == this) {
        return true;
      }

      if (!(obj instanceof GroupByFamilyView view)) {
        return false;
      }

      return ObjectUtils.equals(this.getLastName(), view.getLastName());
    }

    @Override
    public int hashCode() {
      return Objects.hash(getLastName());
    }

    @Override
    public String toString() {
      return "{ lastName = %s, count = %d} }".formatted(getLastName(), count());
    }
  }

  interface GroupByLastNameGenderView {

    static GroupByLastNameGenderView map(Person person) {

      Assert.notNull(person, "Person is required");

      return new AbstractGroupByLastNameGenderView() {

        @Override
        public String getLastName() {
          return person.getLastName();
        }

        @Override
        public Gender getGender() {
          return person.getGender();
        }

        @Override
        public int getMaxAge() {
          return person.getAge();
        }
      };
    }

    static GroupByLastNameGenderView remap(QueryResult<GroupByLastNameGenderView> result) {

      Assert.notNull(result, "QueryResult is required");

      return new AbstractGroupByLastNameGenderView() {

        @Override
        public String getLastName() {
          return result.get("LastName");
        }

        @Override
        public Gender getGender() {
          return result.get("Gender");
        }

        @Override
        public int getMaxAge() {
          return result.get("MaxAge");
        }

        @Override
        public long count() {
          return result.get("Count");
        }
      };
    }

    static GroupByLastNameGenderView from(String lastName, Gender gender, int maxAge, int count) {

      return new AbstractGroupByLastNameGenderView() {

        @Override
        public String getLastName() {
          return lastName;
        }

        @Override
        public Gender getGender() {
          return gender;
        }

        @Override
        public int getMaxAge() {
          return maxAge;
        }

        @Override
        public long count() {
          return count;
        }
      };
    }

    String getLastName();

    Gender getGender();

    int getMaxAge();

    default long count() {
      return 1L;
    }
  }

  static abstract class AbstractGroupByLastNameGenderView implements GroupByLastNameGenderView {

    @Override
    public boolean equals(Object obj) {

      if (obj == this) {
        return true;
      }

      if (!(obj instanceof GroupByLastNameGenderView view)) {
        return false;
      }

      return ObjectUtils.equalsIgnoreNull(this.getLastName(), view.getLastName())
        && ObjectUtils.equalsIgnoreNull(this.getGender(), view.getGender())
        && ObjectUtils.equalsIgnoreNull(this.getMaxAge(), view.getMaxAge())
        && ObjectUtils.equalsIgnoreNull(this.count(), view.count());
    }

    @Override
    public int hashCode() {
      return Objects.hash(getLastName(), getGender());
    }

    @Override
    public String toString() {
      return "{ lastName = %s, gender = %s, maxAge = %d, count = %d"
        .formatted(getLastName(), getGender(), getMaxAge(), count());
    }
  }
}
