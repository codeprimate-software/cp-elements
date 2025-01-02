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

import java.util.Comparator;
import java.util.Set;

import org.junit.jupiter.api.Test;

import org.cp.elements.data.oql.Oql.Projection;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Nameable;

import lombok.AccessLevel;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

/**
 * Unit Tests for {@link Oql}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.Oql
 * @see org.junit.jupiter.api.Test
 * @since 2.0.0
 */
public class OqlUnitTests {

  @Test
  void queryAll() {

    Set<Person> people = Set.of(
      Person.named("Jon", "Doe"),
      Person.named("Jane", "Doe"),
      Person.named("Pie", "Doe")
    );

    Iterable<Person> result = Oql.defaultProvider().select(Projection.star(Person.class))
      .from(people)
      .execute();

    assertThat(result).isNotNull();
    assertThat(result).isNotSameAs(people);
    assertThat(result).containsAll(people);
  }

  @Test
  void queryProjected() {

    Set<Person> people = Set.of(
      Person.named("Pie", "Doe"),
      Person.named("Jon", "Doe"),
      Person.named("Jane", "Doe")
    );

    Projection<Person, String> projection = Projection.<Person, String>of(String.class)
      .mappedWith(Person::getName);

    Iterable<String> result = Oql.defaultProvider().select(projection)
      .from(people)
      .execute();

    assertThat(result).isNotNull();
    assertThat(result).containsExactlyInAnyOrder("Jane Doe", "Jon Doe", "Pie Doe");
  }

  @Test
  void queryProjectedAndOrdered() {

    Set<Person> people = Set.of(
      Person.named("Jane", "Doe").withAge(48),
      Person.named("Pie", "Doe").withAge(16),
      Person.named("Jon", "Doe").withAge(42)
    );

    Projection<Person, String> projection = Projection.<Person, String>of(String.class)
      .mappedWith(Person::getName);

    Iterable<String> result = Oql.defaultProvider().select(projection)
      .from(people)
      .orderBy(Comparator.comparing(Person::getAge))
      .execute();

    assertThat(result).isNotNull();
    assertThat(result).containsExactly("Pie Doe", "Jon Doe", "Jane Doe");
  }

  @Test
  void queryProjectedOrderedAndFiltered() {

    Set<Person> people = Set.of(
      Person.named("Pie", "Doe").withAge(16),
      Person.named("Jon", "Doe").withAge(42),
      Person.named("Jane", "Doe").withAge(48),
      Person.named("Jack", "Handy").withAge(51),
      Person.named("Sandy", "Handy").withAge(47)
    );

    Projection<Person, String> projection = Projection.<Person, String>of(String.class)
      .mappedWith(Person::getName);

    Iterable<String> result = Oql.defaultProvider().select(projection)
      .from(people)
      .where(person -> "doe".equalsIgnoreCase(person.getLastName()))
      .and(person -> person.getAge() > 40)
      .orderBy(Comparator.comparing(Person::getAge))
      .descending()
      .execute();

    assertThat(result).isNotNull();
    assertThat(result).containsExactly("Jane Doe", "Jon Doe");
  }

  @Getter
  @ToString
  @EqualsAndHashCode
  @RequiredArgsConstructor(access = AccessLevel.PACKAGE)
  static class Person implements Comparable<Person>, Nameable<String> {

    static final String NAME_FORMAT = "%s %s";

    static Person named(String firstName, String lastName) {
      return new Person(firstName, lastName);
    }

    private final String firstName;
    private final String lastName;

    private int age;

    @Override
    public int compareTo(Person that) {
      return this.getName().compareTo(that.getName());
    }

    @Override
    public String getName() {
      return NAME_FORMAT.formatted(getFirstName(), getLastName());
    }

    Person withAge(int age) {
      Assert.isTrue(age > 0, "Person's age must be greater than 0");
      this.age = age;
      return this;
    }
  }
}
