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
package org.cp.elements.util;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.List;
import java.util.Set;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link CollectionExtensions}.
 *
 * @author John J. Blum
 * @see org.cp.elements.util.CollectionExtensions
 * @see org.junit.jupiter.api.Test
 * @since 1.0.0
 */
class CollectionExtensionsTest {

  @Test
  void testFromArrayToList() {

    Object[] expected = { "Aardvark", "Baboon", "Cheetah", "Dog", "Elephant", "Ferret", "Guinea Pig", "Horse",
      "Iguana", "Jackal", "Kangaroo", "Lemming", "Moose", "N", "Octopus", "Pig", "Quail", "Rattlesnake", "Sheep",
      "Turkey", "U", "Velociraptor", "Whale", "X", "Y", "Zebra"
    };

    List<Object> actual = CollectionExtensions.from(expected).toList();

    assertThat(actual).isNotNull();
    assertThat(actual.size()).isEqualTo(expected.length);
    assertThat(actual.toArray(new Object[0])).isEqualTo(expected);
  }

  @Test
  void testFromEmptyArrayToList() {

    Object[] expected = new Object[0];
    List<Object> actual = CollectionExtensions.from(expected).toList();

    assertThat(actual).isNotNull();
    assertThat(actual.isEmpty()).isTrue();
  }

  @Test
  void testFromNullArrayToList() {

    List<Object> actual = CollectionExtensions.from((Object[]) null).toList();

    assertThat(actual).isNotNull();
    assertThat(actual.isEmpty()).isTrue();
  }

  @Test
  void testFromSingleElementArrayToList() {

    Object[] expected = { "test" };
    List<Object> actual = CollectionExtensions.from(expected).toList();

    assertThat(actual).isNotNull();
    assertThat(actual.size()).isEqualTo(1);
    assertThat(actual.toArray(new Object[0])).isEqualTo(expected);
  }

  @Test
  void testFromArrayToSet() {

    Object[] expected = { "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" };
    Set<Object> actual = CollectionExtensions.from(expected).toSet();

    assertThat(actual).isNotNull();
    assertThat(actual.size()).isEqualTo(expected.length);

    for (Object number : expected) {
      assertThat(actual.contains(number)).as("Expected the Set to contain (" + number + ")!").isTrue();
    }
  }

  @Test
  void testFromDuplicateElementArrayToSet() {

    Object[] expected = { 1, 0, 1, 3, 2, 0, 1, 1, 3 };
    Set<Object> actual = CollectionExtensions.from(expected).toSet();

    assertThat(actual).isNotNull();
    assertThat(actual.size()).isEqualTo(4);
    assertThat(actual.contains(0)).isTrue();
    assertThat(actual.contains(1)).isTrue();
    assertThat(actual.contains(2)).isTrue();
    assertThat(actual.contains(3)).isTrue();
  }

  @Test
  void testFromEmptyArrayToSet() {

    Object[] expected = new Object[0];
    Set<Object> actual = CollectionExtensions.from(expected).toSet();

    assertThat(actual).isNotNull();
    assertThat(actual.isEmpty()).isTrue();
  }

  @Test
  void testFromNullArrayToSet() {

    Set<Object> actual = CollectionExtensions.from((Object[]) null).toSet();

    assertThat(actual).isNotNull();
    assertThat(actual.isEmpty()).isTrue();
  }

  @Test
  void testFromSingleElementArrayToSet() {

    Object[] expected = { "test" };
    Set<Object> actual = CollectionExtensions.from(expected).toSet();

    assertThat(actual).isNotNull();
    assertThat(actual.size()).isEqualTo(1);
    assertThat(actual.contains(expected[0])).isTrue();
  }

  @Test
  void testFromArrayToString() {
    assertThat(CollectionExtensions.from("one", "two", "three").toString()).isEqualTo("[one, two, three]");
  }

  @Test
  void testFromArrayContainingBlankEmptyOrNullElementsToString() throws Exception {
    assertThat(CollectionExtensions.from("", "test", " ", "", null).toString()).isEqualTo("[, test,  , , null]");
  }

  @Test
  void testFromEmptyArrayToString() {
    assertThat(CollectionExtensions.from(new Object[0].clone()).toString()).isEqualTo("[]");
  }

  @Test
  void testFromNullArrayToString() {
    assertThat(CollectionExtensions.from((Object[]) null).toString()).isEqualTo("[]");
  }

  @Test
  void testFromSingleElementArrayToString() {
    assertThat(CollectionExtensions.from("test").toString()).isEqualTo("[test]");
  }
}
