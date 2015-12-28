/*
 * Copyright 2016 Author or Authors.
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

package org.cp.elements.util.search.support;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.cp.elements.util.search.AbstractMatcher;
import org.junit.Test;

/**
 * The LinearSearchTest class is a test suite class testing the contract and functionality of the LinearSearch class.
 *
 * @author John J. Blum
 * @see org.cp.elements.util.search.AbstractMatcher
 * @see org.cp.elements.util.search.support.LinearSearch
 * @see org.junit.Test
 * @since 1.0.0
 */
public class LinearSearchTest {

  @Test
  public void testSearch() {
    String[] animals = { "ape", "bat", "cat", "dog", "eel", "fish", "gorilla", "hamster", "iguana", "jackrabbit",
      "kangaroo", "lama", "mouse", "newt", "octopus", "parrot", "quail", "rat", "snail", "turtle", "uakari", "vulture",
      "walrus", "x-ray tetra", "yak", "zebra" };

    LinearSearch searcher = new LinearSearch();

    searcher.setMatcher(new AbstractMatcher<String>() {
      @Override public int match(final String animal) {
        return "octopus".compareTo(animal);
      }
    });

    assertEquals("octopus", searcher.search(animals));

    searcher.setMatcher(new AbstractMatcher<String>() {
      @Override public int match(final String animal) {
        return "baboon".compareTo(animal);
      }
    });

    assertNull(searcher.search(animals));
  }

  @Test
  public void testSearchUnorderedCollection() {
    String[] computerTerms = {
      "code", "byte", "vm", "xcode", "electronic", "defect", "float", "hard drive", "queue", "kilobyte", "ram", "long",
      "machine", "ada", "nanosecond", "octal", "java", "pseudo code", "int", "terabyte", "short", "u", "watt", "yarn",
      "groovy", "z"
    };

    LinearSearch searcher = new LinearSearch();

    searcher.setMatcher(new AbstractMatcher<String>() {
      @Override public int match(final String computerTerm) {
        return "xcode".compareTo(computerTerm);
      }
    });

    assertEquals("xcode", searcher.search(computerTerms));
  }

}
