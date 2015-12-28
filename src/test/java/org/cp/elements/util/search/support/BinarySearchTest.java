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
 * The BinarySearchTest class is a test suite of test cases testing the contract and functionality of the BinarySearch
 * class.
 *
 * @author John J. Blum
 * @see org.cp.elements.util.search.AbstractMatcher
 * @see org.cp.elements.util.search.support.BinarySearch
 * @see org.junit.Test
 * @since 1.0.0
 */
public class BinarySearchTest {

  @Test
  public void testSearch() {
    String[] animals = { "ape", "bat", "cat", "dog", "eel", "fish", "gorilla", "hamster", "iguana", "jackrabbit",
      "kangaroo", "lama", "mouse", "newt", "octopus", "parrot", "quail", "rat", "snail", "turtle", "uakari", "vulture",
      "walrus", "x-ray tetra", "yak", "zebra" };

    BinarySearch searcher = new BinarySearch();

    searcher.setMatcher(new AbstractMatcher<String>() {
      @Override public int match(final String animal) {
        return "Zebra".compareTo(animal);
      }
    });

    assertNull(searcher.search(animals));

    searcher.setMatcher(new AbstractMatcher<String>() {
      @Override public int match(final String animal) {
        return "gorilla".compareTo(animal);
      }
    });

    assertEquals("gorilla", searcher.search(animals));

    searcher.setMatcher(new AbstractMatcher<String>() {
      @Override public int match(final String animal) {
        return "rattlesnake".compareTo(animal);
      }
    });

    assertNull(searcher.search(animals));

    searcher.setMatcher(new AbstractMatcher<String>() {
      @Override public int match(final String animal) {
        return "APE".compareTo(animal.toUpperCase());
      }
    });

    assertEquals("ape", searcher.search(animals));
  }

  @Test
  public void testSearchUnorderedCollection() {
    String[] computerTerms = {
      "code", "byte", "vm", "xcode", "electronic", "defect", "float", "hard drive", "queue", "kilobyte", "ram", "long",
      "machine", "ada", "nanosecond", "octal", "java", "pseudo code", "int", "terabyte", "short", "u", "watt", "yarn",
      "groovy", "z"
    };

    BinarySearch searcher = new BinarySearch();

    searcher.setMatcher(new AbstractMatcher<String>() {
      @Override public int match(final String computerTerm) {
        return "xcode".compareTo(computerTerm);
      }
    });

    assertNull(searcher.search(computerTerms));
  }

}
