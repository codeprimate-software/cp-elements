/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.util.search.support;

import static org.junit.Assert.*;

import org.cp.elements.util.search.AbstractMatcher;
import org.junit.Test;

/**
 * The BinarySearchTest class is a test suite of test cases testing the contract and functionality of the BinarySearch
 * class.
 * <p/>
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
