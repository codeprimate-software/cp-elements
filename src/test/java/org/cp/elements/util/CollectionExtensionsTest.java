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

package org.cp.elements.util;

import static org.junit.Assert.*;
import static org.junit.Assert.assertEquals;

import java.util.List;
import java.util.Set;

import org.junit.Test;

/**
 * The CollectionExtensionsTest class is a test suite of test cases testing the contract and functionality
 * of the CollectionExtensions class.
 *
 * @author John J. Blum
 * @see java.util.List
 * @see java.util.Set
 * @see org.cp.elements.util.CollectionExtensions
 * @see org.junit.Test
 * @since 1.0.0
 */
public class CollectionExtensionsTest {

  @Test
  public void testFromArrayToList() {
    final Object[] expected = { "Aardvark", "Baboon", "Cheetah", "Dog", "Elephant", "Ferret", "Guinea Pig", "Horse",
      "Iguana", "Jackal", "Kangaroo", "Lemming", "Moose", "N", "Octopus", "Pig", "Quail", "Rattlesnake", "Sheep",
      "Turkey", "U", "Velociraptor", "Whale", "X", "Y", "Zebra"
    };

    final List<Object> actual = CollectionExtensions.from(expected).toList();

    assertNotNull(actual);
    assertEquals(expected.length, actual.size());
    assertArrayEquals(expected, actual.toArray(new Object[actual.size()]));
  }

  @Test
  public void testFromEmptyArrayToList() {
    final Object[] expected = new Object[0];
    final List<Object> actual = CollectionExtensions.from(expected).toList();

    assertNotNull(actual);
    assertTrue(actual.isEmpty());
  }

  @Test
  public void testFromNullArrayToList() {
    final List<Object> actual = CollectionExtensions.from((Object[]) null).toList();

    assertNotNull(actual);
    assertTrue(actual.isEmpty());
  }

  @Test
  public void testFromSingleElementArrayToList() {
    final Object[] expected = { "test" };
    final List<Object> actual = CollectionExtensions.from(expected).toList();

    assertNotNull(actual);
    assertEquals(1, actual.size());
    assertArrayEquals(expected, actual.toArray(new Object[actual.size()]));
  }

  @Test
  public void testFromArrayToSet() {
    final Object[] expected = { "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" };
    final Set<Object> actual = CollectionExtensions.from(expected).toSet();

    assertNotNull(actual);
    assertEquals(expected.length, actual.size());

    for (final Object number : expected) {
      assertTrue("Expected the Set to contain (" + number + ")!", actual.contains(number));
    }
  }

  @Test
  public void testFromDuplicateElementArrayToSet() {
    final Object[] expected = { 1, 0, 1, 3, 2, 0, 1, 1, 3 };
    final Set<Object> actual = CollectionExtensions.from(expected).toSet();

    assertNotNull(actual);
    assertEquals(4, actual.size());
    assertTrue(actual.contains(0));
    assertTrue(actual.contains(1));
    assertTrue(actual.contains(2));
    assertTrue(actual.contains(3));
  }

  @Test
  public void testFromEmptyArrayToSet() {
    final Object[] expected = new Object[0];
    final Set<Object> actual = CollectionExtensions.from(expected).toSet();

    assertNotNull(actual);
    assertTrue(actual.isEmpty());
  }

  @Test
  public void testFromNullArrayToSet() {
    final Set<Object> actual = CollectionExtensions.from((Object[]) null).toSet();

    assertNotNull(actual);
    assertTrue(actual.isEmpty());
  }

  @Test
  public void testFromSingleElementArrayToSet() {
    final Object[] expected = { "test" };
    final Set<Object> actual = CollectionExtensions.from(expected).toSet();

    assertNotNull(actual);
    assertEquals(1, actual.size());
    assertTrue(actual.contains(expected[0]));
  }

  @Test
  public void testFromArrayToString() {
    assertEquals("[one, two, three]", CollectionExtensions.from("one", "two", "three").toString());
  }

  @Test
  public void testFromArrayContainingBlankEmptyOrNullElementsToString() throws Exception {
    assertEquals("[, test,  , , null]", CollectionExtensions.from("", "test", " ", "", null).toString());
  }

  @Test
  public void testFromEmptyArrayToString() {
    assertEquals("[]", CollectionExtensions.from(new Object[0].clone()).toString());
  }

  @Test
  public void testFromNullArrayToString() {
    assertEquals("[]", CollectionExtensions.from((Object[]) null).toString());
  }

  @Test
  public void testFromSingleElementArrayToString() {
    assertEquals("[test]", CollectionExtensions.from("test").toString());
  }

}
