/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 *
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 *
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 *
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 *
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.util;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import org.junit.Test;

/**
 * The BreadthFirstIteratorTest class is a test suite of test cases testing the contract and functionality
 * of the BreadthFirstIterator class.
 *
 * @author John J. Blum
 * @see java.util.Iterator
 * @see org.cp.elements.util.BreadthFirstIterator
 * @see org.junit.Test
 * @since 1.0.0
 */
@SuppressWarnings("unchecked")
public class BreadthFirstIteratorTest {

  @SuppressWarnings("unused")
  protected void log(List<?> list) {
    System.out.printf("%1$s%n", list);
  }

  protected <T> List<T> asList(T... elements) {
    List<T> list = new ArrayList<T>(elements.length);
    Collections.addAll(list, elements);
    return list;
  }

  protected <T> List<T> toList(final Iterator<T> iterator) {
    List<T> list = new ArrayList<T>();

    for (T element : CollectionUtils.iterable(iterator)) {
      list.add(element);
    }

    return list;
  }

  protected <T> List<T> toListWeavedWithRemoves(final Iterator<T> iterator) {
    List<T> list = new ArrayList<T>();
    int index = 0;

    while (iterator.hasNext()) {
      T element = iterator.next();

      if (index++ % 2 == 1) {
        iterator.remove();
      }
      else {
        list.add(element);
      }
    }

    return list;
  }

  @Test(expected = NullPointerException.class)
  public void constructWithNullIteratorOfIterators() {
    try {
      new BreadthFirstIterator<Object>(null);
    }
    catch (NullPointerException expected) {
      assertEquals("The Iterator of Iterators must not be null!", expected.getMessage());
      throw expected;
    }
  }

  @Test
  public void iterationOfUniformIteratorOfIterators() {
    List<String> row1 = Arrays.asList("A", "B", "C");
    List<String> row2 = Arrays.asList("D", "E", "F");
    List<String> row3 = Arrays.asList("G", "H", "I");

    Iterator<Iterator<String>> iteratorOfIterators = Arrays.asList(row1.iterator(), row2.iterator(), row3.iterator())
      .iterator();

    BreadthFirstIterator<String> iterator = new BreadthFirstIterator<String>(iteratorOfIterators);

    assertEquals(Arrays.asList("A", "B", "C", "D", "E", "F", "G", "H", "I"), toList(iterator));
    assertFalse(iterator.hasNext());
  }

  @Test
  public void iterationOfNonUniformIteratorOfIterators() {
    List<String> row1 = Arrays.asList("A", "B", "C", "D");
    List<String> row2 = Arrays.asList("E", "F");
    List<String> row3 = Arrays.asList("G", "H", "I");

    Iterator<Iterator<String>> iteratorOfIterators = Arrays.asList(row1.iterator(), row2.iterator(), row3.iterator())
      .iterator();

    BreadthFirstIterator<String> iterator = new BreadthFirstIterator<String>(iteratorOfIterators);

    assertEquals(Arrays.asList("A", "B", "C", "D", "E", "F", "G", "H", "I"), toList(iterator));
    assertFalse(iterator.hasNext());
  }

  @Test
  public void iterationOfIncreasingIteratorOfIterators() {
    List<String> row1 = Arrays.asList("A");
    List<String> row2 = Arrays.asList("B", "C");
    List<String> row3 = Arrays.asList("D", "E", "F");
    List<String> row4 = Arrays.asList("G", "H", "I", "J");

    Iterator<Iterator<String>> iteratorOfIterators = Arrays.asList(row1.iterator(), row2.iterator(), row3.iterator(),
      row4.iterator()).iterator();

    BreadthFirstIterator<String> iterator = new BreadthFirstIterator<String>(iteratorOfIterators);

    assertEquals(Arrays.asList("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"), toList(iterator));
    assertFalse(iterator.hasNext());
  }

  @Test
  public void iterationOfDecreasingIteratorOfIterators() {
    List<String> row1 = Arrays.asList("A", "B", "C", "D");
    List<String> row2 = Arrays.asList("E", "F", "G");
    List<String> row3 = Arrays.asList("H", "I");
    List<String> row4 = Arrays.asList("J");

    Iterator<Iterator<String>> iteratorOfIterators = Arrays.asList(row1.iterator(), row2.iterator(), row3.iterator(),
      row4.iterator()).iterator();

    BreadthFirstIterator<String> iterator = new BreadthFirstIterator<String>(iteratorOfIterators);

    assertEquals(Arrays.asList("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"), toList(iterator));
    assertFalse(iterator.hasNext());
  }

  @Test
  public void iterationOfIteratorOfIteratorsWithEmptyRows() {
    List<String> row1 = Arrays.asList("A", "B", "C", "D");
    List<String> row2 = Collections.emptyList();
    List<String> row3 = Arrays.asList("E", "F", "G");
    List<String> row4 = Collections.emptyList();
    List<String> row5 = Arrays.asList("H", "I");
    List<String> row6 = Collections.emptyList();
    List<String> row7 = Arrays.asList("J");
    List<String> row8 = Collections.emptyList();

    Iterator<Iterator<String>> iteratorOfIterators = Arrays.asList(row1.iterator(), row2.iterator(), row3.iterator(),
      row4.iterator(), row5.iterator(), row6.iterator(), row7.iterator(), row8.iterator()).iterator();

    BreadthFirstIterator<String> iterator = new BreadthFirstIterator<String>(iteratorOfIterators);

    assertEquals(Arrays.asList("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"), toList(iterator));
    assertFalse(iterator.hasNext());
  }

  @Test
  public void iterationOfStaggeredIteratorOfIteratorsAndNullRows() {
    List<String> row1 = Arrays.asList("A");
    // row2 is null
    List<String> row3 = Arrays.asList("B", "C", "D", "E", "F");
    // row4, row5 are null
    List<String> row6 = Arrays.asList("G", "H");
    List<String> row7 = Arrays.asList("I", "J", "K");
    // row8 is null
    List<String> row9 = Arrays.asList("L");

    Iterator<Iterator<String>> iteratorOfIterators = Arrays.asList(row1.iterator(), null, row3.iterator(),
      null, null, row6.iterator(), row7.iterator(), null, row9.iterator()).iterator();

    BreadthFirstIterator<String> iterator = new BreadthFirstIterator<String>(iteratorOfIterators);

    assertEquals(Arrays.asList("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"), toList(iterator));
    assertFalse(iterator.hasNext());
  }

  @Test(expected = NoSuchElementException.class)
  public void nextWhenIteratorExhausted() {
    try {
      new BreadthFirstIterator<Object>(Collections.<Iterator<Object>>emptyList().iterator()).next();
    }
    catch (NoSuchElementException expected) {
      assertEquals("The iteration has no more elements!", expected.getMessage());
      throw expected;
    }
  }

  @Test
  public void removeOddIndexedElements() {
    List<String> row1 = asList("A", "B", "C");
    List<String> row2 = asList("D", "E", "F");
    List<String> row3 = asList("G", "H", "I");

    Iterator<Iterator<String>> iteratorOfIterators = asList(row1.iterator(), row2.iterator(), row3.iterator())
      .iterator();

    BreadthFirstIterator<String> iterator = new BreadthFirstIterator<String>(iteratorOfIterators);

    assertEquals(Arrays.asList("A", "C", "E", "G", "I"), toListWeavedWithRemoves(iterator));
    assertFalse(iterator.hasNext());
  }

  @Test(expected = IllegalStateException.class)
  public void removeCalledBeforeNext() {
    try {
      new BreadthFirstIterator<String>(asList(asList("A").iterator()).iterator()).remove();
    }
    catch (IllegalStateException expected) {
      assertEquals("next was not called before remove", expected.getMessage());
      throw expected;
    }
  }

  @Test(expected = IllegalStateException.class)
  public void removeCalledTwiceBeforeNext() {
    try {
      BreadthFirstIterator<String> iterator = new BreadthFirstIterator<String>(
        asList(asList("A").iterator()).iterator());

      assertTrue(iterator.hasNext());
      assertEquals("A", iterator.next());

      iterator.remove();

      assertFalse(iterator.hasNext());

      iterator.remove();
    }
    catch (IllegalStateException expected) {
      assertEquals("next was not called before remove", expected.getMessage());
      throw expected;
    }
  }

}
