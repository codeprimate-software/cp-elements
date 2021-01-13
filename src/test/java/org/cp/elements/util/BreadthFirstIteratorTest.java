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

import static org.cp.elements.util.CollectionUtils.asIterable;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

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

    for (T element : asIterable(iterator)) {
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

  @Test(expected = IllegalArgumentException.class)
  public void constructWithNullIteratorOfIterators() {
    try {
      new BreadthFirstIterator<Object>(null);
    }
    catch (IllegalArgumentException expected) {
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
