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
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.assertThatIllegalStateException;
import static org.cp.elements.util.CollectionUtils.asIterable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link BreadthFirstIterator}.
 *
 * @author John J. Blum
 * @see java.util.Iterator
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.util.BreadthFirstIterator
 * @since 1.0.0
 */
@SuppressWarnings("unchecked")
public class BreadthFirstIteratorTest {

  @SuppressWarnings("unused")
  protected void log(List<?> list) {
    System.out.printf("%1$s%n", list);
  }

  protected <T> List<T> asList(T... elements) {
    List<T> list = new ArrayList<>(elements.length);
    Collections.addAll(list, elements);
    return list;
  }

  protected <T> List<T> toList(final Iterator<T> iterator) {

    List<T> list = new ArrayList<>();

    for (T element : asIterable(iterator)) {
      list.add(element);
    }

    return list;
  }

  protected <T> List<T> toListWeavedWithRemoves(final Iterator<T> iterator) {

    List<T> list = new ArrayList<>();

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

  @Test
  public void constructWithNullIteratorOfIterators() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new BreadthFirstIterator<>(null))
      .withMessage("The Iterator of Iterators must not be null!")
      .withNoCause();
  }

  @Test
  public void iterationOfUniformIteratorOfIterators() {

    List<String> row1 = Arrays.asList("A", "B", "C");
    List<String> row2 = Arrays.asList("D", "E", "F");
    List<String> row3 = Arrays.asList("G", "H", "I");

    Iterator<Iterator<String>> iteratorOfIterators = Arrays.asList(row1.iterator(), row2.iterator(), row3.iterator())
      .iterator();

    BreadthFirstIterator<String> iterator = new BreadthFirstIterator<>(iteratorOfIterators);

    assertThat(toList(iterator)).isEqualTo(Arrays.asList("A", "B", "C", "D", "E", "F", "G", "H", "I"));
    assertThat(iterator.hasNext()).isFalse();
  }

  @Test
  public void iterationOfNonUniformIteratorOfIterators() {

    List<String> row1 = Arrays.asList("A", "B", "C", "D");
    List<String> row2 = Arrays.asList("E", "F");
    List<String> row3 = Arrays.asList("G", "H", "I");

    Iterator<Iterator<String>> iteratorOfIterators = Arrays.asList(row1.iterator(), row2.iterator(), row3.iterator())
      .iterator();

    BreadthFirstIterator<String> iterator = new BreadthFirstIterator<>(iteratorOfIterators);

    assertThat(toList(iterator)).isEqualTo(Arrays.asList("A", "B", "C", "D", "E", "F", "G", "H", "I"));
    assertThat(iterator.hasNext()).isFalse();
  }

  @Test
  public void iterationOfIncreasingIteratorOfIterators() {

    List<String> row1 = Collections.singletonList("A");
    List<String> row2 = Arrays.asList("B", "C");
    List<String> row3 = Arrays.asList("D", "E", "F");
    List<String> row4 = Arrays.asList("G", "H", "I", "J");

    Iterator<Iterator<String>> iteratorOfIterators = Arrays.asList(row1.iterator(), row2.iterator(), row3.iterator(),
      row4.iterator()).iterator();

    BreadthFirstIterator<String> iterator = new BreadthFirstIterator<>(iteratorOfIterators);

    assertThat(toList(iterator)).isEqualTo(Arrays.asList("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"));
    assertThat(iterator.hasNext()).isFalse();
  }

  @Test
  public void iterationOfDecreasingIteratorOfIterators() {

    List<String> row1 = Arrays.asList("A", "B", "C", "D");
    List<String> row2 = Arrays.asList("E", "F", "G");
    List<String> row3 = Arrays.asList("H", "I");
    List<String> row4 = Collections.singletonList("J");

    Iterator<Iterator<String>> iteratorOfIterators = Arrays.asList(row1.iterator(), row2.iterator(), row3.iterator(),
      row4.iterator()).iterator();

    BreadthFirstIterator<String> iterator = new BreadthFirstIterator<>(iteratorOfIterators);

    assertThat(toList(iterator)).isEqualTo(Arrays.asList("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"));
    assertThat(iterator.hasNext()).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void iterationOfIteratorOfIteratorsWithEmptyRows() {

    List<String> row1 = Arrays.asList("A", "B", "C", "D");
    List<String> row2 = Collections.emptyList();
    List<String> row3 = Arrays.asList("E", "F", "G");
    List<String> row4 = Collections.emptyList();
    List<String> row5 = Arrays.asList("H", "I");
    List<String> row6 = Collections.emptyList();
    List<String> row7 = Collections.singletonList("J");
    List<String> row8 = Collections.emptyList();

    Iterator<Iterator<String>> iteratorOfIterators = Arrays.asList(row1.iterator(), row2.iterator(), row3.iterator(),
      row4.iterator(), row5.iterator(), row6.iterator(), row7.iterator(), row8.iterator()).iterator();

    BreadthFirstIterator<String> iterator = new BreadthFirstIterator<String>(iteratorOfIterators);

    assertThat(toList(iterator)).isEqualTo(Arrays.asList("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"));
    assertThat(iterator.hasNext()).isFalse();
  }

  @Test
  public void iterationOfStaggeredIteratorOfIteratorsAndNullRows() {

    List<String> row1 = Collections.singletonList("A");
    // row2 is null
    List<String> row3 = Arrays.asList("B", "C", "D", "E", "F");
    // row4, row5 are null
    List<String> row6 = Arrays.asList("G", "H");
    List<String> row7 = Arrays.asList("I", "J", "K");
    // row8 is null
    List<String> row9 = Collections.singletonList("L");

    Iterator<Iterator<String>> iteratorOfIterators = Arrays.asList(row1.iterator(), null, row3.iterator(),
      null, null, row6.iterator(), row7.iterator(), null, row9.iterator()).iterator();

    BreadthFirstIterator<String> iterator = new BreadthFirstIterator<>(iteratorOfIterators);

    assertThat(toList(iterator)).isEqualTo(Arrays.asList("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"));
    assertThat(iterator.hasNext()).isFalse();
  }

  @Test
  public void nextWhenIteratorExhausted() {

    assertThatExceptionOfType(NoSuchElementException.class)
      .isThrownBy(() -> new BreadthFirstIterator<>(Collections.emptyIterator()).next())
      .withMessage("The iteration has no more elements!")
      .withNoCause();
  }

  @Test
  public void removeOddIndexedElements() {

    List<String> row1 = asList("A", "B", "C");
    List<String> row2 = asList("D", "E", "F");
    List<String> row3 = asList("G", "H", "I");

    Iterator<Iterator<String>> iteratorOfIterators = asList(row1.iterator(), row2.iterator(), row3.iterator())
      .iterator();

    BreadthFirstIterator<String> iterator = new BreadthFirstIterator<>(iteratorOfIterators);

    assertThat(toListWeavedWithRemoves(iterator)).isEqualTo(Arrays.asList("A", "C", "E", "G", "I"));
    assertThat(iterator.hasNext()).isFalse();
  }

  @Test
  public void removeCalledBeforeNext() {

    assertThatIllegalStateException()
      .isThrownBy(() -> new BreadthFirstIterator<>(asList(asList("A").iterator()).iterator()).remove())
      .withMessage("next was not called before remove")
      .withNoCause();
  }

  @Test
  public void removeCalledTwiceBeforeNext() {

    BreadthFirstIterator<String> iterator = new BreadthFirstIterator<>(
      asList(asList("A").iterator()).iterator());

    assertThat(iterator).hasNext();
    assertThat(iterator.next()).isEqualTo("A");

    iterator.remove();

    assertThat(iterator.hasNext()).isFalse();

    assertThatIllegalStateException()
      .isThrownBy(iterator::remove)
      .withMessage("next was not called before remove")
      .withNoCause();
  }
}
