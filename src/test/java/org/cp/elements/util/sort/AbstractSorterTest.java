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
package org.cp.elements.util.sort;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import org.cp.elements.lang.ThrowableAssertions;
import org.cp.elements.lang.factory.ObjectInstantiationException;
import org.cp.elements.lang.support.SmartComparator;
import org.cp.elements.lang.support.SmartComparator.ComparableComparator;

import edu.umd.cs.mtc.MultithreadedTestCase;
import edu.umd.cs.mtc.TestFramework;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

/**
 * Unit Tests for {@link AbstractSorter}.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.util.sort.AbstractSorter
 * @see edu.umd.cs.mtc.MultithreadedTestCase
 * @see edu.umd.cs.mtc.TestFramework
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AbstractSorterTest {

  private static final String[] ELEMENTS = { "test", "testing", "tested" };

  @AfterEach
  public void tearDown() {
    AbstractSorter.ComparatorHolder.unset();
  }

  @Test
  public void setAndIsCustomComparatorAllowed() {

    AbstractSorter sorter = new TestSorter();

    assertThat(sorter.isCustomComparatorAllowed()).isEqualTo(AbstractSorter.DEFAULT_CUSTOM_COMPARATOR_ALLOWED);

    sorter.setCustomComparatorAllowed(false);

    assertThat(sorter.isCustomComparatorAllowed()).isFalse();

    sorter.setCustomComparatorAllowed(true);

    assertThat(sorter.isCustomComparatorAllowed()).isTrue();
  }

  @Test
  public void setAndGetOrderBy() {

    AbstractSorter sorter = new TestSorter();

    Comparator<?> mockOrderBy = mock(Comparator.class);

    sorter.setOrderBy(mockOrderBy);

    assertThat(sorter.getOrderBy()).isSameAs(mockOrderBy);

    sorter.setOrderBy(null);

    assertThat(sorter.getOrderBy()).isSameAs(SmartComparator.ComparableComparator.INSTANCE);
  }

  @Test
  public void setAndGetOrderByOfCallingThread() {

    AbstractSorter sorter = new TestSorter();

    Comparator<?> mockSorterOrderBy = mock(Comparator.class, "MockSorterOrderBy");
    Comparator<?> mockThreadOrderBy = mock(Comparator.class, "MockThreadOrderBy");

    assertThat(sorter.getOrderBy()).isSameAs(SmartComparator.ComparableComparator.INSTANCE);

    sorter.setOrderBy(mockSorterOrderBy);

    assertThat(sorter.getOrderBy()).isSameAs(mockSorterOrderBy);

    AbstractSorter.ComparatorHolder.set(mockThreadOrderBy);

    assertThat(sorter.getOrderBy()).isSameAs(mockThreadOrderBy);

    sorter.setOrderBy(null);

    assertThat(sorter.getOrderBy()).isSameAs(mockThreadOrderBy);

    AbstractSorter.ComparatorHolder.unset();

    assertThat(sorter.getOrderBy()).isSameAs(SmartComparator.ComparableComparator.INSTANCE);
  }

  @Test
  public void sortArray() {

    TestSorter sorter = new TestSorter();

    assertThat(sorter.sort(ELEMENTS)).isSameAs(ELEMENTS);
    assertThat(sorter.isSorted()).isTrue();
  }

  @Test
  public void sortSortableImplementObject() {

    TestSorter sorter = new TestSorter();

    assertThat(sorter.isCustomComparatorAllowed()).isTrue();
    assertThat(sorter.getOrderBy()).isEqualTo(SmartComparator.ComparableComparator.INSTANCE);
    assertThat(AbstractSorter.ComparatorHolder.isSet()).isFalse();
    assertThat(sorter.sort(TestSortableWithDefaults.INSTANCE)).isSameAs(TestSortableWithDefaults.INSTANCE);
    assertThat(sorter.isSorted()).isTrue();
    assertThat(sorter.getOrderBy()).isEqualTo(SmartComparator.ComparableComparator.INSTANCE);
    assertThat(AbstractSorter.ComparatorHolder.isSet()).isFalse();
  }

  @Test
  public void sortSortableAnnotatedObject() {

    TestSorter sorter = new TestSorter();

    assertThat(sorter.isCustomComparatorAllowed()).isTrue();
    assertThat(sorter.getOrderBy()).isEqualTo(SmartComparator.ComparableComparator.INSTANCE);
    assertThat(AbstractSorter.ComparatorHolder.isSet()).isFalse();
    assertThat(sorter.sort(TestSortableWithOverrides.INSTANCE)).isSameAs(TestSortableWithOverrides.INSTANCE);
    assertThat(sorter.isSorted()).isTrue();
    assertThat(sorter.getOrderBy()).isEqualTo(SmartComparator.ComparableComparator.INSTANCE);
    assertThat(AbstractSorter.ComparatorHolder.isSet()).isFalse();
  }

  @Test
  public void getSortableMetaDataWithSortableAnnotatedObjectUsingDefaults() {

    org.cp.elements.util.sort.annotation.Sortable sortable = new TestSorter()
      .getSortableMetaData(TestSortableWithDefaults.INSTANCE);

    assertThat(sortable).isNotNull();
    assertThat(sortable.listMethod()).isEqualTo("asList");
    assertThat(sortable.orderBy()).isEqualTo(Comparator.class);
  }

  @Test
  public void getSortableMetaDataWithSortableAnnotatedObjectUsingOverrides() {

    org.cp.elements.util.sort.annotation.Sortable sortable = new TestSorter()
      .getSortableMetaData(TestSortableWithOverrides.INSTANCE);

    assertThat(sortable).isNotNull();
    assertThat(sortable.listMethod()).isEqualTo("toCollection");
    assertThat(sortable.orderBy()).isEqualTo(TestComparator.class);
  }

  @Test
  public void getSortableMetaDataWithNullSortableAnnotatedObject() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new TestSorter().getSortableMetaData(null))
      .withMessage("The @Sortable annotated object cannot be null!")
      .withNoCause();
  }

  @Test
  public void getSortableMetaDataWithNonSortableAnnotatedObject() {

    assertThatExceptionOfType(SortException.class)
      .isThrownBy(() -> new TestSorter().getSortableMetaData(new Object()))
      .withMessage("To sort an object of type (%1$s), the class must be annotated with the (%2$s) annotation!",
          Object.class.getName(), org.cp.elements.util.sort.annotation.Sortable.class.getName())
      .withNoCause();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void configureComparatorWithSortableImplementingObject() {

    AbstractSorter sorter = new TestSorter();

    assertThat(sorter.isCustomComparatorAllowed()).isTrue();
    assertThat(sorter.getOrderBy()).isSameAs(SmartComparator.ComparableComparator.INSTANCE);

    Comparator<Comparable<?>> mockComparator = mock(Comparator.class);
    Sortable<Comparable<?>> mockSortable = mock(Sortable.class);

    when(mockSortable.getOrderBy()).thenReturn(mockComparator);

    assertThat(sorter.configureComparator(mockSortable)).isSameAs(mockSortable);
    assertThat(sorter.getOrderBy()).isSameAs(mockComparator);

    AbstractSorter.ComparatorHolder.unset();

    assertThat(sorter.getOrderBy()).isSameAs(SmartComparator.ComparableComparator.INSTANCE);

    verify(mockSortable, times(1)).getOrderBy();
  }

  @Test
  public void configureComparatorWithSortableImplementingObjectHavingNullOrderBy() {

    AbstractSorter sorter = new TestSorter();

    assertThat(sorter.isCustomComparatorAllowed()).isTrue();
    assertThat(sorter.getOrderBy()).isSameAs(SmartComparator.ComparableComparator.INSTANCE);

    Sortable<?> mockSortable = mock(Sortable.class);

    when(mockSortable.getOrderBy()).thenReturn(null);

    assertThat(sorter.configureComparator(mockSortable)).isSameAs(mockSortable);
    assertThat(sorter.getOrderBy()).isSameAs(SmartComparator.ComparableComparator.INSTANCE);

    verify(mockSortable, times(1)).getOrderBy();
  }

  @Test
  public void configureComparatorWithSortableImplementObjectWhenCustomComparatorNotAllowed() {

    AbstractSorter sorter = new TestSorter();

    sorter.setCustomComparatorAllowed(false);

    assertThat(sorter.isCustomComparatorAllowed()).isFalse();
    assertThat(sorter.getOrderBy()).isSameAs(SmartComparator.ComparableComparator.INSTANCE);

    Sortable<?> mockSortable = mock(Sortable.class);

    assertThat(sorter.configureComparator(mockSortable)).isSameAs(mockSortable);
    assertThat(sorter.getOrderBy()).isSameAs(SmartComparator.ComparableComparator.INSTANCE);

    verify(mockSortable, never()).getOrderBy();
  }

  @Test
  public void configureComparatorWithSortableAnnotatedObject() {

    AbstractSorter sorter = new TestSorter();

    assertThat(sorter.isCustomComparatorAllowed()).isTrue();
    assertThat(sorter.getOrderBy()).isSameAs(SmartComparator.ComparableComparator.INSTANCE);

    org.cp.elements.util.sort.annotation.Sortable sortableMetaData =
      sorter.getSortableMetaData(TestSortableWithOverrides.INSTANCE);

    assertThat(sorter.configureComparator(sortableMetaData)).isSameAs(sortableMetaData);
    assertThat(sorter.getOrderBy()).isNotEqualTo(SmartComparator.ComparableComparator.INSTANCE);
    assertThat(sorter.<String>getOrderBy() instanceof TestComparator).isTrue();

    AbstractSorter.ComparatorHolder.unset();

    assertThat(sorter.getOrderBy()).isSameAs(SmartComparator.ComparableComparator.INSTANCE);
  }

  @Test
  public void configureComparatorWithSortableAnnotatedObjectsHavingUnspecifiedOrderBy() {

    AbstractSorter sorter = new TestSorter();

    assertThat(sorter.isCustomComparatorAllowed()).isTrue();
    assertThat(sorter.getOrderBy()).isSameAs(SmartComparator.ComparableComparator.INSTANCE);

    org.cp.elements.util.sort.annotation.Sortable sortableMetaData =
      sorter.getSortableMetaData(TestSortableWithDefaults.INSTANCE);

    assertThat(sorter.configureComparator(sortableMetaData)).isSameAs(sortableMetaData);
    assertThat(sorter.getOrderBy()).isSameAs(SmartComparator.ComparableComparator.INSTANCE);
  }

  @Test
  public void configureComparatorWithSortableAnnotatedObjectWhenCustomComparatorNotAllowed() {

    AbstractSorter sorter = new TestSorter();

    sorter.setCustomComparatorAllowed(false);

    assertThat(sorter.isCustomComparatorAllowed()).isFalse();
    assertThat(sorter.getOrderBy()).isSameAs(SmartComparator.ComparableComparator.INSTANCE);

    org.cp.elements.util.sort.annotation.Sortable sortableMetaData =
      sorter.getSortableMetaData(TestSortableWithOverrides.INSTANCE);

    assertThat(sorter.configureComparator(sortableMetaData)).isSameAs(sortableMetaData);
    assertThat(sorter.getOrderBy()).isSameAs(SmartComparator.ComparableComparator.INSTANCE);
  }

  @Test
  public void configureComparatorWithSortableAnnotatedObjectUsingComparatorThrowingException() {

    AbstractSorter sorter = new TestSorter();

    assertThat(sorter.isCustomComparatorAllowed()).isTrue();
    assertThat(sorter.getOrderBy()).isSameAs(SmartComparator.ComparableComparator.INSTANCE);

    org.cp.elements.util.sort.annotation.Sortable sortableMetaData =
      sorter.getSortableMetaData(TestSortableWithProblem.INSTANCE);

    String expectedSortExceptionMessage =
      "Error occurred creating an instance of Comparator class (%1$s) to be used by this Sorter (%2$s)!"
        + " The Comparator class (%1$s) must have a public no-arg constructor!";

    ThrowableAssertions.assertThatThrowableOfType(SortException.class)
      .isThrownBy(args -> sorter.configureComparator(sortableMetaData))
      .havingMessage(expectedSortExceptionMessage, sortableMetaData.orderBy().getName(), sorter.getClass().getName())
      .causedBy(ObjectInstantiationException.class)
      .havingMessage("Failed to construct object of type [%s]", TestProblemComparator.class.getName())
      .causedBy(InvocationTargetException.class)
      .causedBy(IllegalStateException.class)
      .withNoCause();

    assertThat(sorter.getOrderBy()).isSameAs(SmartComparator.ComparableComparator.INSTANCE);
  }

  @Test
  public void asListWithDefaults() {

    AbstractSorter sorter = new TestSorter();

    List<String> elements = sorter.asList(TestSortableWithDefaults.INSTANCE,
      sorter.getSortableMetaData(TestSortableWithDefaults.INSTANCE));

    assertThat(elements).isNotNull();
    assertThat(elements.size()).isEqualTo(ELEMENTS.length);
    assertThat(elements.containsAll(Arrays.asList(ELEMENTS))).isTrue();
    assertThat(elements.get(0)).isEqualTo("test");
  }

  @Test
  public void asListWithOverrides() {

    AbstractSorter sorter = new TestSorter();

    List<String> elements = sorter.asList(TestSortableWithOverrides.INSTANCE,
      sorter.getSortableMetaData(TestSortableWithOverrides.INSTANCE));

    assertThat(elements).isNotNull();
    assertThat(elements.size()).isEqualTo(ELEMENTS.length);
    assertThat(elements.containsAll(Arrays.asList(ELEMENTS))).isTrue();
    assertThat(elements.get(0)).isEqualTo("tested");
  }

  @Test
  public void asListWithNullList() {

    AbstractSorter sorter = new TestSorter();

    List<String> elements = sorter.asList(TestSortableWithProblem.INSTANCE,
      sorter.getSortableMetaData(TestSortableWithProblem.INSTANCE));

    assertThat(elements).isNotNull();
    assertThat(elements.isEmpty()).isTrue();
  }

  @Test
  public void asListWithNonSortableAnnotatedObject() {

    assertThatExceptionOfType(SortException.class)
      .isThrownBy(() -> {
        AbstractSorter sorter = new TestSorter();
        sorter.asList(new Object(), sorter.getSortableMetaData(TestSortableWithDefaults.INSTANCE));
      })
      .withMessage("Error occurred getting the list of elements to sort from the (asList) method"
        + " on object of type (java.lang.Object)!")
      .withCauseInstanceOf(NoSuchMethodException.class);
  }

  @Test
  public void swap() {

    List<String> elements = new ArrayList<>(Arrays.asList("zero", "one", "two", "three"));

    assertThat(elements.get(1)).isEqualTo("one");
    assertThat(elements.get(2)).isEqualTo("two");

    new TestSorter().swap(elements, 1, 2);

    assertThat(elements.get(1)).isEqualTo("two");
    assertThat(elements.get(2)).isEqualTo("one");
  }

  @Test
  public void sortUsingSorterConcurrently() throws Throwable {
    TestFramework.runOnce(new UseSorterConcurrentlyMultithreadedTestCase());
  }

  @Test
  @SuppressWarnings("unchecked")
  public void comparableComparatorCompare() {

    User jonDoe = User.as("JonDoe");
    User pieDoe = User.as("PieDoe");

    assertThat(SmartComparator.ComparableComparator.INSTANCE.compare(jonDoe, User.as("JonDoe"))).isEqualTo(0);
    assertThat(SmartComparator.ComparableComparator.INSTANCE.compare(jonDoe, pieDoe)).isNegative();
    assertThat(SmartComparator.ComparableComparator.INSTANCE.compare(pieDoe, jonDoe)).isPositive();
  }

  @Test
  public void comparatorHolderGetIsSetSetAndUnset() {

    Comparator<?> mockComparator = mock(Comparator.class);

    assertThat(AbstractSorter.ComparatorHolder.get()).isNull();
    assertThat(AbstractSorter.ComparatorHolder.isSet()).isFalse();

    AbstractSorter.ComparatorHolder.set(mockComparator);

    assertThat(AbstractSorter.ComparatorHolder.get()).isSameAs(mockComparator);
    assertThat(AbstractSorter.ComparatorHolder.isSet()).isTrue();

    AbstractSorter.ComparatorHolder.unset();

    assertThat(AbstractSorter.ComparatorHolder.get()).isNull();
    assertThat(AbstractSorter.ComparatorHolder.isSet()).isFalse();
  }

  @Test
  public void sortableArrayListGetSetAndSize() {

    String[] elements = { "test", "testing", "tested" };

    AbstractSorter.SortableArrayList<String> list = new AbstractSorter.SortableArrayList<>(elements);

    assertThat(list.size()).isEqualTo(elements.length);
    assertThat(list.get(0)).isEqualTo("test");
    assertThat(list.get(1)).isEqualTo("testing");
    assertThat(list.get(2)).isEqualTo("tested");
    assertThat(list.set(1, "tester")).isEqualTo("testing");
    assertThat(list.get(1)).isEqualTo("tester");
    assertThat(list.size()).isEqualTo(elements.length);
  }

  public static class TestComparator implements Comparator<String> {

    @Override
    public int compare(String value1, String value2) {
      return value1.compareToIgnoreCase(value2);
    }
  }

  public static class TestProblemComparator implements Comparator<String> {

    public TestProblemComparator() {
      throw new IllegalStateException("Construction Failed");
    }

    @Override
    public int compare(String valueOne, String valueTwo) {
      return valueOne.compareTo(valueTwo);
    }
  }

  @org.cp.elements.util.sort.annotation.Sortable
  protected static class TestSortableWithDefaults {

    protected static final TestSortableWithDefaults INSTANCE = new TestSortableWithDefaults();

    public List<String> asList() {
      return Arrays.asList(ELEMENTS);
    }
  }

  @org.cp.elements.util.sort.annotation.Sortable(listMethod = "toCollection", orderBy = TestComparator.class)
  protected static class TestSortableWithOverrides {

    protected static final TestSortableWithOverrides INSTANCE = new TestSortableWithOverrides();

    public List<String> toCollection() {
      List<String> elements = new ArrayList<>(Arrays.asList(ELEMENTS));
      Collections.reverse(elements);
      return elements;
    }
  }

  @org.cp.elements.util.sort.annotation.Sortable(orderBy = TestProblemComparator.class)
  protected static class TestSortableWithProblem {

    protected static final TestSortableWithProblem INSTANCE = new TestSortableWithProblem();

    public List<String> asList() {
      return null;
    }
  }

  protected static final class TestSorter extends AbstractSorter {

    private boolean sorted = false;

    public boolean isSorted() {
      return this.sorted;
    }

    @Override
    public <E> List<E> sort(List<E> elements) {
      this.sorted = true;
      return elements;
    }
  }

  protected static final class UseSorterConcurrentlyMultithreadedTestCase extends MultithreadedTestCase {

    private AbstractSorter sorter;

    @Override
    public void initialize() {

      super.initialize();

      this.sorter = new AbstractSorter() {

        @Override
        public <E> List<E> sort(List<E> elements) {

          if ("Thread One".equalsIgnoreCase(Thread.currentThread().getName())) {
            assertThat(getOrderBy()).isNotSameAs(ComparableComparator.INSTANCE);
            waitForTick(2);
          }

          elements.sort(getOrderBy());

          return elements;
        }
      };

      this.sorter.setCustomComparatorAllowed(true);

      assertThat(this.sorter.getOrderBy()).isSameAs(SmartComparator.ComparableComparator.INSTANCE);
    }

    public void thread1() {

      Thread.currentThread().setName("Thread One");

      assertTick(0);
      assertThat(this.sorter.isCustomComparatorAllowed()).isTrue();
      assertThat( sorter.getOrderBy()).isSameAs(SmartComparator.ComparableComparator.INSTANCE);

      Sortable<String> sortable = this.sorter.sort(new Sortable<>() {

        private final List<String> elements = new ArrayList<>(Arrays.asList(ELEMENTS));

        @Override
        public List<String> asList() {
          return elements;
        }

        @Override
        public Comparator<String> getOrderBy() {
          return Comparator.reverseOrder();
        }
      });

      assertThat(sortable).isNotNull();

      List<String> elements = sortable.asList();

      assertThat(elements).isNotNull();
      assertThat(elements).isNotEmpty();
      assertThat(elements).hasSize(3);
      assertThat(elements).containsExactly("testing", "tested", "test");
    }

    public void thread2() {

      Thread.currentThread().setName("Thread Two");

      waitForTick(1);
      assertThat(this.sorter.isCustomComparatorAllowed()).isTrue();
      assertThat(this.sorter.getOrderBy()).isSameAs(SmartComparator.ComparableComparator.INSTANCE);

      Sortable<String> sortable = sorter.sort(new Sortable<>() {

        private final List<String> elements = new ArrayList<>(Arrays.asList(ELEMENTS));

        @Override
        public List<String> asList() {
          return elements;
        }

        @Override
        public Comparator<String> getOrderBy() {
          return null;
        }
      });

      assertThat(sortable).isNotNull();

      List<String> elements = sortable.asList();

      assertThat(elements).isNotNull();
      assertThat(elements).isNotEmpty();
      assertThat(elements).hasSize(3);
      assertThat(elements).containsExactly("test", "tested", "testing");
    }

    @Override
    public void finish() {
      super.finish();
      sorter = null;
    }
  }

  @Getter
  @ToString(of = "name")
  @EqualsAndHashCode(of = "name")
  @RequiredArgsConstructor(staticName = "as")
  static class User implements Comparable<User> {

    @lombok.NonNull
    private final String name;

    @Override
    public int compareTo(User other) {
      return this.getName().compareTo(other.getName());
    }
  }
}
