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

package org.cp.elements.util.sort;

import static org.cp.elements.test.TestUtils.assertNegative;
import static org.cp.elements.test.TestUtils.assertPositive;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.cp.elements.test.AbstractMockingTestSuite;
import org.jmock.Expectations;
import org.junit.After;
import org.junit.Test;

import edu.umd.cs.mtc.MultithreadedTestCase;
import edu.umd.cs.mtc.TestFramework;

/**
 * The AbstractSorterTest class is a test suite of test cases testing the contract and functionality of the
 * AbstractSorter class.
 *
 * @author John J. Blum
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.cp.elements.util.sort.AbstractSorter
 * @see edu.umd.cs.mtc.MultithreadedTestCase
 * @see edu.umd.cs.mtc.TestFramework
 * @see org.junit.Test
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AbstractSorterTest extends AbstractMockingTestSuite {

  private static final String[] ELEMENTS = { "test", "testing", "tested" };

  @After
  public void tearDown() {
    AbstractSorter.ComparatorHolder.unset();
  }

  @Test
  public void testSetAndIsCustomComparatorAllowed() {
    AbstractSorter sorter = new TestSorter();

    assertEquals(AbstractSorter.DEFAULT_CUSTOM_COMPARATOR_ALLOWED, sorter.isCustomComparatorAllowed());

    sorter.setCustomComparatorAllowed(false);

    assertFalse(sorter.isCustomComparatorAllowed());

    sorter.setCustomComparatorAllowed(true);

    assertTrue(sorter.isCustomComparatorAllowed());
  }

  @Test
  public void testSetAndGetOrderBy() {
    AbstractSorter sorter = new TestSorter();
    Comparator mockOrderBy = mockContext.mock(Comparator.class);

    sorter.setOrderBy(mockOrderBy);

    assertSame(mockOrderBy, sorter.getOrderBy());

    sorter.setOrderBy(null);

    assertSame(AbstractSorter.ComparableComparator.INSTANCE, sorter.getOrderBy());
  }

  @Test
  public void testSetAndGetOrderByOfCallingThread() {
    AbstractSorter sorter = new TestSorter();

    Comparator mockSorterOrderBy = mockContext.mock(Comparator.class, "testSetAndGetOrderByOfCallingThread.Sorter");
    Comparator mockThreadOrderBy = mockContext.mock(Comparator.class, "testSetAndGetOrderByOfCallingThread.Thread");

    assertSame(AbstractSorter.ComparableComparator.INSTANCE, sorter.getOrderBy());

    sorter.setOrderBy(mockSorterOrderBy);

    assertSame(mockSorterOrderBy, sorter.getOrderBy());

    AbstractSorter.ComparatorHolder.set(mockThreadOrderBy);

    assertSame(mockThreadOrderBy, sorter.getOrderBy());

    sorter.setOrderBy(null);

    assertSame(mockThreadOrderBy, sorter.getOrderBy());

    AbstractSorter.ComparatorHolder.unset();

    assertSame(AbstractSorter.ComparableComparator.INSTANCE, sorter.getOrderBy());
  }

  @Test
  public void testSortArray() {
    TestSorter sorter = new TestSorter();

    assertSame(ELEMENTS, sorter.sort(ELEMENTS));
    assertTrue(sorter.isSorted());
  }

  @Test
  public void testSortSortableImplementObject() {
    TestSorter sorter = new TestSorter();

    assertTrue(sorter.isCustomComparatorAllowed());
    assertEquals(AbstractSorter.ComparableComparator.INSTANCE, sorter.getOrderBy());
    assertFalse(AbstractSorter.ComparatorHolder.isSet());
    assertSame(TestSortableWithDefaults.INSTANCE, sorter.sort(TestSortableWithDefaults.INSTANCE));
    assertTrue(sorter.isSorted());
    assertEquals(AbstractSorter.ComparableComparator.INSTANCE, sorter.getOrderBy());
    assertFalse(AbstractSorter.ComparatorHolder.isSet());
  }

  @Test
  public void testSortSortableAnnotatedObject() {
    TestSorter sorter = new TestSorter();

    assertTrue(sorter.isCustomComparatorAllowed());
    assertEquals(AbstractSorter.ComparableComparator.INSTANCE, sorter.getOrderBy());
    assertFalse(AbstractSorter.ComparatorHolder.isSet());
    assertSame(TestSortableWithOverrides.INSTANCE, sorter.sort(TestSortableWithOverrides.INSTANCE));
    assertTrue(sorter.isSorted());
    assertEquals(AbstractSorter.ComparableComparator.INSTANCE, sorter.getOrderBy());
    assertFalse(AbstractSorter.ComparatorHolder.isSet());
  }

  @Test
  public void testGetSortableMetaDataWithSortableAnnotatedObjectUsingDefaults() {
    org.cp.elements.util.sort.annotation.Sortable sortable = new TestSorter().getSortableMetaData(
      TestSortableWithDefaults.INSTANCE);

    assertNotNull(sortable);
    assertEquals("asList", sortable.listMethod());
    assertEquals(Comparator.class, sortable.orderBy());
  }

  @Test
  public void testGetSortableMetaDataWithSortableAnnotatedObjectUsingOverrides() {
    org.cp.elements.util.sort.annotation.Sortable sortable = new TestSorter().getSortableMetaData(
      TestSortableWithOverrides.INSTANCE);

    assertNotNull(sortable);
    assertEquals("toCollection", sortable.listMethod());
    assertEquals(TestComparator.class, sortable.orderBy());
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetSortableMetaDataWithNullSortableAnnotatedObject() {
    try {
      new TestSorter().getSortableMetaData(null);
    }
    catch (IllegalArgumentException expected) {
      assertEquals("The @Sortable annotated object cannot be null!", expected.getMessage());
      throw expected;
    }
  }

  @Test(expected = SortException.class)
  public void testGetSortableMetaDataWithNonSortableAnnotatedObject() {
    try {
      new TestSorter().getSortableMetaData(new Object());
    }
    catch (SortException expected) {
      assertEquals(String.format("To sort an object of type (%1$s), the class must be annotated with the (%2$s) annotation!",
        Object.class.getName(), org.cp.elements.util.sort.annotation.Sortable.class.getName()), expected.getMessage());
      throw expected;
    }
  }

  @Test
  public void testConfigureComparatorWithSortableImplementingObject() {
    AbstractSorter sorter = new TestSorter();

    assertTrue(sorter.isCustomComparatorAllowed());
    assertSame(AbstractSorter.ComparableComparator.INSTANCE, sorter.getOrderBy());

    final Comparator<?> mockComparator = mockContext.mock(Comparator.class,
      "testConfigureComparatorWithSortableImplementingObject.Comparator");

    final Sortable<?> mockSortable = mockContext.mock(Sortable.class,
      "testConfigureComparatorWithSortableImplementingObject.Sortable");

    mockContext.checking(new Expectations() {{
      oneOf(mockSortable).getOrderBy();
      will(returnValue(mockComparator));
    }});

    assertSame(mockSortable, sorter.configureComparator(mockSortable));
    assertSame(mockComparator, sorter.getOrderBy());

    AbstractSorter.ComparatorHolder.unset();

    assertSame(AbstractSorter.ComparableComparator.INSTANCE, sorter.getOrderBy());
  }

  @Test
  public void testConfigureComparatorWithSortableImplementingObjectHavingNullOrderBy() {
    AbstractSorter sorter = new TestSorter();

    assertTrue(sorter.isCustomComparatorAllowed());
    assertSame(AbstractSorter.ComparableComparator.INSTANCE, sorter.getOrderBy());

    final Sortable<?> mockSortable = mockContext.mock(Sortable.class,
      "testConfigureComparatorWithSortableImplementingObject.Sortable");

    mockContext.checking(new Expectations() {{
      oneOf(mockSortable).getOrderBy();
      will(returnValue(null));
    }});

    assertSame(mockSortable, sorter.configureComparator(mockSortable));
    assertSame(AbstractSorter.ComparableComparator.INSTANCE, sorter.getOrderBy());
  }

  @Test
  public void testConfigureComparatorWithSortableImplementObjectWhenCustomComparatorNotAllowed() {
    AbstractSorter sorter = new TestSorter();

    sorter.setCustomComparatorAllowed(false);

    assertFalse(sorter.isCustomComparatorAllowed());
    assertSame(AbstractSorter.ComparableComparator.INSTANCE, sorter.getOrderBy());

    final Sortable<?> mockSortable = mockContext.mock(Sortable.class,
      "testConfigureComparatorWithSortableImplementObjectWhenCustomComparatorIsNotAllowed.Sortable");

    mockContext.checking(new Expectations() {{
      never(mockSortable).getOrderBy();
    }});

    assertSame(mockSortable, sorter.configureComparator(mockSortable));
    assertSame(AbstractSorter.ComparableComparator.INSTANCE, sorter.getOrderBy());
  }

  @Test
  public void testConfigureComparatorWithSortableAnnotatedObject() {
    AbstractSorter sorter = new TestSorter();

    assertTrue(sorter.isCustomComparatorAllowed());
    assertSame(AbstractSorter.ComparableComparator.INSTANCE, sorter.getOrderBy());

    org.cp.elements.util.sort.annotation.Sortable sortableMetaData = sorter.getSortableMetaData(
      TestSortableWithOverrides.INSTANCE);

    assertSame(sortableMetaData, sorter.configureComparator(sortableMetaData));
    assertNotEquals(AbstractSorter.ComparableComparator.INSTANCE, sorter.getOrderBy());
    assertTrue(sorter.<String>getOrderBy() instanceof TestComparator);

    AbstractSorter.ComparatorHolder.unset();

    assertSame(AbstractSorter.ComparableComparator.INSTANCE, sorter.getOrderBy());
  }

  @Test
  public void testConfigureComparatorWithSortableAnnotatedObjectsHavingUnspecifiedOrderBy() {
    AbstractSorter sorter = new TestSorter();

    assertTrue(sorter.isCustomComparatorAllowed());
    assertSame(AbstractSorter.ComparableComparator.INSTANCE, sorter.getOrderBy());

    org.cp.elements.util.sort.annotation.Sortable sortableMetaData = sorter.getSortableMetaData(
      TestSortableWithDefaults.INSTANCE);

    assertSame(sortableMetaData, sorter.configureComparator(sortableMetaData));
    assertSame(AbstractSorter.ComparableComparator.INSTANCE, sorter.getOrderBy());
  }

  @Test
  public void testConfigureComparatorWithSortableAnnotatedObjectWhenCustomComparatorNotAllowed() {
    AbstractSorter sorter = new TestSorter();

    sorter.setCustomComparatorAllowed(false);

    assertFalse(sorter.isCustomComparatorAllowed());
    assertSame(AbstractSorter.ComparableComparator.INSTANCE, sorter.getOrderBy());

    org.cp.elements.util.sort.annotation.Sortable sortableMetaData = sorter.getSortableMetaData(
      TestSortableWithOverrides.INSTANCE);

    assertSame(sortableMetaData, sorter.configureComparator(sortableMetaData));
    assertSame(AbstractSorter.ComparableComparator.INSTANCE, sorter.getOrderBy());
  }

  @Test(expected = SortException.class)
  public void testConfigureComparatorWithSortableAnnotatedObjectUsingComparatorThrowingException() {
    AbstractSorter sorter = new TestSorter();

    assertTrue(sorter.isCustomComparatorAllowed());
    assertSame(AbstractSorter.ComparableComparator.INSTANCE, sorter.getOrderBy());

    org.cp.elements.util.sort.annotation.Sortable sortableMetaData = sorter.getSortableMetaData(
      TestSortableWithProblem.INSTANCE);

    try {
      sorter.configureComparator(sortableMetaData);
    }
    catch (SortException expected) {
      assertEquals(String.format("Error occurred creating an instance of Comparator class (%1$s) to be used by this Sorter (%2$s)!"
        + " The Comparator class (%1$s) must have a public no-arg constructor!",
          sortableMetaData.orderBy().getName(), sorter.getClass().getName()), expected.getMessage());
      assertTrue(expected.getCause() instanceof IllegalStateException);
      assertEquals("Construction Failed!", expected.getCause().getMessage());

      throw expected;
    }
    finally {
      assertSame(AbstractSorter.ComparableComparator.INSTANCE, sorter.getOrderBy());
    }
  }

  @Test
  public void testAsListWithDefaults() {
    AbstractSorter sorter = new TestSorter();
    List<String> elements = sorter.asList(TestSortableWithDefaults.INSTANCE, sorter.getSortableMetaData(
      TestSortableWithDefaults.INSTANCE));

    assertNotNull(elements);
    assertEquals(ELEMENTS.length, elements.size());
    assertTrue(elements.containsAll(Arrays.asList(ELEMENTS)));
    assertEquals("test", elements.get(0));
  }

  @Test
  public void testAsListWithOverrides() {
    AbstractSorter sorter = new TestSorter();
    List<String> elements = sorter.asList(TestSortableWithOverrides.INSTANCE, sorter.getSortableMetaData(
      TestSortableWithOverrides.INSTANCE));

    assertNotNull(elements);
    assertEquals(ELEMENTS.length, elements.size());
    assertTrue(elements.containsAll(Arrays.asList(ELEMENTS)));
    assertEquals("tested", elements.get(0));
  }

  @Test
  public void testAsListWithNullList() {
    AbstractSorter sorter = new TestSorter();
    List<String> elements = sorter.asList(TestSortableWithProblem.INSTANCE, sorter.getSortableMetaData(
      TestSortableWithProblem.INSTANCE));

    assertNotNull(elements);
    assertTrue(elements.isEmpty());
  }

  @Test(expected = SortException.class)
  public void testAsListWithNonSortableAnnotatedObject() {
    try {
      AbstractSorter sorter = new TestSorter();
      sorter.asList(new Object(), sorter.getSortableMetaData(TestSortableWithDefaults.INSTANCE));
    }
    catch (SortException expected) {
      assertEquals("Error occurred getting the list of elements to sort from the (asList) method on object of type (java.lang.Object)!",
        expected.getMessage());
      assertTrue(expected.getCause() instanceof NoSuchMethodException);
      throw expected;
    }
  }

  @Test
  public void testSwap() {
    List<String> elements = new ArrayList<>(Arrays.asList("zero", "one", "two", "three"));

    assertEquals("one", elements.get(1));
    assertEquals("two", elements.get(2));

    new TestSorter().swap(elements, 1, 2);

    assertEquals("two", elements.get(1));
    assertEquals("one", elements.get(2));
  }

  @Test
  public void testSortUsingSorterConcurrently() throws Throwable {
    TestFramework.runOnce(new UseSorterConcurrentlyMultithreadedTestCase());
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testComparableComparatorCompare() {
    assertEquals(0, AbstractSorter.ComparableComparator.INSTANCE.compare("test", "test"));
    assertPositive(AbstractSorter.ComparableComparator.INSTANCE.compare("testing", "test"));
    assertNegative(AbstractSorter.ComparableComparator.INSTANCE.compare("test", "tested"));
  }

  @Test
  public void testComparatorHolderGetIsSetSetAndUnset() {
    Comparator<?> mockComparator = mockContext.mock(Comparator.class, "testComparatorHolderGetIsSetSetAndUnset");

    assertNull(AbstractSorter.ComparatorHolder.get());
    assertFalse(AbstractSorter.ComparatorHolder.isSet());

    AbstractSorter.ComparatorHolder.set(mockComparator);

    assertSame(mockComparator, AbstractSorter.ComparatorHolder.get());
    assertTrue(AbstractSorter.ComparatorHolder.isSet());

    AbstractSorter.ComparatorHolder.unset();

    assertNull(AbstractSorter.ComparatorHolder.get());
    assertFalse(AbstractSorter.ComparatorHolder.isSet());
  }

  @Test
  public void testSortableArrayListGetSetAndSize() {
    String[] elements = { "test", "testing", "tested" };
    AbstractSorter.SortableArrayList<String> list = new AbstractSorter.SortableArrayList<>(elements);

    assertEquals(elements.length, list.size());
    assertEquals("test", list.get(0));
    assertEquals("testing", list.get(1));
    assertEquals("tested", list.get(2));
    assertEquals("testing", list.set(1, "tester"));
    assertEquals("tester", list.get(1));
    assertEquals(elements.length, list.size());
  }

  protected static class TestComparator implements Comparator<String> {

    @Override
    public int compare(final String value1, final String value2) {
      return value1.compareToIgnoreCase(value2);
    }
  }

  public static class TestProblemComparator implements Comparator<String> {

    public TestProblemComparator() {
      throw new IllegalStateException("Construction Failed!");
    }

    @Override
    public int compare(final String value1, final String value2) {
      return value1.compareTo(value2);
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
      return sorted;
    }

    @Override
    public <E> List<E> sort(final List<E> elements) {
      sorted = true;
      return elements;
    }
  }

  protected static final class UseSorterConcurrentlyMultithreadedTestCase extends MultithreadedTestCase {

    private AbstractSorter sorter;

    @Override
    public void initialize() {
      super.initialize();

      sorter = new AbstractSorter() {
        @Override public <E> List<E> sort(final List<E> elements) {
          if ("Thread One".equalsIgnoreCase(Thread.currentThread().getName())) {
            assertNotSame(ComparableComparator.INSTANCE, getOrderBy());
            waitForTick(2);
          }

          Collections.sort(elements, getOrderBy());

          return elements;
        }
      };

      sorter.setCustomComparatorAllowed(true);

      assertSame(AbstractSorter.ComparableComparator.INSTANCE, sorter.getOrderBy());
    }

    public void thread1() {
      Thread.currentThread().setName("Thread One");

      assertTick(0);
      assertTrue(sorter.isCustomComparatorAllowed());
      assertSame(AbstractSorter.ComparableComparator.INSTANCE, sorter.getOrderBy());

      Sortable<String> sortable = sorter.sort(new Sortable<String>() {
        private final List<String> elements = new ArrayList<>(Arrays.asList(ELEMENTS));

        @Override public List<String> asList() {
          return elements;
        }

        @Override public Comparator<String> getOrderBy() {
          return (value1, value2) -> value2.compareTo(value1);
        }
      });

      assertNotNull(sortable);

      List<String> elements = sortable.asList();

      assertNotNull(elements);
      assertFalse(elements.isEmpty());
      assertEquals(3, elements.size());
      assertEquals("testing", elements.get(0));
      assertEquals("tested", elements.get(1));
      assertEquals("test", elements.get(2));
    }

    public void thread2() {
      Thread.currentThread().setName("Thread Two");

      waitForTick(1);
      assertTrue(sorter.isCustomComparatorAllowed());
      assertSame(AbstractSorter.ComparableComparator.INSTANCE, sorter.getOrderBy());

      Sortable<String> sortable = sorter.sort(new Sortable<String>() {
        private final List<String> elements = new ArrayList<>(Arrays.asList(ELEMENTS));

        @Override public List<String> asList() {
          return elements;
        }

        @Override public Comparator<String> getOrderBy() {
          return null;
        }
      });

      assertNotNull(sortable);

      List<String> elements = sortable.asList();

      assertNotNull(elements);
      assertFalse(elements.isEmpty());
      assertEquals(3, elements.size());
      assertEquals("test", elements.get(0));
      assertEquals("tested", elements.get(1));
      assertEquals("testing", elements.get(2));
    }

    @Override
    public void finish() {
      super.finish();
      sorter = null;
    }
  }
}
