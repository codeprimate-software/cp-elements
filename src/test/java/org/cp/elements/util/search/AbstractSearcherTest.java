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

package org.cp.elements.util.search;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.cp.elements.lang.NumberUtils;
import org.cp.elements.lang.reflect.MethodNotFoundException;
import org.cp.elements.test.AbstractMockingTestSuite;
import org.jmock.Expectations;
import org.junit.Test;

import edu.umd.cs.mtc.MultithreadedTestCase;
import edu.umd.cs.mtc.TestFramework;

/**
 * The AbstractSearcherTest class is a test suite of test cases testing the contract and functionality of the
 * AbstractSearcher class.
 *
 * @author John J. Blum
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.cp.elements.util.search.AbstractSearcher
 * @see org.cp.elements.util.search.Searcher
 * @see org.junit.Test
 * @see edu.umd.cs.mtc.MultithreadedTestCase
 * @see edu.umd.cs.mtc.TestFramework
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AbstractSearcherTest extends AbstractMockingTestSuite {

  private static final String[] ANIMALS = {
    "aardvark", "baboon", "cat", "dog", "elephant", "ferret", "giraffe", "horse", "iguana", "jackal", "kangaroo",
    "lama", "mouse", "newt", "octopus", "porcupine", "quail", "rabbit", "snake", "turtle", "urchin", "viper", "walrus",
    "x", "yack", "zebra"
  };

  @Test
  public void testSetAndGetCustomMatcherAllowed() {
    AbstractSearcher searcher = new TestSearcher();

    assertNotNull(searcher);
    assertEquals(AbstractSearcher.DEFAULT_CUSTOM_MATCHER_ALLOWED, searcher.isCustomMatcherAllowed());

    searcher.setCustomMatcherAllowed(false);

    assertFalse(searcher.isCustomMatcherAllowed());

    searcher.setCustomMatcherAllowed(true);

    assertTrue(searcher.isCustomMatcherAllowed());
  }

  @Test
  public void testSetAndGetMatcher() {
    AbstractSearcher searcher = new TestSearcher();
    Matcher mockMatcher = mockContext.mock(Matcher.class);

    searcher.setMatcher(mockMatcher);

    assertSame(mockMatcher, searcher.getMatcher());
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSetMatcherWithNull() {
    try {
      new TestSearcher().setMatcher(null);
    }
    catch (IllegalArgumentException expected) {
      assertEquals(String.format("The Matcher used to match elements in the collection during the search operation by this Searcher (%1$s) cannot be null!",
        TestSearcher.class.getName()), expected.getMessage());
      throw expected;
    }
  }

  @Test(expected = IllegalStateException.class)
  public void testGetMatcherWhenNull() {
    try {
      new TestSearcher().getMatcher();
    }
    catch (IllegalStateException expected) {
      assertEquals(String.format("A reference to a Matcher used by this Searcher (%1$s) for searching and matching elements in the collection was not properly configured!",
        TestSearcher.class.getName()), expected.getMessage());
      throw expected;
    }
  }

  @Test
  public void testSearchArray() {
    AbstractSearcher searcher = new TestSearcher();

    searcher.setMatcher(new AbstractMatcher<String>() {
      @Override public int match(final String value) {
        return "testing".compareTo(value);
      }
    });

    assertNotNull(searcher.getMatcher());
    assertEquals("testing", searcher.search("test", "testing", "tested"));
  }

  @Test
  public void testSearchObjectImplementingSearchable() {
    final Searchable<?> mockSearchable = mockContext.mock(Searchable.class, "testSearchObjectImplementingSearchable");

    mockContext.checking(new Expectations() {{
      oneOf(mockSearchable).asList();
      will(returnValue(Arrays.asList(ANIMALS)));
      oneOf(mockSearchable).getMatcher();
      will(returnValue(null));
    }});

    AbstractSearcher searcher = new TestSearcher();

    searcher.setCustomMatcherAllowed(true);

    searcher.setMatcher(new AbstractMatcher<String>() {
      @Override public int match(final String value) {
        return ANIMALS[12].compareTo(value);
      }
    });

    assertTrue(searcher.isCustomMatcherAllowed());
    assertNotNull(searcher.getMatcher());
    assertEquals(ANIMALS[12], searcher.search(mockSearchable));
  }

  @Test
  public void testSearchObjectImplementingSearchableHavingCustomMatcher() {
    final Searchable<?> mockSearchable = mockContext.mock(Searchable.class,
      "testSearchObjectImplementingSearchableHavingCustomMatcher");

    mockContext.checking(new Expectations() {{
      exactly(2).of(mockSearchable).asList();
      will(returnValue(Arrays.asList(ANIMALS)));
      oneOf(mockSearchable).getMatcher();
      will(returnValue(new AbstractMatcher<String>() {
        @Override public int match(final String value) {
          return ANIMALS[16].compareTo(value);
        }
      }));
    }});

    AbstractSearcher searcher = new TestSearcher();

    searcher.setCustomMatcherAllowed(true);

    searcher.setMatcher(new AbstractMatcher<String>() {
      @Override public int match(final String value) {
        return ANIMALS[12].compareTo(value);
      }
    });

    assertTrue(searcher.isCustomMatcherAllowed());
    assertNotNull(searcher.getMatcher());
    assertEquals(ANIMALS[16], searcher.search(mockSearchable));

    searcher.setCustomMatcherAllowed(false);

    assertFalse(searcher.isCustomMatcherAllowed());
    assertNotNull(searcher.getMatcher());
    assertEquals(ANIMALS[12], searcher.search(mockSearchable));
  }

  @Test
  public void testSearchSearchableAnnotatedObject() {
    AbstractSearcher searcher = new TestSearcher();

    searcher.setCustomMatcherAllowed(true);

    searcher.setMatcher(new AbstractMatcher<String>() {
      @Override public int match(final String value) {
        return ANIMALS[25].compareTo(value);
      }
    });

    assertTrue(searcher.isCustomMatcherAllowed());
    assertNotNull(searcher.getMatcher());
    assertEquals(ANIMALS[25], searcher.search(TestSearchableWithDefaults.INSTANCE));
  }

  @Test
  public void testSearchSearchableAnnotatedObjectHavingCustomMatcher() {
    AbstractSearcher searcher = new TestSearcher();

    searcher.setCustomMatcherAllowed(true);

    searcher.setMatcher(new AbstractMatcher<String>() {
      @Override public int match(final String value) {
        return ANIMALS[25].compareTo(value);
      }
    });

    assertTrue(searcher.isCustomMatcherAllowed());
    assertNotNull(searcher.getMatcher());
    assertEquals("baboon", searcher.search(TestSearchableWithOverrides.INSTANCE));

    searcher.setCustomMatcherAllowed(false);

    assertFalse(searcher.isCustomMatcherAllowed());
    assertNotNull(searcher.getMatcher());
    assertEquals(ANIMALS[25], searcher.search(TestSearchableWithOverrides.INSTANCE));
  }

  @Test
  public void testSearchReturningNoMatch() {
    AbstractSearcher searcher = new TestSearcher();

    searcher.setMatcher(new AbstractMatcher() {
      @Override public int match(final Object obj) {
        return -1;
      }
    });

    assertNotNull(searcher.getMatcher());
    assertNull(searcher.search(ANIMALS));
  }

  @Test
  public void testSearchForAllInArray() {
    AbstractSearcher searcher = new TestSearcher();

    searcher.setMatcher(new AbstractMatcher() {
      int count = 0;
      @Override public int match(final Object obj) {
        return (NumberUtils.isEven(++count) ? 0 : -1);
      }
    });

    assertNotNull(searcher.getMatcher());

    Iterable<String> searchResults = searcher.searchForAll(ANIMALS);

    assertNotNull(searchResults);

    int count = 0;
    int index = 1;

    for (String result : searchResults) {
      assertEquals(ANIMALS[index], result);
      index += 2;
      count++;
    }

    assertEquals(ANIMALS.length / 2, count);
  }

  @Test
  public void testSearchForAllInCollection() {
    AbstractSearcher searcher = new TestSearcher();

    searcher.setMatcher(new AbstractMatcher() {
      int count = 0;
      @Override public int match(final Object obj) {
        return (NumberUtils.isOdd(++count) ? 0 : -1);
      }
    });

    assertNotNull(searcher.getMatcher());

    Iterable<String> searchResults = searcher.searchForAll(Arrays.asList(ANIMALS));

    assertNotNull(searchResults);

    int count = 0;
    int index = 0;

    for (String result : searchResults) {
      assertEquals(ANIMALS[index], result);
      index += 2;
      count++;
    }

    assertEquals(ANIMALS.length / 2, count);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSearchForAllInCollectionWithNull() {
    try {
      new TestSearcher().searchForAll((Collection<?>) null);
    }
    catch (IllegalArgumentException expected) {
      assertEquals("The collection to search cannot be null!", expected.getMessage());
      throw expected;
    }
  }

  @Test
  public void testSearchForAllInObjectImplementingSearchable() {
    final Searchable<?> mockSearchable = mockContext.mock(Searchable.class,
      "testSearchForAllInObjectImplementingSearchable");

    mockContext.checking(new Expectations() {{
      oneOf(mockSearchable).asList();
      will(returnValue(Arrays.asList(ANIMALS)));
      oneOf(mockSearchable).getMatcher();
      will(returnValue(null));
    }});

    AbstractSearcher searcher = new TestSearcher();

    searcher.setCustomMatcherAllowed(true);

    searcher.setMatcher(new AbstractMatcher() {
      @Override public int match(final Object obj) {
        return 0;
      }
    });

    assertTrue(searcher.isCustomMatcherAllowed());
    assertNotNull(searcher.getMatcher());

    Iterable searchResults = searcher.searchForAll(mockSearchable);

    assertNotNull(searchResults);

    int index = 0;

    for (Object result : searchResults) {
      assertEquals(ANIMALS[index++], result);
    }

    assertEquals(ANIMALS.length, index);
  }

  @Test
  public void testSearchForAllInObjectImplementingSearchableHavingCustomMatcher() {
    final Searchable<?> mockSearchable = mockContext.mock(Searchable.class,
      "testSearchForAllInObjectImplementingSearchable");

    mockContext.checking(new Expectations() {{
      exactly(2).of(mockSearchable).asList();
      will(returnValue(Arrays.asList(ANIMALS)));
      oneOf(mockSearchable).getMatcher();
      will(returnValue(new TestMatcher()));
    }});

    AbstractSearcher searcher = new TestSearcher();

    searcher.setCustomMatcherAllowed(true);

    searcher.setMatcher(new AbstractMatcher() {
      @Override public int match(final Object obj) {
        return 0;
      }
    });

    assertTrue(searcher.isCustomMatcherAllowed());
    assertNotNull(searcher.getMatcher());

    Iterable searchResults = searcher.searchForAll(mockSearchable);

    assertNotNull(searchResults);

    int count = 0;
    int index = 2;

    for (Object result : searchResults) {
      assertEquals(ANIMALS[index - 1], result);
      index *= 2;
      count++;
    }

    assertEquals(4, count);

    searcher.setCustomMatcherAllowed(false);

    assertFalse(searcher.isCustomMatcherAllowed());
    assertNotNull(searcher.getMatcher());

    searchResults = searcher.searchForAll(mockSearchable);

    assertNotNull(searchResults);

    count = index = 0;

    for (Object result : searchResults) {
      assertEquals(ANIMALS[index++], result);
      count++;
    }

    assertEquals(ANIMALS.length, count);
  }

  @Test
  public void testSearchForAllInSearchableAnnotatedObject() {
    AbstractSearcher searcher = new TestSearcher();

    searcher.setCustomMatcherAllowed(true);

    searcher.setMatcher(new AbstractMatcher() {
      @Override public int match(final Object obj) {
        return 0;
      }
    });

    assertTrue(searcher.isCustomMatcherAllowed());
    assertNotNull(searcher.getMatcher());

    Iterable<String> searchResults = searcher.searchForAll(TestSearchableWithDefaults.INSTANCE);

    assertNotNull(searchResults);

    int count = 0;
    int index = 0;

    for (String result : searchResults) {
      assertEquals(ANIMALS[index++], result);
      count++;
    }

    assertEquals(ANIMALS.length, count);
  }

  @Test
  public void testSearchForAllInSearchableAnnotatedObjectHavingCustomMatcher() {
    AbstractSearcher searcher = new TestSearcher();

    searcher.setCustomMatcherAllowed(true);

    searcher.setMatcher(new AbstractMatcher() {
      @Override public int match(final Object obj) {
        return 0;
      }
    });

    assertTrue(searcher.isCustomMatcherAllowed());
    assertNotNull(searcher.getMatcher());

    Iterable<String> searchResults = searcher.searchForAll(TestSearchableWithOverrides.INSTANCE);

    assertNotNull(searchResults);

    int count = 0;
    int index = 2;

    for (String result : searchResults) {
      assertEquals(ANIMALS[index - 1], result);
      index *= 2;
      count++;
    }

    assertEquals(4, count);

    searcher.setCustomMatcherAllowed(false);

    assertFalse(searcher.isCustomMatcherAllowed());
    assertNotNull(searcher.getMatcher());

    searchResults = searcher.searchForAll(TestSearchableWithOverrides.INSTANCE);

    assertNotNull(searchResults);

    count = index = 0;

    for (String result : searchResults) {
      assertEquals(ANIMALS[index++], result);
      count++;
    }

    assertEquals(ANIMALS.length, count);
  }

  @Test
  public void testSearchForAllReturningNoMatches() {
    AbstractSearcher searcher = new TestSearcher();

    searcher.setMatcher(new AbstractMatcher() {
      @Override public int match(final Object obj) {
        return -1;
      }
    });

    assertNotNull(searcher.getMatcher());

    Iterable<String> searchResults = searcher.searchForAll("test", "testing", "tested");

    assertNotNull(searchResults);
    assertFalse(searchResults.iterator().hasNext());
  }

  @Test
  public void testSearchForAllUsingSearcherConcurrently() throws Throwable {
    TestFramework.runOnce(new UseSearcherConcurrentlyMultithreadedTestCase());
  }

  @Test
  public void testGetSearchableMetaData() {
    AbstractSearcher searcher = new TestSearcher();

    org.cp.elements.util.search.annotation.Searchable searchableMetaData = searcher.getSearchableMetaData(
      TestSearchableWithDefaults.INSTANCE);

    assertNotNull(searchableMetaData);
    assertEquals("asList", searchableMetaData.listMethod());
    assertEquals(Matcher.class, searchableMetaData.matcher());

    searchableMetaData = searcher.getSearchableMetaData(TestSearchableWithOverrides.INSTANCE);

    assertNotNull(searchableMetaData);
    assertEquals("toCollection", searchableMetaData.listMethod());
    assertEquals(TestMatcher.class, searchableMetaData.matcher());
  }

  @Test(expected = SearchException.class)
  public void testGetSearchableMetaDataWithNonSearchableAnnotatedObject() {
    try {
      new TestSearcher().getSearchableMetaData(new Object());
    }
    catch (SearchException expected) {
      assertEquals(String.format("To search an object of type (java.lang.Object), the class must be annotated with the (%1$s) annotation!",
        org.cp.elements.util.search.annotation.Searchable.class.getName()), expected.getMessage());
      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetSearchableMetaDataWithNull() {
    try {
      new TestSearcher().getSearchableMetaData(null);
    }
    catch (IllegalArgumentException expected) {
      assertEquals("The object to search cannot be null!", expected.getMessage());
      throw expected;
    }
  }

  @Test
  public void testConfigureMatcherUsingSearchable() {
    Matcher<?> mockMatcher = mockContext.mock(Matcher.class, "testConfigureMatcherUsingSearchable.Matcher");

    final Searchable<?> mockSearchable = mockContext.mock(Searchable.class, "testConfigureMatcherUsingSearchable.Searchable");

    mockContext.checking(new Expectations() {{
      oneOf(mockSearchable).getMatcher();
      will(returnValue(new TestMatcher()));
    }});

    AbstractSearcher searcher = new TestSearcher();

    searcher.setCustomMatcherAllowed(true);
    searcher.setMatcher(mockMatcher);

    assertTrue(searcher.isCustomMatcherAllowed());
    assertSame(mockMatcher, searcher.getMatcher());
    assertSame(mockSearchable, searcher.configureMatcher(mockSearchable));
    assertTrue(searcher.getMatcher() instanceof TestMatcher);

    AbstractSearcher.MatcherHolder.unset();
    searcher.setCustomMatcherAllowed(false);

    assertFalse(searcher.isCustomMatcherAllowed());
    assertSame(mockMatcher, searcher.getMatcher());
    assertSame(mockSearchable, searcher.configureMatcher(mockSearchable));
    assertSame(mockMatcher, searcher.getMatcher());

    mockContext.checking(new Expectations() {{
      oneOf(mockSearchable).getMatcher();
      will(returnValue(null));
    }});

    searcher.setCustomMatcherAllowed(true);

    assertTrue(searcher.isCustomMatcherAllowed());
    assertSame(mockSearchable, searcher.configureMatcher(mockSearchable));
    assertSame(mockMatcher, searcher.getMatcher());
  }

  @Test
  public void testConfigureMatcherUsingSearchableAnnotatedObject() {
    Matcher<?> mockMatcher = mockContext.mock(Matcher.class, "testConfigureMatcherUsingSearchableAnnotatedObject");
    AbstractSearcher searcher = new TestSearcher();

    searcher.setCustomMatcherAllowed(true);
    searcher.setMatcher(mockMatcher);

    assertTrue(searcher.isCustomMatcherAllowed());
    assertSame(mockMatcher, searcher.getMatcher());

    org.cp.elements.util.search.annotation.Searchable searchableMetaData = searcher.getSearchableMetaData(
      TestSearchableWithOverrides.INSTANCE);

    assertSame(searchableMetaData, searcher.configureMatcher(searchableMetaData));
    assertTrue(searcher.getMatcher() instanceof TestMatcher);

    AbstractSearcher.MatcherHolder.unset();
    searcher.setCustomMatcherAllowed(false);

    assertFalse(searcher.isCustomMatcherAllowed());
    assertSame(mockMatcher, searcher.getMatcher());
    assertSame(searchableMetaData, searcher.configureMatcher(searchableMetaData));
    assertSame(mockMatcher, searcher.getMatcher());

    searchableMetaData = searcher.getSearchableMetaData(TestSearchableWithDefaults.INSTANCE);
    searcher.setCustomMatcherAllowed(true);

    assertTrue(searcher.isCustomMatcherAllowed());
    assertSame(searchableMetaData, searcher.configureMatcher(searchableMetaData));
    assertSame(mockMatcher, searcher.getMatcher());
  }

  @Test(expected = SearchException.class)
  public void testConfigureMatcherUsingSearchableAnnotatedObjectThrowsException() {
    try {
      AbstractSearcher searcher = new TestSearcher();

      searcher.setCustomMatcherAllowed(true);

      assertTrue(searcher.isCustomMatcherAllowed());

      searcher.configureMatcher(searcher.getSearchableMetaData(new TestSearchableWithProblem<>()));
    }
    catch (SearchException expected) {
      assertEquals(String.format(
        "Error occurred creating an instance of Matcher class (%1$s) to be used by this Searcher (%2$s)!"
          + " The Matcher class (%1$s) must have a public no-arg constructor!", TestProblemMatcher.class.getName(),
            TestSearcher.class.getName()), expected.getMessage());
      assertTrue(expected.getCause() instanceof IllegalStateException);
      assertEquals("Construction Failed!", expected.getCause().getMessage());
      throw expected;
    }
  }

  @Test
  public void testAsList() {
    AbstractSearcher searcher = new TestSearcher();

    List<String> animalList = searcher.asList(TestSearchableWithOverrides.INSTANCE, searcher.getSearchableMetaData(
      TestSearchableWithOverrides.INSTANCE));

    assertNotNull(animalList);
    assertEquals(ANIMALS.length, animalList.size());
    assertTrue(animalList.containsAll(Arrays.asList(ANIMALS)));
  }

  @Test(expected = SearchException.class)
  public void testAsListWithNonSearchableAnnotatedObject() {
    try {
      AbstractSearcher searcher = new TestSearcher();
      searcher.asList(new Object(), searcher.getSearchableMetaData(TestSearchableWithDefaults.INSTANCE));
    }
    catch (SearchException expected) {
      assertEquals("Error occurred getting the list of elements to search from the (asList) method on object of type (java.lang.Object)!",
        expected.getMessage());
      assertTrue(expected.getCause() instanceof MethodNotFoundException);
      throw expected;
    }
  }

  @Test
  public void testAsListWithSearchableAnnotatedObjectReturningNullList() {
    AbstractSearcher searcher = new TestSearcher();

    TestSearchableWithDefaults<Object> testSearchable = new TestSearchableWithDefaults<>(null);

    List<String> actualList = searcher.asList(testSearchable, searcher.getSearchableMetaData(testSearchable));

    assertNotNull(actualList);
    assertTrue(actualList.isEmpty());
  }

  @Test
  public void testMatcherHolderGetIsSetSetAndUnset() {
    Matcher<?> mockMatcher = mockContext.mock(Matcher.class, "testMatcherHolderGetIsSetSetAndUnset");

    assertNull(AbstractSearcher.MatcherHolder.get());
    assertFalse(AbstractSearcher.MatcherHolder.isSet());

    AbstractSearcher.MatcherHolder.set(mockMatcher);

    assertSame(mockMatcher, AbstractSearcher.MatcherHolder.get());
    assertTrue(AbstractSearcher.MatcherHolder.isSet());

    AbstractSearcher.MatcherHolder.unset();

    assertNull(AbstractSearcher.MatcherHolder.get());
    assertFalse(AbstractSearcher.MatcherHolder.isSet());
  }

  public static class TestMatcher extends AbstractMatcher<Object> {

    private int count = 0;
    private int powersOfTwo = 2;

    @Override
    public final int match(final Object obj) {
      if (++count == powersOfTwo) {
        powersOfTwo *= 2;
        return 0;
      }

      return -1;
    }
  }

  public static class TestProblemMatcher extends TestMatcher {

    public TestProblemMatcher() {
      throw new IllegalStateException("Construction Failed!");
    }
  }

  @org.cp.elements.util.search.annotation.Searchable
  protected static final class TestSearchableWithDefaults<E> {

    protected static final TestSearchableWithDefaults<String> INSTANCE = new TestSearchableWithDefaults<>(
      Arrays.asList(ANIMALS));

    private final List<E> list;

    protected TestSearchableWithDefaults(final List<E> list) {
      this.list = list;
    }

    public List<E> asList() {
      return this.list;
    }
  }

  @org.cp.elements.util.search.annotation.Searchable(listMethod = "toCollection", matcher = TestMatcher.class)
  protected static final class TestSearchableWithOverrides<E> {

    protected static final TestSearchableWithOverrides<String> INSTANCE = new TestSearchableWithOverrides<>(
      Arrays.asList(ANIMALS));

    private final List<E> list;

    protected TestSearchableWithOverrides(final List<E> list) {
      this.list = list;
    }

    public List<E> toCollection() {
      return this.list;
    }
  }

  @org.cp.elements.util.search.annotation.Searchable(matcher = TestProblemMatcher.class)
  protected static final class TestSearchableWithProblem<E> {

    public List<E> asList() {
      return Collections.emptyList();
    }
  }

  protected static class TestSearcher extends AbstractSearcher {

    @Override
    public <E> E search(final Collection<E> collection) {
      for (E element : collection) {
        if (getMatcher().accept(element)) {
          return element;
        }
      }

      return null;
    }
  }

  protected static final class UseSearcherConcurrentlyMultithreadedTestCase extends MultithreadedTestCase {

    private volatile AbstractSearcher searcher;

    @Override
    public void initialize() {
      super.initialize();

      searcher = new TestSearcher() {
        @Override public <E> Iterable<E> searchForAll(final Collection<E> collection) {
          Iterable<E> searchResults = super.searchForAll(collection);
          if ("Thread One".equals(Thread.currentThread().getName())) {
            assertTrue(getMatcher() instanceof TestMatcher);
            waitForTick(2);
          }
          else {
            assertFalse(getMatcher() instanceof TestMatcher);
          }
          return searchResults;
        }
      };

      searcher.setCustomMatcherAllowed(true);

      searcher.setMatcher(new AbstractMatcher<String>() {
        @Override public int match(final String value) {
          return 0;
        }
      });
    }

    public void thread1() {
      assertTick(0);

      Thread.currentThread().setName("Thread One");

      assertTrue(searcher.isCustomMatcherAllowed());
      assertNotNull(searcher.getMatcher());
      assertFalse(searcher.getMatcher() instanceof TestMatcher);

      Iterable<String> searchResults = searcher.searchForAll(TestSearchableWithOverrides.INSTANCE);

      assertTick(2);
      assertNotNull(searchResults);

      int count = 0;
      int index = 2;

      for (String result : searchResults) {
        assertEquals(ANIMALS[index - 1], result);
        index *= 2;
        count++;
      }

      assertEquals(4, count);
    }

    public void thread2() {
      waitForTick(1);

      Thread.currentThread().setName("Thread Two");

      assertTrue(searcher.isCustomMatcherAllowed());
      assertNotNull(searcher.getMatcher());
      assertFalse(searcher.getMatcher() instanceof TestMatcher);

      Iterable<String> searchResults = searcher.searchForAll(TestSearchableWithDefaults.INSTANCE);

      assertNotNull(searchResults);

      int count = 0;
      int index = 0;

      for (String result : searchResults) {
        assertEquals(ANIMALS[index++], result);
        count++;
      }

      assertEquals(ANIMALS.length, count);
    }

    @Override
    public void finish() {
      super.finish();
      searcher = null;
    }
  }
}
