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
package org.cp.elements.util.search;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.assertThatIllegalStateException;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.NumberUtils;
import org.cp.elements.lang.ThrowableAssertions;
import org.cp.elements.lang.reflect.MethodNotFoundException;

import edu.umd.cs.mtc.MultithreadedTestCase;
import edu.umd.cs.mtc.TestFramework;

/**
 * Unit Tests for {@link AbstractSearcher}.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.util.search.AbstractSearcher
 * @see org.cp.elements.util.search.Searcher
 * @see edu.umd.cs.mtc.MultithreadedTestCase
 * @see edu.umd.cs.mtc.TestFramework
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AbstractSearcherTests {

  private static final String[] ANIMALS = {
    "aardvark", "baboon", "cat", "dog", "elephant", "ferret", "giraffe", "horse", "iguana", "jackal", "kangaroo",
    "lama", "mouse", "newt", "octopus", "porcupine", "quail", "rabbit", "snake", "turtle", "urchin", "viper", "walrus",
    "x", "yack", "zebra"
  };

  @Test
  public void setAndGetCustomMatcherAllowed() {

    AbstractSearcher searcher = new TestSearcher();

    assertThat(searcher).isNotNull();
    assertThat(searcher.isCustomMatcherAllowed()).isEqualTo(AbstractSearcher.DEFAULT_CUSTOM_MATCHER_ALLOWED);

    searcher.setCustomMatcherAllowed(false);

    assertThat(searcher.isCustomMatcherAllowed()).isFalse();

    searcher.setCustomMatcherAllowed(true);

    assertThat(searcher.isCustomMatcherAllowed()).isTrue();
  }

  @Test
  public void setAndGetMatcher() {

    AbstractSearcher searcher = new TestSearcher();
    Matcher<?> mockMatcher = mock(Matcher.class);

    searcher.setMatcher(mockMatcher);

    assertThat(searcher.getMatcher()).isSameAs(mockMatcher);
  }

  @Test
  public void setMatcherWithNull() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new TestSearcher().setMatcher(null))
      .withMessage("The Matcher used to match elements in the collection during the search operation by this Searcher (%1$s) cannot be null!",
        TestSearcher.class.getName())
      .withNoCause();
  }

  @Test
  public void getMatcherWhenNull() {

    assertThatIllegalStateException()
      .isThrownBy(() -> new TestSearcher().getMatcher())
      .withMessage("A reference to a Matcher used by this Searcher (%1$s) for searching and matching elements in the collection was not properly configured!",
        TestSearcher.class.getName())
      .withNoCause();
  }

  @Test
  public void searchArray() {

    AbstractSearcher searcher = new TestSearcher();

    searcher.setMatcher(new AbstractMatcher<String>() {
      @Override public int match(final String value) {
        return "testing".compareTo(value);
      }
    });

    assertThat(searcher.getMatcher()).isNotNull();
    assertThat(searcher.search("test", "testing", "tested")).isEqualTo("testing");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void searchObjectImplementingSearchable() {

    Searchable<Object> mockSearchable = mock(Searchable.class);

    when(mockSearchable.asList()).thenReturn(Arrays.asList(ANIMALS));
    when(mockSearchable.getMatcher()).thenReturn(null);

    AbstractSearcher searcher = new TestSearcher();

    searcher.setCustomMatcherAllowed(true);

    searcher.setMatcher(new AbstractMatcher<String>() {
      @Override public int match(final String value) {
        return ANIMALS[12].compareTo(value);
      }
    });

    assertThat(searcher.isCustomMatcherAllowed()).isTrue();
    assertThat(searcher.getMatcher()).isNotNull();
    assertThat(searcher.search(mockSearchable)).isEqualTo(ANIMALS[12]);

    verify(mockSearchable, times(1)).asList();
    verify(mockSearchable, times(1)).getMatcher();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void searchObjectImplementingSearchableHavingCustomMatcher() {

    Searchable<String> mockSearchable = mock(Searchable.class);

    Matcher<String> matcher = new AbstractMatcher<>() {

      @Override
      public int match(String value) {
        return ANIMALS[16].compareTo(value);
      }
    };

    when(mockSearchable.asList()).thenReturn(Arrays.asList(ANIMALS));
    when(mockSearchable.getMatcher()).thenReturn(matcher);

    AbstractSearcher searcher = new TestSearcher();

    searcher.setCustomMatcherAllowed(true);

    searcher.setMatcher(new AbstractMatcher<String>() {
      @Override public int match(final String value) {
        return ANIMALS[12].compareTo(value);
      }
    });

    assertThat(searcher.isCustomMatcherAllowed()).isTrue();
    assertThat(searcher.getMatcher()).isNotNull();
    assertThat(searcher.search(mockSearchable)).isEqualTo(ANIMALS[16]);

    searcher.setCustomMatcherAllowed(false);

    assertThat(searcher.isCustomMatcherAllowed()).isFalse();
    assertThat(searcher.getMatcher()).isNotNull();
    assertThat(searcher.search(mockSearchable)).isEqualTo(ANIMALS[12]);

    verify(mockSearchable, times(2)).asList();
    verify(mockSearchable, times(1)).getMatcher();
  }

  @Test
  public void searchSearchableAnnotatedObject() {

    AbstractSearcher searcher = new TestSearcher();

    searcher.setCustomMatcherAllowed(true);

    searcher.setMatcher(new AbstractMatcher<String>() {

      @Override
      public int match(final String value) {
        return ANIMALS[25].compareTo(value);
      }
    });

    assertThat(searcher.isCustomMatcherAllowed()).isTrue();
    assertThat(searcher.getMatcher()).isNotNull();
    assertThat(searcher.<Object>search(TestSearchableWithDefaults.INSTANCE)).isEqualTo(ANIMALS[25]);
  }

  @Test
  public void searchSearchableAnnotatedObjectHavingCustomMatcher() {
    AbstractSearcher searcher = new TestSearcher();

    searcher.setCustomMatcherAllowed(true);

    searcher.setMatcher(new AbstractMatcher<String>() {

      @Override
      public int match(final String value) {
        return ANIMALS[25].compareTo(value);
      }
    });

    assertThat(searcher.isCustomMatcherAllowed()).isTrue();
    assertThat(searcher.getMatcher()).isNotNull();
    assertThat(searcher.<Object>search(TestSearchableWithOverrides.INSTANCE)).isEqualTo("baboon");

    searcher.setCustomMatcherAllowed(false);

    assertThat(searcher.isCustomMatcherAllowed()).isFalse();
    assertThat(searcher.getMatcher()).isNotNull();
    assertThat(searcher.<Object>search(TestSearchableWithOverrides.INSTANCE)).isEqualTo(ANIMALS[25]);
  }

  @Test
  public void searchReturningNoMatch() {
    AbstractSearcher searcher = new TestSearcher();

    searcher.setMatcher(new AbstractMatcher<>() {

      @Override
      public int match(final Object obj) {
        return -1;
      }
    });

    assertThat(searcher.getMatcher()).isNotNull();
    assertThat(searcher.search(ANIMALS)).isNull();
  }

  @Test
  public void searchForAllInArray() {
    AbstractSearcher searcher = new TestSearcher();

    searcher.setMatcher(new AbstractMatcher<>() {

      int count = 0;

      @Override
      public int match(final Object obj) {
        return (NumberUtils.isEven(++count) ? 0 : -1);
      }
    });

    assertThat(searcher.getMatcher()).isNotNull();

    Iterable<String> searchResults = searcher.searchForAll(ANIMALS);

    assertThat(searchResults).isNotNull();

    int count = 0;
    int index = 1;

    for (String result : searchResults) {
      assertThat(result).isEqualTo(ANIMALS[index]);
      index += 2;
      count++;
    }

    assertThat(count).isEqualTo(ANIMALS.length / 2);
  }

  @Test
  public void searchForAllInCollection() {

    AbstractSearcher searcher = new TestSearcher();

    searcher.setMatcher(new AbstractMatcher<>() {

      int count = 0;

      @Override
      public int match(final Object obj) {
        return (NumberUtils.isOdd(++count) ? 0 : -1);
      }
    });

    assertThat(searcher.getMatcher()).isNotNull();

    Iterable<String> searchResults = searcher.searchForAll(Arrays.asList(ANIMALS));

    assertThat(searchResults).isNotNull();

    int count = 0;
    int index = 0;

    for (String result : searchResults) {
      assertThat(result).isEqualTo(ANIMALS[index]);
      index += 2;
      count++;
    }

    assertThat(count).isEqualTo(ANIMALS.length / 2);
  }

  @Test
  public void searchForAllInCollectionWithNull() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new TestSearcher().searchForAll((Collection<?>) null))
      .withMessage("The collection to search cannot be null!")
      .withNoCause();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void searchForAllInObjectImplementingSearchable() {

    Searchable<Object> mockSearchable = mock(Searchable.class);

    when(mockSearchable.asList()).thenReturn(Arrays.asList(ANIMALS));
    when(mockSearchable.getMatcher()).thenReturn(null);

    AbstractSearcher searcher = new TestSearcher();

    searcher.setCustomMatcherAllowed(true);

    searcher.setMatcher(new AbstractMatcher<>() {

      @Override
      public int match(Object obj) {
        return 0;
      }
    });

    assertThat(searcher.isCustomMatcherAllowed()).isTrue();
    assertThat(searcher.getMatcher()).isNotNull();

    Iterable<?> searchResults = searcher.searchForAll(mockSearchable);

    assertThat(searchResults).isNotNull();

    int index = 0;

    for (Object result : searchResults) {
      assertThat(result).isEqualTo(ANIMALS[index++]);
    }

    assertThat(index).isEqualTo(ANIMALS.length);

    verify(mockSearchable, times(1)).asList();
    verify(mockSearchable, times(1)).getMatcher();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void searchForAllInObjectImplementingSearchableHavingCustomMatcher() {

    Searchable<Object> mockSearchable = mock(Searchable.class);

    Matcher<Object> matcher = new TestMatcher();

    when(mockSearchable.asList()).thenReturn(Arrays.asList(ANIMALS));
    when(mockSearchable.getMatcher()).thenReturn(matcher);

    AbstractSearcher searcher = new TestSearcher();

    searcher.setCustomMatcherAllowed(true);

    searcher.setMatcher(new AbstractMatcher<>() {

      @Override
      public int match(final Object obj) {
        return 0;
      }
    });

    assertThat(searcher.isCustomMatcherAllowed()).isTrue();
    assertThat(searcher.getMatcher()).isNotNull();

    Iterable<?> searchResults = searcher.searchForAll(mockSearchable);

    assertThat(searchResults).isNotNull();

    int count = 0;
    int index = 2;

    for (Object result : searchResults) {
      assertThat(result).isEqualTo(ANIMALS[index - 1]);
      index *= 2;
      count++;
    }

    assertThat(count).isEqualTo(4);

    searcher.setCustomMatcherAllowed(false);

    assertThat(searcher.isCustomMatcherAllowed()).isFalse();
    assertThat(searcher.getMatcher()).isNotNull();

    searchResults = searcher.searchForAll(mockSearchable);

    assertThat(searchResults).isNotNull();

    count = index = 0;

    for (Object result : searchResults) {
      assertThat(result).isEqualTo(ANIMALS[index++]);
      count++;
    }

    assertThat(count).isEqualTo(ANIMALS.length);

    verify(mockSearchable, times(2)).asList();
    verify(mockSearchable, times(1)).getMatcher();
  }

  @Test
  public void searchForAllInSearchableAnnotatedObject() {
    AbstractSearcher searcher = new TestSearcher();

    searcher.setCustomMatcherAllowed(true);

    searcher.setMatcher(new AbstractMatcher<>() {

      @Override
      public int match(final Object obj) {
        return 0;
      }
    });

    assertThat(searcher.isCustomMatcherAllowed()).isTrue();
    assertThat(searcher.getMatcher()).isNotNull();

    Iterable<String> searchResults = searcher.searchForAll(TestSearchableWithDefaults.INSTANCE);

    assertThat(searchResults).isNotNull();

    int count = 0;
    int index = 0;

    for (String result : searchResults) {
      assertThat(result).isEqualTo(ANIMALS[index++]);
      count++;
    }

    assertThat(count).isEqualTo(ANIMALS.length);
  }

  @Test
  public void searchForAllInSearchableAnnotatedObjectHavingCustomMatcher() {

    AbstractSearcher searcher = new TestSearcher();

    searcher.setCustomMatcherAllowed(true);

    searcher.setMatcher(new AbstractMatcher<>() {

      @Override
      public int match(final Object obj) {
        return 0;
      }
    });

    assertThat(searcher.isCustomMatcherAllowed()).isTrue();
    assertThat(searcher.getMatcher()).isNotNull();

    Iterable<String> searchResults = searcher.searchForAll(TestSearchableWithOverrides.INSTANCE);

    assertThat(searchResults).isNotNull();

    int count = 0;
    int index = 2;

    for (String result : searchResults) {
      assertThat(result).isEqualTo(ANIMALS[index - 1]);
      index *= 2;
      count++;
    }

    assertThat(count).isEqualTo(4);

    searcher.setCustomMatcherAllowed(false);

    assertThat(searcher.isCustomMatcherAllowed()).isFalse();
    assertThat(searcher.getMatcher()).isNotNull();

    searchResults = searcher.searchForAll(TestSearchableWithOverrides.INSTANCE);

    assertThat(searchResults).isNotNull();

    count = index = 0;

    for (String result : searchResults) {
      assertThat(result).isEqualTo(ANIMALS[index++]);
      count++;
    }

    assertThat(count).isEqualTo(ANIMALS.length);
  }

  @Test
  public void searchForAllReturningNoMatches() {

    AbstractSearcher searcher = new TestSearcher();

    searcher.setMatcher(new AbstractMatcher<>() {

      @Override
      public int match(final Object obj) {
        return -1;
      }
    });

    assertThat(searcher.getMatcher()).isNotNull();

    Iterable<String> searchResults = searcher.searchForAll("test", "testing", "tested");

    assertThat(searchResults).isNotNull();
    assertThat(searchResults.iterator().hasNext()).isFalse();
  }

  @Test
  public void searchForAllUsingSearcherConcurrently() throws Throwable {
    TestFramework.runOnce(new UseSearcherConcurrentlyMultithreadedTestCase());
  }

  @Test
  public void getSearchableMetaData() {

    AbstractSearcher searcher = new TestSearcher();

    org.cp.elements.util.search.annotation.Searchable searchableMetaData =
      searcher.getSearchableMetaData(TestSearchableWithDefaults.INSTANCE);

    assertThat(searchableMetaData).isNotNull();
    assertThat(searchableMetaData.listMethod()).isEqualTo("asList");
    assertThat(searchableMetaData.matcher()).isEqualTo(Matcher.class);

    searchableMetaData = searcher.getSearchableMetaData(TestSearchableWithOverrides.INSTANCE);

    assertThat(searchableMetaData).isNotNull();
    assertThat(searchableMetaData.listMethod()).isEqualTo("toCollection");
    assertThat(searchableMetaData.matcher()).isEqualTo(TestMatcher.class);
  }

  @Test
  public void getSearchableMetaDataWithNonSearchableAnnotatedObject() {

    assertThatExceptionOfType(SearchException.class)
      .isThrownBy(() -> new TestSearcher().getSearchableMetaData(new Object()))
      .withMessage("To search an object of type (java.lang.Object), the class must be annotated with the (%1$s) annotation!",
        org.cp.elements.util.search.annotation.Searchable.class.getName())
      .withNoCause();
  }

  @Test
  public void getSearchableMetaDataWithNull() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new TestSearcher().getSearchableMetaData(null))
      .withMessage("The object to search cannot be null!")
      .withNoCause();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void configureMatcherUsingSearchable() {

    Matcher<Object> mockMatcher = mock(Matcher.class);
    Matcher<Object> testMatcher = new TestMatcher();

    Searchable<Object> mockSearchable = mock(Searchable.class);

    when(mockSearchable.getMatcher()).thenReturn(testMatcher);

    AbstractSearcher searcher = new TestSearcher();

    searcher.setCustomMatcherAllowed(true);
    searcher.setMatcher(mockMatcher);

    assertThat(searcher.isCustomMatcherAllowed()).isTrue();
    assertThat(searcher.getMatcher()).isSameAs(mockMatcher);
    assertThat(searcher.configureMatcher(mockSearchable)).isSameAs(mockSearchable);
    assertThat(searcher.getMatcher() instanceof TestMatcher).isTrue();

    AbstractSearcher.MatcherHolder.unset();
    searcher.setCustomMatcherAllowed(false);

    assertThat(searcher.isCustomMatcherAllowed()).isFalse();
    assertThat(searcher.getMatcher()).isSameAs(mockMatcher);
    assertThat(searcher.configureMatcher(mockSearchable)).isSameAs(mockSearchable);
    assertThat(searcher.getMatcher()).isSameAs(mockMatcher);

    when(mockSearchable.getMatcher()).thenReturn(null);

    searcher.setCustomMatcherAllowed(true);

    assertThat(searcher.isCustomMatcherAllowed()).isTrue();
    assertThat(searcher.configureMatcher(mockSearchable)).isSameAs(mockSearchable);
    assertThat(searcher.getMatcher()).isSameAs(mockMatcher);

    verify(mockSearchable, times(2)).getMatcher();
  }

  @Test
  public void configureMatcherUsingSearchableAnnotatedObject() {

    Matcher<?> mockMatcher = mock(Matcher.class);
    AbstractSearcher searcher = new TestSearcher();

    searcher.setCustomMatcherAllowed(true);
    searcher.setMatcher(mockMatcher);

    assertThat(searcher.isCustomMatcherAllowed()).isTrue();
    assertThat(searcher.getMatcher()).isSameAs(mockMatcher);

    org.cp.elements.util.search.annotation.Searchable searchableMetaData = searcher.getSearchableMetaData(
      TestSearchableWithOverrides.INSTANCE);

    assertThat(searcher.configureMatcher(searchableMetaData)).isSameAs(searchableMetaData);
    assertThat(searcher.getMatcher() instanceof TestMatcher).isTrue();

    AbstractSearcher.MatcherHolder.unset();
    searcher.setCustomMatcherAllowed(false);

    assertThat(searcher.isCustomMatcherAllowed()).isFalse();
    assertThat(searcher.getMatcher()).isSameAs(mockMatcher);
    assertThat(searcher.configureMatcher(searchableMetaData)).isSameAs(searchableMetaData);
    assertThat(searcher.getMatcher()).isSameAs(mockMatcher);

    searchableMetaData = searcher.getSearchableMetaData(TestSearchableWithDefaults.INSTANCE);
    searcher.setCustomMatcherAllowed(true);

    assertThat(searcher.isCustomMatcherAllowed()).isTrue();
    assertThat(searcher.configureMatcher(searchableMetaData)).isSameAs(searchableMetaData);
    assertThat(searcher.getMatcher()).isSameAs(mockMatcher);
  }

  @Test
  public void configureMatcherUsingSearchableAnnotatedObjectThrowsException() {

    ThrowableAssertions.assertThatThrowableOfType(SearchException.class)
      .isThrownBy(args -> {

        AbstractSearcher searcher = new TestSearcher();

        searcher.setCustomMatcherAllowed(true);

        assertThat(searcher.isCustomMatcherAllowed()).isTrue();

        searcher.configureMatcher(searcher.getSearchableMetaData(new TestSearchableWithProblem<>()));

        return null;
      })
      .havingMessage("Error occurred creating an instance of Matcher class (%1$s) to be used by this Searcher (%2$s)!"
          + " The Matcher class (%1$s) must have a public no-arg constructor!", TestProblemMatcher.class.getName(),
            TestSearcher.class.getName())
      .causedBy(IllegalStateException.class)
      .havingMessage("Construction Failed!")
      .withNoCause();
  }

  @Test
  public void asList() {

    AbstractSearcher searcher = new TestSearcher();

    List<String> animalList = searcher.asList(TestSearchableWithOverrides.INSTANCE,
      searcher.getSearchableMetaData(TestSearchableWithOverrides.INSTANCE));

    assertThat(animalList).isNotNull();
    assertThat(animalList.size()).isEqualTo(ANIMALS.length);
    assertThat(animalList.containsAll(Arrays.asList(ANIMALS))).isTrue();
  }

  @Test
  public void asListWithNonSearchableAnnotatedObject() {

    ThrowableAssertions.assertThatThrowableOfType(SearchException.class)
      .isThrownBy(args -> {
        AbstractSearcher searcher = new TestSearcher();
        searcher.asList(new Object(), searcher.getSearchableMetaData(TestSearchableWithDefaults.INSTANCE));
        return null;
      })
      .havingMessage("Error occurred getting the list of elements to search from the (asList) method on object of type (java.lang.Object)!")
      .causedBy(MethodNotFoundException.class)
      .causedBy(NoSuchMethodException.class)
      .withNoCause();
  }

  @Test
  public void asListWithSearchableAnnotatedObjectReturningNullList() {

    AbstractSearcher searcher = new TestSearcher();

    TestSearchableWithDefaults<Object> testSearchable = new TestSearchableWithDefaults<>(null);

    List<String> actualList = searcher.asList(testSearchable, searcher.getSearchableMetaData(testSearchable));

    assertThat(actualList).isNotNull();
    assertThat(actualList.isEmpty()).isTrue();
  }

  @Test
  public void matcherHolderGetIsSetSetAndUnset() {

    Matcher<?> mockMatcher = mock(Matcher.class);

    assertThat(AbstractSearcher.MatcherHolder.get()).isNull();
    assertThat(AbstractSearcher.MatcherHolder.isSet()).isFalse();

    AbstractSearcher.MatcherHolder.set(mockMatcher);

    assertThat(AbstractSearcher.MatcherHolder.get()).isSameAs(mockMatcher);
    assertThat(AbstractSearcher.MatcherHolder.isSet()).isTrue();

    AbstractSearcher.MatcherHolder.unset();

    assertThat(AbstractSearcher.MatcherHolder.get()).isNull();
    assertThat(AbstractSearcher.MatcherHolder.isSet()).isFalse();
  }

  public static class TestMatcher extends AbstractMatcher<Object> {

    private int count = 0;
    private int powersOfTwo = 2;

    @Override
    public final int match(Object obj) {
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
  protected static class TestSearchableWithDefaults<E> {

    protected static final TestSearchableWithDefaults<String> INSTANCE =
      new TestSearchableWithDefaults<>(Arrays.asList(ANIMALS));

    private final List<E> list;

    protected TestSearchableWithDefaults(List<E> list) {
      this.list = list;
    }

    public List<E> asList() {
      return this.list;
    }
  }

  @org.cp.elements.util.search.annotation.Searchable(listMethod = "toCollection", matcher = TestMatcher.class)
  protected static class TestSearchableWithOverrides<E> {

    protected static final TestSearchableWithOverrides<String> INSTANCE =
      new TestSearchableWithOverrides<>(Arrays.asList(ANIMALS));

    private final List<E> list;

    protected TestSearchableWithOverrides(List<E> list) {
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
    public <E> E search(Collection<E> collection) {
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

        @Override
        public <E> Iterable<E> searchForAll(Collection<E> collection) {

          Iterable<E> searchResults = super.searchForAll(collection);

          if ("Thread One".equals(Thread.currentThread().getName())) {
            assertThat(getMatcher()).isInstanceOf(TestMatcher.class);
            waitForTick(2);
          }
          else {
            assertThat(getMatcher()).isNotInstanceOf(TestMatcher.class);
          }

          return searchResults;
        }
      };

      searcher.setCustomMatcherAllowed(true);

      searcher.setMatcher(new AbstractMatcher<String>() {

        @Override
        public int match(String value) {
          return 0;
        }
      });
    }

    public void thread1() {

      Thread.currentThread().setName("Thread One");

      assertThat(searcher.isCustomMatcherAllowed()).isTrue();
      assertThat(searcher.getMatcher()).isNotNull();
      assertThat(searcher.getMatcher()).isNotInstanceOf(TestMatcher.class);

      Iterable<String> searchResults = searcher.searchForAll(TestSearchableWithOverrides.INSTANCE);

      assertTick(2);
      assertThat(searchResults).isNotNull();

      int count = 0;
      int index = 2;

      for (String result : searchResults) {
        assertThat(result).isEqualTo(ANIMALS[index - 1]);
        index *= 2;
        count++;
      }

      assertThat(count).isEqualTo(4);
    }

    public void thread2() {

      Thread.currentThread().setName("Thread Two");

      waitForTick(1);

      assertThat(searcher.isCustomMatcherAllowed()).isTrue();
      assertThat(searcher.getMatcher()).isNotNull();
      assertThat(searcher.getMatcher()).isNotInstanceOf(TestMatcher.class);

      Iterable<String> searchResults = searcher.searchForAll(TestSearchableWithDefaults.INSTANCE);

      assertThat(searchResults).isNotNull();

      int count = 0;
      int index = 0;

      for (String result : searchResults) {
        assertThat(result).isEqualTo(ANIMALS[index++]);
        count++;
      }

      assertThat(count).isEqualTo(ANIMALS.length);
    }

    @Override
    public void finish() {
      super.finish();
      searcher = null;
    }
  }
}
