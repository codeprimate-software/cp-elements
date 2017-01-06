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

package org.cp.elements.lang.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.util.ArrayUtils.asIterable;
import static org.cp.elements.util.ArrayUtils.getFirst;
import static org.cp.elements.util.CollectionUtils.toSet;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.mock;

import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.Visitor;
import org.cp.elements.util.CollectionUtils;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for {@link ComposableVisitor}.
 *
 * @author John J. Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.Visitor
 * @see org.cp.elements.lang.support.ComposableVisitor
 * @since 1.0.0
 */
public class ComposableVisitorTests {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  protected Visitor mockVisitor(String... name) {
    return mock(Visitor.class, getFirst(name, "MockVisitor"));
  }

  @Test
  public void addVisitorReturnsTrue() {
    assertThat(new ComposableVisitor().add(mockVisitor())).isTrue();
  }

  @Test
  public void addExistingVisitorReturnsFalse() {
    Visitor mockVisitor = mockVisitor();

    ComposableVisitor visitors = new ComposableVisitor();

    assertThat(visitors.add(mockVisitor)).isTrue();
    assertThat(visitors.add(mockVisitor)).isFalse();
  }

  @Test
  public void addNullVisitorThrowsIllegalArgumentException() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("The Visitor to add to this composite cannot be null");

    new ComposableVisitor().add(null);
  }

  @Test
  public void addSelfReturnsFalse() {
    ComposableVisitor visitors = new ComposableVisitor();

    assertThat(visitors.add(visitors)).isFalse();
  }

  @Test
  public void containsAddedVisitorIsTrue() {
    Visitor mockVisitor = mockVisitor();

    ComposableVisitor visitors = new ComposableVisitor();

    assertThat(visitors.add(mockVisitor)).isTrue();
    assertThat(visitors.contains(mockVisitor)).isTrue();
    assertThat(visitors.add(mockVisitor)).isFalse();
    assertThat(visitors.contains(mockVisitor)).isTrue();
  }

  @Test
  public void containsNonAddedVisitorIsFalse() {
    assertThat(new ComposableVisitor().contains(mockVisitor())).isFalse();
  }

  @Test
  public void containsNullIsFalse() {
    assertThat(new ComposableVisitor().contains(null)).isFalse();
  }
  @Test
  public void isEmptyAfterAddIsFalse() {
    ComposableVisitor visitors = new ComposableVisitor();

    assertThat(visitors.add(mockVisitor())).isTrue();
    assertThat(visitors.isEmpty()).isFalse();
  }

  @Test
  public void isEmptyBeforeAddIsTrue() {
    assertThat(new ComposableVisitor().isEmpty()).isTrue();
  }

  @Test
  public void iterator() {
    Visitor[] expectedVisitors = {
      mockVisitor("one"),
      mockVisitor("two"),
      mockVisitor("three")
    };

    ComposableVisitor visitors = (ComposableVisitor) new ComposableVisitor().compose(expectedVisitors);

    int index = 0;

    for (Visitor actualVisitor : visitors) {
      assertThat(actualVisitor).isEqualTo(expectedVisitors[index++]);
    }

    assertThat(index).isEqualTo(expectedVisitors.length);
  }

  @Test
  public void iteratorRemoveThrowsUnsupportedOperationException() {
    Visitor mockVisitor = mockVisitor();

    ComposableVisitor visitors = new ComposableVisitor();

    assertThat(visitors.add(mockVisitor)).isTrue();

    Iterator<Visitor> iterator = visitors.iterator();

    assertThat(iterator).isNotNull();
    assertThat(iterator.hasNext()).isTrue();
    assertThat(iterator.next()).isEqualTo(mockVisitor);

    exception.expect(UnsupportedOperationException.class);
    exception.expectCause(is(nullValue(Throwable.class)));

    iterator.remove();
  }

  @Test
  public void iteratorWithNoVisitors() {
    ComposableVisitor composableVisitor = new ComposableVisitor();

    assertThat(composableVisitor.isEmpty()).isTrue();

    Iterator<Visitor> iterator = composableVisitor.iterator();

    assertThat(iterator).isNotNull();
    assertThat(iterator.hasNext()).isFalse();
  }

  @Test
  public void remove() {
    Visitor mockVisitor = mockVisitor();

    ComposableVisitor visitors = new ComposableVisitor();

    assertThat(visitors.remove(mockVisitor)).isFalse();
    assertThat(visitors.add(mockVisitor)).isTrue();
    assertThat(visitors.contains(mockVisitor)).isTrue();
    assertThat(visitors.remove(mockVisitor)).isTrue();
    assertThat(visitors.contains(mockVisitor)).isFalse();
    assertThat(visitors.remove(mockVisitor)).isFalse();
  }

  @Test
  public void size() {
    Visitor mockVisitor = mockVisitor();

    ComposableVisitor visitors = new ComposableVisitor();

    assertThat(visitors.size()).isEqualTo(0);
    assertThat(visitors.add(mockVisitor)).isTrue();
    assertThat(visitors.size()).isEqualTo(1);
    assertThat(visitors.add(mockVisitor)).isFalse();
    assertThat(visitors.size()).isEqualTo(1);
    assertThat(visitors.remove(null)).isFalse();
    assertThat(visitors.size()).isEqualTo(1);
    assertThat(visitors.remove(mockVisitor)).isTrue();
    assertThat(visitors.size()).isEqualTo(0);
    assertThat(visitors.remove(mockVisitor)).isFalse();
    assertThat(visitors.size()).isEqualTo(0);
  }

  @Test
  public void visit() {
    TestVisitable visitableThree = new TestVisitable(3);
    TestVisitable visitableTwo = new TestVisitable(2, visitableThree);
    TestVisitable visitableOne = new TestVisitable(1, visitableTwo);

    TraceVisitor traceVisitor = new TraceVisitor();

    SetVisitedVisitor visitedVisitor = new SetVisitedVisitor();

    ComposableVisitor visitors = new ComposableVisitor();

    assertThat(visitors.add(traceVisitor)).isTrue();
    assertThat(visitors.add(visitedVisitor)).isTrue();

    visitableOne.accept(visitors);

    Iterator<Visitable> iterator = traceVisitor.iterator();

    TestVisitable currentVisitable = visitableOne;

    do {
      assertThat(iterator.hasNext()).isTrue();
      assertThat(iterator.next()).isEqualTo(currentVisitable);
      assertThat(currentVisitable.isVisited()).isTrue();
      currentVisitable = currentVisitable.next();
    }
    while (currentVisitable != null);
  }

  @Test
  public void compose() {
    Visitor mockVisitorOne = mockVisitor("one");
    Visitor mockVisitorTwo = mockVisitor("two");

    ComposableVisitor visitors = new ComposableVisitor();

    assertThat(visitors.isEmpty()).isTrue();
    assertThat(visitors.compose(mockVisitorOne, mockVisitorTwo)).isSameAs(visitors);
    assertThat(visitors.size()).isEqualTo(2);
    assertThat(toSet(visitors)).containsAll(asIterable(mockVisitorOne, mockVisitorTwo));
  }

  @Test
  public void composeArray() {
    Visitor[] visitorArray = {
      mockVisitor("one"),
      mockVisitor("two"),
      mockVisitor("three")
    };

    ComposableVisitor visitors = new ComposableVisitor();

    assertThat(visitors.isEmpty()).isTrue();
    assertThat(visitors.compose(visitorArray)).isSameAs(visitors);
    assertThat(visitors.size()).isEqualTo(visitorArray.length);
    assertThat(toSet(visitors)).containsAll(asIterable(visitorArray));
  }

  @Test
  public void composeIterable() {
    Iterable<Visitor> iterable = Arrays.asList(mockVisitor("one"), mockVisitor("two"), mockVisitor("three"));

    ComposableVisitor visitors = new ComposableVisitor();

    assertThat(visitors.isEmpty()).isTrue();
    assertThat(visitors.compose(iterable)).isSameAs(visitors);
    assertThat(visitors.size()).isEqualTo(3);
    assertThat(toSet(visitors)).containsAll(iterable);
  }

  @Test
  public void composeOne() {
    Visitor mockVisitor = mockVisitor("one");

    ComposableVisitor visitors = new ComposableVisitor();

    assertThat(visitors.isEmpty()).isTrue();
    assertThat(visitors.compose(mockVisitor, null)).isSameAs(visitors);
    assertThat(visitors.size()).isEqualTo(1);
    assertThat(toSet(visitors)).containsAll(asIterable(mockVisitor));

    visitors = new ComposableVisitor();

    assertThat(visitors.isEmpty()).isTrue();
    assertThat(visitors.compose(null, mockVisitor)).isSameAs(visitors);
    assertThat(visitors.size()).isEqualTo(1);
    assertThat(toSet(visitors)).containsAll(asIterable(mockVisitor));
  }

  private static final class TestVisitable implements Visitable {

    private boolean visited;
    private final long id;
    private final TestVisitable visitable;

    public TestVisitable(long id) {
      this(id, null);
    }

    public TestVisitable(long id, TestVisitable visitable) {
      this.id = id;
      this.visitable = visitable;
    }

    public long getId() {
      return id;
    }

    public boolean isVisited() {
      return visited;
    }

    public void setVisited() {
      this.visited = true;
    }

    public void accept(Visitor visitor) {
      visitor.visit(this);

      if (next() != null) {
        next().accept(visitor);
      }
    }

    public TestVisitable next() {
      return visitable;
    }

    @Override
    public boolean equals(final Object obj) {
      if (obj == this) {
        return true;
      }

      if (!(obj instanceof TestVisitable)) {
        return false;
      }

      TestVisitable that = (TestVisitable) obj;

      return (this.getId() == that.getId());
    }

    @Override
    public int hashCode() {
      int hashValue = 17;
      hashValue = 37 * hashValue + ObjectUtils.hashCode(getId());
      return hashValue;
    }

    @Override
    public String toString() {
      return String.format("{ id = %1$s, vistable = is %2$s null, visited = %3$s }", getId(),
        (next() == null ? "" : "not"), isVisited());
    }
  }

  private static final class SetVisitedVisitor implements Visitor {

    public void visit(Visitable visitable) {
      if (visitable instanceof TestVisitable) {
        ((TestVisitable) visitable).setVisited();
      }
    }
  }

  @SuppressWarnings("unused")
  private static final class TraceVisitor implements Iterable<Visitable>, Visitor {

    private List<Visitable> visitablesVisited = new LinkedList<>();

    public Iterator<Visitable> iterator() {
      return CollectionUtils.unmodifiableIterator(visitablesVisited.iterator());
    }

    public void visit(Visitable visitable) {
      visitablesVisited.add(visitable);
    }

    public boolean visited(Visitable visitable) {
      return visitablesVisited.contains(visitable);
    }
  }
}
