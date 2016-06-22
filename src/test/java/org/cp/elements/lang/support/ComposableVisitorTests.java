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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.Visitor;
import org.cp.elements.util.CollectionUtils;
import org.junit.Test;

/**
 * Test suite of test cases testing the contract and functionality of the {@link ComposableVisitor} class.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.Visitor
 * @see org.cp.elements.lang.support.ComposableVisitor
 * @since 1.0.0
 */
public class ComposableVisitorTests {

  @Test
  public void add() {
    assertTrue(new ComposableVisitor().add(mock(Visitor.class)));
  }

  @Test
  public void addExistingVisitor() {
    Visitor mockVisitor = mock(Visitor.class);
    ComposableVisitor visitors = new ComposableVisitor();

    assertTrue(visitors.add(mockVisitor));
    assertFalse(visitors.add(mockVisitor));
  }

  @Test(expected = IllegalArgumentException.class)
  public void addNullVisitor() {
    new ComposableVisitor().add(null);
  }

  @Test
  public void contains() {
    Visitor mockVisitor = mock(Visitor.class);

    ComposableVisitor visitors = new ComposableVisitor();

    assertFalse(visitors.contains(mockVisitor));
    assertTrue(visitors.add(mockVisitor));
    assertTrue(visitors.contains(mockVisitor));
    assertFalse(visitors.add(mockVisitor));
    assertTrue(visitors.contains(mockVisitor));
    assertFalse(visitors.contains(null));
  }

  @Test
  public void iterator() {
    final Visitor[] expectedVisitors = {
      mock(Visitor.class, "MockVisitorOne"),
      mock(Visitor.class, "MockVisitorTwo"),
      mock(Visitor.class, "MockVisitorThree"),
    };

    ComposableVisitor visitors = new ComposableVisitor();

    for (Visitor expectedVisitor : expectedVisitors) {
      assertTrue(visitors.add(expectedVisitor));
    }

    int index = 0;

    for (Visitor actualVisitor : visitors) {
      assertEquals(expectedVisitors[index++], actualVisitor);
    }

    assertEquals(expectedVisitors.length, index);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void iteratorModification() {
    Visitor mockVisitor = mock(Visitor.class);
    ComposableVisitor visitors = new ComposableVisitor();

    assertTrue(visitors.add(mockVisitor));
    assertTrue(visitors.contains(mockVisitor));

    Iterator<Visitor> it = visitors.iterator();

    assertNotNull(it);
    assertTrue(it.hasNext());
    assertEquals(mockVisitor, it.next());

    it.remove();
  }

  @Test
  public void iteratorWithNoVisitors() {
    Iterator<Visitor> iterator = new ComposableVisitor().iterator();

    assertNotNull(iterator);
    assertFalse(iterator.hasNext());
  }

  @Test
  public void remove() {
    Visitor mockVisitor = mock(Visitor.class);
    ComposableVisitor visitors = new ComposableVisitor();

    assertFalse(visitors.contains(mockVisitor));
    assertFalse(visitors.remove(mockVisitor));
    assertTrue(visitors.add(mockVisitor));
    assertTrue(visitors.contains(mockVisitor));
    assertTrue(visitors.remove(mockVisitor));
    assertFalse(visitors.contains(mockVisitor));
    assertFalse(visitors.remove(mockVisitor));
    assertFalse(visitors.remove(null));
  }

  @Test
  public void size() {
    Visitor mockVisitor = mock(Visitor.class);
    ComposableVisitor visitors = new ComposableVisitor();

    assertEquals(0, visitors.size());
    assertTrue(visitors.add(mockVisitor));
    assertEquals(1, visitors.size());
    assertFalse(visitors.add(mockVisitor));
    assertEquals(1, visitors.size());
    assertFalse(visitors.remove(null));
    assertEquals(1, visitors.size());
    assertTrue(visitors.remove(mockVisitor));
    assertEquals(0, visitors.size());
    assertFalse(visitors.remove(mockVisitor));
    assertEquals(0, visitors.size());
  }

  @Test
  public void visit() {
    MockVisitable visitableThree = new MockVisitable(3);
    MockVisitable visitableTwo = new MockVisitable(2, visitableThree);
    MockVisitable visitableOne = new MockVisitable(1, visitableTwo);

    TraceVisitor traceVisitor = new TraceVisitor();

    SetVisitedVisitor visitedVisitor = new SetVisitedVisitor();

    ComposableVisitor visitors = new ComposableVisitor();

    assertTrue(visitors.add(traceVisitor));
    assertTrue(visitors.add(visitedVisitor));

    visitableOne.accept(visitors);

    Iterator<Visitable> iterator = traceVisitor.iterator();

    MockVisitable theVisitable = visitableOne;

    do {
      assertTrue(iterator.hasNext());
      assertEquals(theVisitable, iterator.next());
      assertTrue(theVisitable.isVisited());
      theVisitable = theVisitable.next();
    }
    while (theVisitable != null);
  }

  private static final class MockVisitable implements Visitable {

    private boolean visited;
    private final long id;
    private final MockVisitable visitable;

    public MockVisitable(long id) {
      this(id, null);
    }

    public MockVisitable(long id, MockVisitable visitable) {
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

    public MockVisitable next() {
      return visitable;
    }

    @Override
    public boolean equals(final Object obj) {
      if (obj == this) {
        return true;
      }

      if (!(obj instanceof MockVisitable)) {
        return false;
      }

      MockVisitable that = (MockVisitable) obj;

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
      if (visitable instanceof MockVisitable) {
        ((MockVisitable) visitable).setVisited();
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
