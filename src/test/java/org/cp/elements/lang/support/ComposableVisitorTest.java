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

package org.cp.elements.lang.support;

import static org.junit.Assert.*;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.Visitor;
import org.cp.elements.test.AbstractMockingTestSuite;
import org.cp.elements.util.CollectionUtils;
import org.junit.Test;

/**
 * The ComposableVisitorTest class is a test suite of test cases testing the contract and functionality of the
 * ComposableVisitor class.
 * <p/>
 * @author John J. Blum
 * @see ComposableVisitor
 * @see org.junit.Test
 * @since 1.0.0
 */
public class ComposableVisitorTest extends AbstractMockingTestSuite {

  @Test
  public void testAdd() {
    assertTrue(new ComposableVisitor().add(mockContext.mock(Visitor.class)));
  }

  @Test
  public void testAddExistingVisitor() {
    final Visitor mockVisitor = mockContext.mock(Visitor.class);
    final ComposableVisitor visitors = new ComposableVisitor();
    assertTrue(visitors.add(mockVisitor));
    assertFalse(visitors.add(mockVisitor));
  }

  @Test(expected = NullPointerException.class)
  public void testAddNullVisitor() {
    new ComposableVisitor().add(null);
  }

  @Test
  public void testContains() {
    final Visitor mockVisitor = mockContext.mock(Visitor.class);
    final ComposableVisitor visitors = new ComposableVisitor();

    assertFalse(visitors.contains(mockVisitor));
    assertTrue(visitors.add(mockVisitor));
    assertTrue(visitors.contains(mockVisitor));
    assertFalse(visitors.add(mockVisitor));
    assertTrue(visitors.contains(mockVisitor));
    assertFalse(visitors.contains(null));
  }

  @Test
  public void testIterator() {
    final Visitor[] expectedVisitors = {
      mockContext.mock(Visitor.class, "Visitor 1"),
      mockContext.mock(Visitor.class, "Visitor 2"),
      mockContext.mock(Visitor.class, "Visitor 3"),
    };

    final ComposableVisitor visitors = new ComposableVisitor();

    for (final Visitor expectedVisitor : expectedVisitors) {
      assertTrue(visitors.add(expectedVisitor));
    }

    int index = 0;

    for (final Visitor actualVisitor : visitors) {
      assertEquals(expectedVisitors[index++], actualVisitor);
    }

    assertEquals(expectedVisitors.length, index);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testIteratorModification() {
    final Visitor mockVisitor = mockContext.mock(Visitor.class);
    final ComposableVisitor visitors = new ComposableVisitor();

    assertTrue(visitors.add(mockVisitor));
    assertTrue(visitors.contains(mockVisitor));

    final Iterator<Visitor> it = visitors.iterator();

    assertNotNull(it);
    assertTrue(it.hasNext());
    assertEquals(mockVisitor, it.next());

    it.remove();
  }

  @Test
  public void testIteratorWithNoVisitors() {
    final Iterator<Visitor> it = new ComposableVisitor().iterator();

    assertNotNull(it);
    assertFalse(it.hasNext());
  }

  @Test
  public void testRemove() {
    final Visitor mockVisitor = mockContext.mock(Visitor.class);
    final ComposableVisitor visitors = new ComposableVisitor();

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
  public void testSize() {
    final Visitor mockVisitor = mockContext.mock(Visitor.class);
    final ComposableVisitor visitors = new ComposableVisitor();

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
  public void testVisit() {
    final MockVisitable visitable3 = new MockVisitable(3);
    final MockVisitable visitable2 = new MockVisitable(2, visitable3);
    final MockVisitable visitable1 = new MockVisitable(1, visitable2);

    final SetVisitedVisitor visitedVisitor = new SetVisitedVisitor();
    final TraceVisitor traceVisitor = new TraceVisitor();

    final ComposableVisitor visitors = new ComposableVisitor();

    assertTrue(visitors.add(traceVisitor));
    assertTrue(visitors.add(visitedVisitor));

    visitable1.accept(visitors);

    final Iterator<Visitable> it = traceVisitor.iterator();

    MockVisitable theVisitable = visitable1;

    do {
      assertTrue(it.hasNext());
      assertEquals(theVisitable, it.next());
      assertTrue(theVisitable.isVisited());
      theVisitable = theVisitable.next();
    }
    while (theVisitable != null);
  }

  private static final class MockVisitable implements Visitable {

    private boolean visited;
    private final long id;
    private final MockVisitable visitable;

    public MockVisitable(final long id) {
      this(id, null);
    }

    public MockVisitable(final long id, final MockVisitable visitable) {
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

    public void accept(final Visitor visitor) {
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

      final MockVisitable that = (MockVisitable) obj;

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
      final StringBuilder buffer = new StringBuilder(getClass().getName());
      buffer.append(":{id = ").append(getId());
      buffer.append(", visitable = is ").append(next() == null ? "" : "not").append(" null");
      buffer.append(", visited = ").append(isVisited());
      buffer.append("}");
      return buffer.toString();
    }
  }

  private static final class SetVisitedVisitor implements Visitor {

    public void visit(final Visitable visitable) {
      if (visitable instanceof MockVisitable) {
        ((MockVisitable) visitable).setVisited();
      }
    }
  }

  @SuppressWarnings("unused")
  private static final class TraceVisitor implements Iterable<Visitable>, Visitor {

    private List<Visitable> visitablesVisited = new LinkedList<Visitable>();

    public Iterator<Visitable> iterator() {
      return CollectionUtils.unmodifiableIterator(visitablesVisited.iterator());
    }

    public void visit(final Visitable visitable) {
      visitablesVisited.add(visitable);
    }

    public boolean visited(final Visitable visitable) {
      return visitablesVisited.contains(visitable);
    }
  }

}
