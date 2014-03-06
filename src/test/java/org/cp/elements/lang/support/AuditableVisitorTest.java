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

import static org.cp.elements.test.TestUtils.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.*;

import java.util.Calendar;

import org.cp.elements.lang.Auditable;
import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.Visitable;
import org.cp.elements.test.AbstractMockingTestSuite;
import org.jmock.Expectations;
import org.junit.Test;

/**
 * The AuditableVisitorTest class is a test suite of test cases testing the contract and functionality of the
 * AuditableVisitor class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.lang.Auditable
 * @see org.cp.elements.lang.support.AuditableVisitor
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.junit.Test
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AuditableVisitorTest extends AbstractMockingTestSuite {

  @Test
  public void testConstructWithUserProcess() {
    User mockUser = mockContext.mock(User.class);
    Process mockProcess = mockContext.mock(Process.class);

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<User, Process>(mockUser, mockProcess);

    assertNotNull(visitor);
    assertSame(mockUser, visitor.getUser());
    assertSame(mockProcess, visitor.getProcess());
    assertEqualDate(Calendar.getInstance(), visitor.getDateTime());
  }

  @Test
  public void testConstructWithUserProcessAndDateTime() {
    User mockUser = mockContext.mock(User.class);
    Process mockProcess = mockContext.mock(Process.class);
    Calendar now = createCalendar(2014, Calendar.JANUARY, 16, 22, 30, 45);

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<User, Process>(mockUser, mockProcess, now);

    assertNotNull(visitor);
    assertSame(mockUser, visitor.getUser());
    assertSame(mockProcess, visitor.getProcess());
    assertNotSame(now, visitor.getDateTime());
    assertEqualDateTime(now, visitor.getDateTime());
  }

  @Test(expected = NullPointerException.class)
  public void testConstructWithNullUser() {
    try {
      new AuditableVisitor<User, Process>(null, mockContext.mock(Process.class));
    }
    catch (NullPointerException expected) {
      assertEquals("The user cannot be null!", expected.getMessage());
      throw expected;
    }
  }

  @Test(expected = NullPointerException.class)
  public void testConstructWithNullProcess() {
    try {
      new AuditableVisitor<User, Process>(mockContext.mock(User.class), null);
    }
    catch (NullPointerException expected) {
      assertEquals("The process cannot be null!", expected.getMessage());
      throw expected;
    }
  }

  @Test
  public void testIsCreatedUnsetWithCreatedByAndCreatedDateTimeUnset() {
    User mockUser = mockContext.mock(User.class);
    Process mockProcess = mockContext.mock(Process.class);

    final Auditable mockAuditable = mockContext.mock(Auditable.class);

    mockContext.checking(new Expectations() {{
      oneOf(mockAuditable).getCreatedBy();
      will(returnValue(null));
      allowing(mockAuditable).getCreatedDateTime();
      will(returnValue(null));
    }});

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<User, Process>(mockUser, mockProcess);

    assertTrue(visitor.isCreatedUnset(mockAuditable));
  }

  @Test
  public void testIsCreatedUnsetWithCreatedByUnset() {
    User mockUser = mockContext.mock(User.class);
    Process mockProcess = mockContext.mock(Process.class);

    final Calendar now = Calendar.getInstance();

    final Auditable mockAuditable = mockContext.mock(Auditable.class);

    mockContext.checking(new Expectations() {{
      oneOf(mockAuditable).getCreatedBy();
      will(returnValue(null));
      allowing(mockAuditable).getCreatedDateTime();
      will(returnValue(now));
    }});

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<User, Process>(mockUser, mockProcess);

    assertTrue(visitor.isCreatedUnset(mockAuditable));
  }

  @Test
  public void testIsCreatedUnsetWithCreatedDateTimeUnset() {
    final User mockUser = mockContext.mock(User.class);
    final Process mockProcess = mockContext.mock(Process.class);
    final Auditable mockAuditable = mockContext.mock(Auditable.class);

    mockContext.checking(new Expectations() {{
      oneOf(mockAuditable).getCreatedBy();
      will(returnValue(mockUser));
      oneOf(mockAuditable).getCreatedDateTime();
      will(returnValue(null));
    }});

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<User, Process>(mockUser, mockProcess);

    assertTrue(visitor.isCreatedUnset(mockAuditable));
  }

  @Test
  public void testIsCreatedUnset() {
    final User mockUser = mockContext.mock(User.class);
    final Process mockProcess = mockContext.mock(Process.class);
    final Calendar now = Calendar.getInstance();
    final Auditable mockAuditable = mockContext.mock(Auditable.class);

    mockContext.checking(new Expectations() {{
      oneOf(mockAuditable).getCreatedBy();
      will(returnValue(mockUser));
      oneOf(mockAuditable).getCreatedDateTime();
      will(returnValue(now));
    }});

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<User, Process>(mockUser, mockProcess);

    assertFalse(visitor.isCreatedUnset(mockAuditable));
  }

  @Test
  public void testIsNew() {
    final VisitableIdentifiableAuditable mockIdentifiable = mockContext.mock(VisitableIdentifiableAuditable.class);

    mockContext.checking(new Expectations() {{
      oneOf(mockIdentifiable).isNew();
      will(returnValue(true));
    }});

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<User, Process>(
      mockContext.mock(User.class), mockContext.mock(Process.class));

    assertTrue(visitor.isNew(mockIdentifiable));
  }

  @Test
  public void testIsNewReturningFalse() {
    final VisitableIdentifiableAuditable mockIdentifiable = mockContext.mock(VisitableIdentifiableAuditable.class);

    mockContext.checking(new Expectations() {{
      oneOf(mockIdentifiable).isNew();
      will(returnValue(false));
    }});

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<User, Process>(
      mockContext.mock(User.class), mockContext.mock(Process.class));

    assertFalse(visitor.isNew(mockIdentifiable));
  }

  @Test
  public void testIsNewWithNonIdentifiableObject() {
    AuditableVisitor<User, Process> visitor = new AuditableVisitor<User, Process>(
      mockContext.mock(User.class), mockContext.mock(Process.class));

    assertFalse(visitor.isNew(mockContext.mock(Auditable.class)));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testVisit() {
    final User mockUser = mockContext.mock(User.class);
    final Process mockProcess = mockContext.mock(Process.class);
    final Calendar now = createCalendar(2014, Calendar.JANUARY, 18, 14, 55, 30);

    final VisitableIdentifiableAuditable<User, Process> mockAuditable = mockContext.mock(VisitableIdentifiableAuditable.class);

    mockContext.checking(new Expectations() {{
      oneOf(mockAuditable).isNew();
      will(returnValue(true));
      oneOf(mockAuditable).setCreatedBy(with(same(mockUser)));
      oneOf(mockAuditable).setCreatedDateTime(with(equal(now)));
      oneOf(mockAuditable).setCreatingProcess(with(same(mockProcess)));
      oneOf(mockAuditable).isModified();
      will(returnValue(true));
      oneOf(mockAuditable).setModifiedBy(with(same(mockUser)));
      oneOf(mockAuditable).setModifiedDateTime(with(equal(now)));
      oneOf(mockAuditable).setModifyingProcess(with(same(mockProcess)));
    }});

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<User, Process>(mockUser, mockProcess, now);

    assertNotNull(visitor);
    assertSame(mockUser, visitor.getUser());
    assertSame(mockProcess, visitor.getProcess());
    assertEqualDateTime(now, visitor.getDateTime());

    visitor.visit(mockAuditable);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testVisitWithNonIdentifiableObject() {
    final User mockUser = mockContext.mock(User.class);
    final Process mockProcess = mockContext.mock(Process.class);
    final Calendar now = createCalendar(2014, Calendar.JANUARY, 18, 15, 10, 15);

    final VisitableAuditable<User, Process> mockAuditable = mockContext.mock(VisitableAuditable.class);

    mockContext.checking(new Expectations() {{
      oneOf(mockAuditable).getCreatedBy();
      will(returnValue(new User() {}));
      oneOf(mockAuditable).getCreatedDateTime();
      will(returnValue(null));
      oneOf(mockAuditable).setCreatedBy(with(same(mockUser)));
      oneOf(mockAuditable).setCreatedDateTime(with(equal(now)));
      oneOf(mockAuditable).setCreatingProcess(with(same(mockProcess)));
      oneOf(mockAuditable).isModified();
      will(returnValue(true));
      oneOf(mockAuditable).setModifiedBy(with(same(mockUser)));
      oneOf(mockAuditable).setModifiedDateTime(with(equal(now)));
      oneOf(mockAuditable).setModifyingProcess(with(same(mockProcess)));
    }});

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<User, Process>(mockUser, mockProcess, now);

    assertNotNull(visitor);
    assertSame(mockUser, visitor.getUser());
    assertSame(mockProcess, visitor.getProcess());
    assertEqualDateTime(now, visitor.getDateTime());

    visitor.visit(mockAuditable);
  }

  @Test
  public void testVisitWithNonIdentiableNonAuditableObject() {
    new AuditableVisitor<User, Process>(mockContext.mock(User.class), mockContext.mock(Process.class))
      .visit(mockContext.mock(Visitable.class));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testVisitWhenModifiedOnly() {
    final User mockUser = mockContext.mock(User.class);
    final Process mockProcess = mockContext.mock(Process.class);
    final Calendar now = createCalendar(2014, Calendar.JANUARY, 18, 23, 45, 0);

    final VisitableIdentifiableAuditable<User, Process> mockAuditable = mockContext.mock(VisitableIdentifiableAuditable.class);

    mockContext.checking(new Expectations() {{
      oneOf(mockAuditable).isNew();
      will(returnValue(false));
      oneOf(mockAuditable).getCreatedBy();
      will(returnValue(new User() {}));
      oneOf(mockAuditable).getCreatedDateTime();
      will(returnValue(Calendar.getInstance()));
      never(mockAuditable).setCreatedBy(with(any(User.class)));
      never(mockAuditable).setCreatedDateTime(with(any(Calendar.class)));
      never(mockAuditable).setCreatingProcess(with(any(Process.class)));
      oneOf(mockAuditable).isModified();
      will(returnValue(true));
      oneOf(mockAuditable).setModifiedBy(with(same(mockUser)));
      oneOf(mockAuditable).setModifiedDateTime(with(equal(now)));
      oneOf(mockAuditable).setModifyingProcess(with(same(mockProcess)));
    }});

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<User, Process>(mockUser, mockProcess, now);

    assertNotNull(visitor);
    assertSame(mockUser, visitor.getUser());
    assertSame(mockProcess, visitor.getProcess());
    assertEqualDateTime(now, visitor.getDateTime());

    visitor.visit(mockAuditable);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testVisitDoesNothing() {
    final User mockUser = mockContext.mock(User.class);
    final Process mockProcess = mockContext.mock(Process.class);
    final Calendar now = createCalendar(2014, Calendar.JANUARY, 18, 23, 55, 45);

    final VisitableIdentifiableAuditable<User, Process> mockAuditable = mockContext.mock(VisitableIdentifiableAuditable.class);

    mockContext.checking(new Expectations() {{
      oneOf(mockAuditable).isNew();
      will(returnValue(false));
      oneOf(mockAuditable).getCreatedBy();
      will(returnValue(new User() {}));
      oneOf(mockAuditable).getCreatedDateTime();
      will(returnValue(Calendar.getInstance()));
      never(mockAuditable).setCreatedBy(with(any(User.class)));
      never(mockAuditable).setCreatedDateTime(with(any(Calendar.class)));
      never(mockAuditable).setCreatingProcess(with(any(Process.class)));
      oneOf(mockAuditable).isModified();
      will(returnValue(false));
      never(mockAuditable).setModifiedBy(with(any(User.class)));
      never(mockAuditable).setModifiedDateTime(with(any(Calendar.class)));
      never(mockAuditable).setModifyingProcess(with(any(Process.class)));
    }});

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<User, Process>(mockUser, mockProcess, now);

    assertNotNull(visitor);
    assertSame(mockUser, visitor.getUser());
    assertSame(mockProcess, visitor.getProcess());
    assertEqualDateTime(now, visitor.getDateTime());

    visitor.visit(mockAuditable);
  }

  protected static interface VisitableIdentifiableAuditable<USER, PROCESS> extends Auditable<USER, PROCESS>, Identifiable, Visitable {
  }

  protected static interface VisitableAuditable<USER, PROCESS> extends Auditable<USER, PROCESS>, Visitable {
  }

  protected static interface User {
  }

  protected static interface Process {
  }

}
