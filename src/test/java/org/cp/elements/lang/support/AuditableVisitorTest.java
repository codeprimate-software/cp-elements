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

import static org.cp.elements.test.TestUtils.assertEqualDate;
import static org.cp.elements.test.TestUtils.assertEqualDateTime;
import static org.cp.elements.test.TestUtils.createCalendar;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.same;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Calendar;

import org.cp.elements.lang.Auditable;
import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.Visitable;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

/**
 * The AuditableVisitorTest class is a test suite of test cases testing the contract and functionality of the
 * AuditableVisitor class.
 *
 * @author John J. Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
 * @see org.cp.elements.lang.Auditable
 * @see org.cp.elements.lang.Visitor
 * @see org.cp.elements.lang.support.AuditableVisitor
 * @since 1.0.0
 */
@SuppressWarnings("unused")
@RunWith(MockitoJUnitRunner.class)
public class AuditableVisitorTest {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @Mock
  private Process mockProcess;

  @Mock
  private User mockUser;

  @Test
  public void constructWithUserAndProcess() {
    AuditableVisitor<User, Process> visitor = new AuditableVisitor<>(mockUser, mockProcess);

    assertNotNull(visitor);
    assertSame(mockUser, visitor.getUser());
    assertSame(mockProcess, visitor.getProcess());
    assertEqualDate(Calendar.getInstance(), visitor.getDateTime());
  }

  @Test
  public void constructWithUserProcessAndDateTime() {
    Calendar now = createCalendar(2014, Calendar.JANUARY, 16, 22, 30, 45);

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<>(mockUser, mockProcess, now);

    assertNotNull(visitor);
    assertSame(mockUser, visitor.getUser());
    assertSame(mockProcess, visitor.getProcess());
    assertNotSame(now, visitor.getDateTime());
    assertEqualDateTime(now, visitor.getDateTime());
  }

  @Test
  public void constructWithNullUser() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("user must not be null");

    new AuditableVisitor<User, Process>(null, mockProcess);
  }

  @Test
  public void constructWithNullProcess() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("process must not be null");

    new AuditableVisitor<User, Process>(mockUser, null);
  }

  @Test
  public void isCreatedUnsetWithCreatedByAndCreatedDateTimePropertiesUnset() {
    Auditable mockAuditable = mock(Auditable.class);

    when(mockAuditable.getCreatedBy()).thenReturn(null);
    when(mockAuditable.getCreatedDateTime()).thenReturn(null);

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<>(mockUser, mockProcess);

    assertTrue(visitor.isCreatedUnset(mockAuditable));

    verify(mockAuditable, times(1)).getCreatedBy();
    verify(mockAuditable, never()).getCreatedDateTime();
  }

  @Test
  public void isCreatedUnsetWithOnlyCreatedByPropertyUnset() {
    Auditable mockAuditable = mock(Auditable.class);

    when(mockAuditable.getCreatedBy()).thenReturn(null);
    when(mockAuditable.getCreatedDateTime()).thenReturn(Calendar.getInstance());

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<>(mockUser, mockProcess);

    assertTrue(visitor.isCreatedUnset(mockAuditable));

    verify(mockAuditable, times(1)).getCreatedBy();
    verify(mockAuditable, never()).getCreatedDateTime();
  }

  @Test
  public void isCreatedUnsetWithOnlyCreatedDateTimePropertyUnset() {
    Auditable mockAuditable = mock(Auditable.class);

    when(mockAuditable.getCreatedBy()).thenReturn(mockUser);
    when(mockAuditable.getCreatedDateTime()).thenReturn(null);

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<>(mockUser, mockProcess);

    assertTrue(visitor.isCreatedUnset(mockAuditable));

    verify(mockAuditable, times(1)).getCreatedBy();
    verify(mockAuditable, times(1)).getCreatedDateTime();
  }

  @Test
  public void isCreatedUnsetWhenBothCreatedByAndCreatedDateTimePropertiesAreSet() {
    Auditable mockAuditable = mock(Auditable.class);

    when(mockAuditable.getCreatedBy()).thenReturn(mockUser);
    when(mockAuditable.getCreatedDateTime()).thenReturn(Calendar.getInstance());

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<>(mockUser, mockProcess);

    assertFalse(visitor.isCreatedUnset(mockAuditable));

    verify(mockAuditable, times(1)).getCreatedBy();
    verify(mockAuditable, times(1)).getCreatedDateTime();
  }

  @Test
  public void isNew() {
    VisitableIdentifiableAuditable mockIdentifiable = mock(VisitableIdentifiableAuditable.class);

    when(mockIdentifiable.isNew()).thenReturn(true);

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<>(mockUser, mockProcess);

    assertTrue(visitor.isNew(mockIdentifiable));

    verify(mockIdentifiable, times(1)).isNew();
  }

  @Test
  public void isNewReturningFalse() {
    VisitableIdentifiableAuditable mockIdentifiable = mock(VisitableIdentifiableAuditable.class);

    when(mockIdentifiable.isNew()).thenReturn(false);

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<>(mockUser, mockProcess);

    assertFalse(visitor.isNew(mockIdentifiable));

    verify(mockIdentifiable, times(1)).isNew();
  }

  @Test
  public void isNewWithNonIdentifiableObject() {
    assertFalse(new AuditableVisitor<>(mockUser, mockProcess).isNew(mock(Auditable.class)));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void visit() {
    VisitableIdentifiableAuditable<User, Process> mockAuditable = mock(VisitableIdentifiableAuditable.class);

    when(mockAuditable.isNew()).thenReturn(true);
    when(mockAuditable.isModified()).thenReturn(true);

    Calendar now = createCalendar(2014, Calendar.JANUARY, 18, 14, 55, 30);

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<>(mockUser, mockProcess, now);

    assertNotNull(visitor);
    assertSame(mockUser, visitor.getUser());
    assertSame(mockProcess, visitor.getProcess());
    assertEqualDateTime(now, visitor.getDateTime());

    visitor.visit(mockAuditable);

    verify(mockAuditable, times(1)).isNew();
    verify(mockAuditable, times(1)).setCreatedBy(same(mockUser));
    verify(mockAuditable, times(1)).setCreatedDateTime(eq(now));
    verify(mockAuditable, times(1)).setCreatingProcess(same(mockProcess));
    verify(mockAuditable, times(1)).isModified();
    verify(mockAuditable, times(1)).setModifiedBy(same(mockUser));
    verify(mockAuditable, times(1)).setModifiedDateTime(eq(now));
    verify(mockAuditable, times(1)).setModifyingProcess(same(mockProcess));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void visitWithNonIdentifiableObject() {
    VisitableAuditable<User, Process> mockAuditable = mock(VisitableAuditable.class);

    when(mockAuditable.getCreatedBy()).thenReturn(new User() {});
    when(mockAuditable.isModified()).thenReturn(true);

    Calendar now = createCalendar(2014, Calendar.JANUARY, 18, 15, 10, 15);

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<>(mockUser, mockProcess, now);

    assertNotNull(visitor);
    assertSame(mockUser, visitor.getUser());
    assertSame(mockProcess, visitor.getProcess());
    assertEqualDateTime(now, visitor.getDateTime());

    visitor.visit(mockAuditable);

    verify(mockAuditable, times(1)).setCreatedBy(same(mockUser));
    verify(mockAuditable, times(1)).setCreatedDateTime(eq(now));
    verify(mockAuditable, times(1)).setCreatingProcess(same(mockProcess));
    verify(mockAuditable, times(1)).isModified();
    verify(mockAuditable, times(1)).setModifiedBy(same(mockUser));
    verify(mockAuditable, times(1)).setModifiedDateTime(eq(now));
    verify(mockAuditable, times(1)).setModifyingProcess(same(mockProcess));
  }

  @Test
  public void visitWithNonIdentifyableNonAuditableObject() {
    new AuditableVisitor<>(mockUser, mockProcess).visit(mock(Visitable.class));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void visitWhenModifiedOnly() {
    VisitableIdentifiableAuditable<User, Process> mockAuditable = mock(VisitableIdentifiableAuditable.class);

    when(mockAuditable.isNew()).thenReturn(false);
    when(mockAuditable.getCreatedBy()).thenReturn(new User() {});
    when(mockAuditable.getCreatedDateTime()).thenReturn(Calendar.getInstance());
    when(mockAuditable.isModified()).thenReturn(true);

    Calendar now = createCalendar(2014, Calendar.JANUARY, 18, 23, 45, 0);

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<>(mockUser, mockProcess, now);

    assertNotNull(visitor);
    assertSame(mockUser, visitor.getUser());
    assertSame(mockProcess, visitor.getProcess());
    assertEqualDateTime(now, visitor.getDateTime());

    visitor.visit(mockAuditable);

    verify(mockAuditable, times(1)).isNew();
    verify(mockAuditable, never()).setCreatedBy(any(User.class));
    verify(mockAuditable, never()).setCreatedDateTime(any(Calendar.class));
    verify(mockAuditable, never()).setCreatingProcess(any(Process.class));
    verify(mockAuditable, times(1)).isModified();
    verify(mockAuditable, times(1)).setModifiedBy(same(mockUser));
    verify(mockAuditable, times(1)).setModifiedDateTime(eq(now));
    verify(mockAuditable, times(1)).setModifyingProcess(same(mockProcess));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void visitDoesNothing() {
    VisitableIdentifiableAuditable<User, Process> mockAuditable = mock(VisitableIdentifiableAuditable.class);

    when(mockAuditable.isNew()).thenReturn(false);
    when(mockAuditable.getCreatedBy()).thenReturn(new User() {});
    when(mockAuditable.getCreatedDateTime()).thenReturn(Calendar.getInstance());
    when(mockAuditable.isModified()).thenReturn(false);

    Calendar now = createCalendar(2014, Calendar.JANUARY, 18, 23, 55, 45);

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<>(mockUser, mockProcess, now);

    assertNotNull(visitor);
    assertSame(mockUser, visitor.getUser());
    assertSame(mockProcess, visitor.getProcess());
    assertEqualDateTime(now, visitor.getDateTime());

    visitor.visit(mockAuditable);

    verify(mockAuditable, times(1)).isNew();
    verify(mockAuditable, never()).setCreatedBy(any(User.class));
    verify(mockAuditable, never()).setCreatedDateTime(any(Calendar.class));
    verify(mockAuditable, never()).setCreatingProcess(any(Process.class));
    verify(mockAuditable, times(1)).isModified();
    verify(mockAuditable, never()).setModifiedBy(any(User.class));
    verify(mockAuditable, never()).setModifiedDateTime(any(Calendar.class));
    verify(mockAuditable, never()).setModifyingProcess(any(Process.class));
  }

  protected interface VisitableIdentifiableAuditable<USER, PROCESS> extends Auditable<USER, PROCESS>, Identifiable, Visitable {
  }

  protected interface VisitableAuditable<USER, PROCESS> extends Auditable<USER, PROCESS>, Visitable {
  }

  protected interface User {
  }

  protected interface Process {
  }
}
