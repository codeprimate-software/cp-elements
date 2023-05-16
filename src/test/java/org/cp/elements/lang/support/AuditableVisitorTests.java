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
package org.cp.elements.lang.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.same;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.Month;
import java.time.ZoneId;
import java.time.ZonedDateTime;

import org.cp.elements.lang.Auditable;
import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.test.TestUtils;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit Tests for {@link AuditableVisitor}.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Auditable
 * @see org.cp.elements.lang.Visitor
 * @see org.cp.elements.lang.support.AuditableVisitor
 * @since 1.0.0
 */
@SuppressWarnings("unused")
@RunWith(MockitoJUnitRunner.class)
public class AuditableVisitorTests {

  @Mock
  private Process mockProcess;

  @Mock
  private User mockUser;

  private @NotNull Instant toInstant(int year, Month month, int day, int hour, int minute, int second) {
    return toInstant(LocalDateTime.of(year, month.getValue(), day, hour, minute, second));
  }

  private @NotNull Instant toInstant(@NotNull LocalDateTime dateTime) {
    return toInstant(ZonedDateTime.of(dateTime, ZoneId.systemDefault()));
  }

  private @NotNull Instant toInstant(@NotNull ZonedDateTime dateTime) {
    return dateTime.toInstant();
  }

  @Test
  public void constructWithUserAndProcess() {

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<>(mockUser, mockProcess);

    assertThat(visitor).isNotNull();
    assertThat(visitor.getUser()).isSameAs(mockUser);
    assertThat(visitor.getProcess()).isSameAs(mockProcess);
    assertThat(visitor.getDateTime()).isBeforeOrEqualTo(Instant.now());
  }

  @Test
  public void constructWithUserProcessAndDateTime() {

    Instant now = toInstant(2014, Month.JANUARY, 16, 22, 30, 45);

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<>(mockUser, mockProcess, now);

    assertThat(visitor).isNotNull();
    assertThat(visitor.getUser()).isSameAs(mockUser);
    assertThat(visitor.getProcess()).isSameAs(mockProcess);
    assertThat(visitor.getDateTime()).isEqualTo(now);
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructWithNullUser() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> new AuditableVisitor<User, Process>(null, mockProcess),
        () -> "User is required");
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructWithNullProcess() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> new AuditableVisitor<User, Process>(mockUser, null),
        () -> "Process is required");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void visit() {

    VisitableAuditable<User, Process> mockAuditable = mock(VisitableAuditable.class);

    when(mockAuditable.isNew()).thenReturn(true);
    when(mockAuditable.isModified()).thenReturn(true);

    Instant now = toInstant(2014, Month.JANUARY, 18, 14, 55, 30);

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<>(mockUser, mockProcess, now);

    assertThat(visitor).isNotNull();
    assertThat(visitor.getUser()).isSameAs(mockUser);
    assertThat(visitor.getProcess()).isSameAs(mockProcess);
    assertThat(visitor.getDateTime()).isEqualTo(now);

    visitor.visit(mockAuditable);

    verify(mockAuditable, times(1)).isNew();
    verify(mockAuditable, times(1)).setCreatedBy(same(mockUser));
    verify(mockAuditable, times(1)).setCreatedOn(eq(now));
    verify(mockAuditable, times(1)).setCreatedWith(same(mockProcess));
    verify(mockAuditable, times(1)).isModified();
    verify(mockAuditable, times(1)).setModifiedBy(same(mockUser));
    verify(mockAuditable, times(1)).setModifiedOn(eq(now));
    verify(mockAuditable, times(1)).setModifiedWith(same(mockProcess));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void visitWithNonIdentifiableObject() {

    VisitableAuditable<User, Process> mockAuditable = mock(VisitableAuditable.class);

    when(mockAuditable.getCreatedBy()).thenReturn(new User() {});
    when(mockAuditable.isModified()).thenReturn(true);

    Instant now = toInstant(2014, Month.JANUARY, 18, 15, 10, 15);

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<>(mockUser, mockProcess, now);

    assertThat(visitor).isNotNull();
    assertThat(visitor.getUser()).isSameAs(mockUser);
    assertThat(visitor.getProcess()).isSameAs(mockProcess);
    assertThat(visitor.getDateTime()).isEqualTo(now);

    visitor.visit(mockAuditable);

    verify(mockAuditable, times(1)).setCreatedBy(same(mockUser));
    verify(mockAuditable, times(1)).setCreatedOn(eq(now));
    verify(mockAuditable, times(1)).setCreatedWith(same(mockProcess));
    verify(mockAuditable, times(1)).isModified();
    verify(mockAuditable, times(1)).setModifiedBy(same(mockUser));
    verify(mockAuditable, times(1)).setModifiedOn(eq(now));
    verify(mockAuditable, times(1)).setModifiedWith(same(mockProcess));
  }

  @Test
  public void visitWithNonIdentifyableNonAuditableObject() {
    new AuditableVisitor<>(mockUser, mockProcess).visit(mock(Visitable.class));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void visitWhenModifiedOnly() {

    VisitableAuditable<User, Process> mockAuditable = mock(VisitableAuditable.class);

    when(mockAuditable.isNew()).thenReturn(false);
    when(mockAuditable.getCreatedBy()).thenReturn(new User() {});
    when(mockAuditable.getCreatedOn()).thenReturn(Instant.now());
    when(mockAuditable.isModified()).thenReturn(true);

    Instant now = toInstant(2014, Month.JANUARY, 18, 23, 45, 0);

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<>(mockUser, mockProcess, now);

    assertThat(visitor).isNotNull();
    assertThat(visitor.getUser()).isSameAs(mockUser);
    assertThat(visitor.getProcess()).isSameAs(mockProcess);
    assertThat(visitor.getDateTime()).isEqualTo(now);

    visitor.visit(mockAuditable);

    verify(mockAuditable, times(1)).isNew();
    verify(mockAuditable, never()).setCreatedBy(any(User.class));
    verify(mockAuditable, never()).setCreatedOn(any(Instant.class));
    verify(mockAuditable, never()).setCreatedWith(any(Process.class));
    verify(mockAuditable, times(1)).isModified();
    verify(mockAuditable, times(1)).setModifiedBy(same(mockUser));
    verify(mockAuditable, times(1)).setModifiedOn(eq(now));
    verify(mockAuditable, times(1)).setModifiedWith(same(mockProcess));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void visitDoesNothing() {

    VisitableAuditable<User, Process> mockAuditable = mock(VisitableAuditable.class);

    when(mockAuditable.isNew()).thenReturn(false);
    when(mockAuditable.getCreatedBy()).thenReturn(new User() {});
    when(mockAuditable.getCreatedOn()).thenReturn(Instant.now());
    when(mockAuditable.isModified()).thenReturn(false);

    Instant now = toInstant(2014, Month.JANUARY, 18, 23, 55, 45);

    AuditableVisitor<User, Process> visitor = new AuditableVisitor<>(mockUser, mockProcess, now);

    assertThat(visitor).isNotNull();
    assertThat(visitor.getUser()).isSameAs(mockUser);
    assertThat(visitor.getProcess()).isSameAs(mockProcess);
    assertThat(visitor.getDateTime()).isEqualTo(now);

    visitor.visit(mockAuditable);

    verify(mockAuditable, times(1)).isNew();
    verify(mockAuditable, never()).setCreatedBy(any(User.class));
    verify(mockAuditable, never()).setCreatedOn(any(Instant.class));
    verify(mockAuditable, never()).setCreatedWith(any(Process.class));
    verify(mockAuditable, times(1)).isModified();
    verify(mockAuditable, never()).setModifiedBy(any(User.class));
    verify(mockAuditable, never()).setModifiedOn(any(Instant.class));
    verify(mockAuditable, never()).setModifiedWith(any(Process.class));
  }

  interface VisitableAuditable<USER, PROCESS> extends Auditable<USER, PROCESS, Long>, Visitable { }

  interface User { }

  interface Process { }

}
