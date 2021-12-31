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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
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
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit Tests for {@link CommitVisitor}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.lang.Auditable
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.Visitor
 * @see org.cp.elements.lang.support.CommitVisitor
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class CommitVisitorTests {

  @Mock
  @SuppressWarnings("rawtypes")
  private Auditable mockAuditable;

  private @NotNull Instant toInstant(@NotNull LocalDateTime dateTime) {
    return toInstant(ZonedDateTime.of(dateTime, ZoneId.systemDefault()));
  }

  private @NotNull Instant toInstant(@NotNull ZonedDateTime dateTime) {
    return dateTime.toInstant();
  }

  @Test
  public void isCommitableWithAuditable() {

    assertTrue(new CommitVisitor().isCommittable(mockAuditable));
    assertTrue(new CommitVisitor(mockAuditable).isCommittable(mockAuditable));
  }

  @Test
  public void isCommitableWithNonAuditable() {

    assertFalse(new CommitVisitor().isCommittable(new Object()));
    assertFalse(new CommitVisitor(mockAuditable).isCommittable(new Object()));
  }

  @Test
  public void isCommitableWithNonTargetedAuditable() {
    assertFalse(new CommitVisitor(mockAuditable).isCommittable(mock(Auditable.class)));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void visit() {

    Instant expectedDateTime =
      toInstant(LocalDateTime.of(2014, Month.DECEMBER, 17, 0, 0));

    AuditableVisitable<String, String> mockAuditableVisitable = mock(AuditableVisitable.class);

    when(mockAuditableVisitable.getModifiedBy()).thenReturn("ExpectedUser");
    when(mockAuditableVisitable.getModifiedOn()).thenReturn(expectedDateTime);
    when(mockAuditableVisitable.getModifiedWith()).thenReturn("ExpectedProcess");

    assertNull(mockAuditableVisitable.lastModifiedBy);
    assertNull(mockAuditableVisitable.lastModifiedOn);
    assertNull(mockAuditableVisitable.lastModifiedWith);

    new CommitVisitor().visit(mockAuditableVisitable);

    assertEquals("ExpectedUser", mockAuditableVisitable.lastModifiedBy);
    assertEquals(expectedDateTime, mockAuditableVisitable.lastModifiedOn);
    assertEquals("ExpectedProcess", mockAuditableVisitable.lastModifiedWith);

    Instant updatedExpectedDateTime =
      toInstant(LocalDateTime.of(2014, Month.DECEMBER, 18, 0, 0));

    when(mockAuditableVisitable.getModifiedBy()).thenReturn("UpdatedExpectedUser");
    when(mockAuditableVisitable.getModifiedOn()).thenReturn(updatedExpectedDateTime);
    when(mockAuditableVisitable.getModifiedWith()).thenReturn("UpdatedExpectedProcess");

    new CommitVisitor(mockAuditableVisitable).visit(mockAuditableVisitable);

    assertEquals("UpdatedExpectedUser", mockAuditableVisitable.lastModifiedBy);
    assertEquals(updatedExpectedDateTime, mockAuditableVisitable.lastModifiedOn);
    assertEquals("UpdatedExpectedProcess", mockAuditableVisitable.lastModifiedWith);
  }

  @Test
  public void visitWithNonAuditable() {
    new CommitVisitor().visit(mock(Visitable.class));
  }

  @Test
  public void visitWithNonTargetedAuditable() {
    AuditableVisitable<?, ?> mockAuditableVisitable = mock(AuditableVisitable.class);

    assertNull(mockAuditableVisitable.lastModifiedBy);
    assertNull(mockAuditableVisitable.lastModifiedOn);
    assertNull(mockAuditableVisitable.lastModifiedWith);

    new CommitVisitor(mockAuditable).visit(mockAuditableVisitable);

    assertNull(mockAuditableVisitable.lastModifiedBy);
    assertNull(mockAuditableVisitable.lastModifiedOn);
    assertNull(mockAuditableVisitable.lastModifiedWith);

    verify(mockAuditableVisitable, never()).getModifiedBy();
    verify(mockAuditableVisitable, never()).getModifiedOn();
    verify(mockAuditableVisitable, never()).getModifiedWith();
  }

  @SuppressWarnings("unused")
  public static abstract class AuditableVisitable<USER, PROCESS> implements Auditable<USER, PROCESS, Long>, Visitable {

    private Instant lastModifiedOn;
    private PROCESS lastModifiedWith;
    private USER lastModifiedBy;

  }
}
