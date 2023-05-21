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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.Month;
import java.time.ZoneId;
import java.time.ZonedDateTime;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cp.elements.lang.Auditable;
import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.annotation.NotNull;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Unit Tests for {@link CommitVisitor}.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Auditable
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.Visitor
 * @see org.cp.elements.lang.support.CommitVisitor
 * @since 1.0.0
 */
@ExtendWith(MockitoExtension.class)
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

    assertThat(new CommitVisitor().isCommittable(this.mockAuditable)).isTrue();
    assertThat(new CommitVisitor(this.mockAuditable).isCommittable(this.mockAuditable)).isTrue();
  }

  @Test
  public void isCommitableWithNonAuditable() {

    assertThat(new CommitVisitor().isCommittable(new Object())).isFalse();
    assertThat(new CommitVisitor(this.mockAuditable).isCommittable(new Object())).isFalse();
  }

  @Test
  public void isCommitableWithNonTargetedAuditable() {
    assertThat(new CommitVisitor(this.mockAuditable).isCommittable(mock(Auditable.class))).isFalse();
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

    assertThat(mockAuditableVisitable.lastModifiedBy).isNull();
    assertThat(mockAuditableVisitable.lastModifiedOn).isNull();
    assertThat(mockAuditableVisitable.lastModifiedWith).isNull();

    new CommitVisitor().visit(mockAuditableVisitable);

    assertThat(mockAuditableVisitable.lastModifiedBy).isEqualTo("ExpectedUser");
    assertThat(mockAuditableVisitable.lastModifiedOn).isEqualTo(expectedDateTime);
    assertThat(mockAuditableVisitable.lastModifiedWith).isEqualTo("ExpectedProcess");

    Instant updatedExpectedDateTime =
      toInstant(LocalDateTime.of(2014, Month.DECEMBER, 18, 0, 0));

    when(mockAuditableVisitable.getModifiedBy()).thenReturn("UpdatedExpectedUser");
    when(mockAuditableVisitable.getModifiedOn()).thenReturn(updatedExpectedDateTime);
    when(mockAuditableVisitable.getModifiedWith()).thenReturn("UpdatedExpectedProcess");

    new CommitVisitor(mockAuditableVisitable).visit(mockAuditableVisitable);

    assertThat(mockAuditableVisitable.lastModifiedBy).isEqualTo("UpdatedExpectedUser");
    assertThat(mockAuditableVisitable.lastModifiedOn).isEqualTo(updatedExpectedDateTime);
    assertThat(mockAuditableVisitable.lastModifiedWith).isEqualTo("UpdatedExpectedProcess");
  }

  @Test
  public void visitWithNonAuditable() {
    new CommitVisitor().visit(mock(Visitable.class));
  }

  @Test
  public void visitWithNonTargetedAuditable() {
    AuditableVisitable<?, ?> mockAuditableVisitable = mock(AuditableVisitable.class);

    assertThat(mockAuditableVisitable.lastModifiedBy).isNull();
    assertThat(mockAuditableVisitable.lastModifiedOn).isNull();
    assertThat(mockAuditableVisitable.lastModifiedWith).isNull();

    new CommitVisitor(this.mockAuditable).visit(mockAuditableVisitable);

    assertThat(mockAuditableVisitable.lastModifiedBy).isNull();
    assertThat(mockAuditableVisitable.lastModifiedOn).isNull();
    assertThat(mockAuditableVisitable.lastModifiedWith).isNull();

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
