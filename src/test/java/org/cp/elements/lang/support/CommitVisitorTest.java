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
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Calendar;

import org.cp.elements.lang.Auditable;
import org.cp.elements.lang.Visitable;
import org.cp.elements.test.TestUtils;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

/**
 * The CommitVisitorTest class is a test suite of test cases testing the contract and functionality
 * of the CommitVisitor class.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
 * @see org.cp.elements.lang.Auditable
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.Visitor
 * @see org.cp.elements.lang.support.CommitVisitor
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class CommitVisitorTest {

  @Mock
  private Auditable mockAuditable;

  @Test
  public void isCommitableWithAuditable() {
    assertTrue(new CommitVisitor().isCommitable(mockAuditable));
    assertTrue(new CommitVisitor(mockAuditable).isCommitable(mockAuditable));
  }

  @Test
  public void isCommitableWithNonAuditable() {
    assertFalse(new CommitVisitor().isCommitable(new Object()));
    assertFalse(new CommitVisitor(mockAuditable).isCommitable(new Object()));
  }

  @Test
  public void isCommitableWithNonTargetedAuditable() {
    assertFalse(new CommitVisitor(mockAuditable).isCommitable(mock(Auditable.class)));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void visit() {
    Calendar expectedDateTime = TestUtils.createCalendar(2014, Calendar.DECEMBER, 17);

    AuditableVisitable<String, String> mockAuditableVisitable = mock(AuditableVisitable.class);

    when(mockAuditableVisitable.getModifiedBy()).thenReturn("ExpectedUser");
    when(mockAuditableVisitable.getModifiedDateTime()).thenReturn(expectedDateTime);
    when(mockAuditableVisitable.getModifyingProcess()).thenReturn("ExpectedProcess");

    assertNull(mockAuditableVisitable.lastModifiedBy);
    assertNull(mockAuditableVisitable.lastModifiedDateTime);
    assertNull(mockAuditableVisitable.lastModifyingProcess);

    new CommitVisitor().visit(mockAuditableVisitable);

    assertEquals("ExpectedUser", mockAuditableVisitable.lastModifiedBy);
    assertEquals(expectedDateTime, mockAuditableVisitable.lastModifiedDateTime);
    assertEquals("ExpectedProcess", mockAuditableVisitable.lastModifyingProcess);

    Calendar updatedExpectedDateTime = TestUtils.createCalendar(2014, Calendar.DECEMBER, 18);

    when(mockAuditableVisitable.getModifiedBy()).thenReturn("UpdatedExpectedUser");
    when(mockAuditableVisitable.getModifiedDateTime()).thenReturn(updatedExpectedDateTime);
    when(mockAuditableVisitable.getModifyingProcess()).thenReturn("UpdatedExpectedProcess");

    new CommitVisitor(mockAuditableVisitable).visit(mockAuditableVisitable);

    assertEquals("UpdatedExpectedUser", mockAuditableVisitable.lastModifiedBy);
    assertEquals(updatedExpectedDateTime, mockAuditableVisitable.lastModifiedDateTime);
    assertEquals("UpdatedExpectedProcess", mockAuditableVisitable.lastModifyingProcess);
  }

  @Test
  public void visitWithNonAuditable() {
    new CommitVisitor().visit(mock(Visitable.class));
  }

  @Test
  public void visitWithNonTargetedAuditable() {
    AuditableVisitable<?, ?> mockAuditableVisitable = mock(AuditableVisitable.class);

    assertNull(mockAuditableVisitable.lastModifiedBy);
    assertNull(mockAuditableVisitable.lastModifiedDateTime);
    assertNull(mockAuditableVisitable.lastModifyingProcess);

    new CommitVisitor(mockAuditable).visit(mockAuditableVisitable);

    assertNull(mockAuditableVisitable.lastModifiedBy);
    assertNull(mockAuditableVisitable.lastModifiedDateTime);
    assertNull(mockAuditableVisitable.lastModifyingProcess);

    verify(mockAuditableVisitable, never()).getModifiedBy();
    verify(mockAuditableVisitable, never()).getModifiedDateTime();
    verify(mockAuditableVisitable, never()).getModifyingProcess();
  }

  @SuppressWarnings("unused")
  public static abstract class AuditableVisitable<USER, PROCESS> implements Auditable<USER, PROCESS>, Visitable {

    private Calendar lastModifiedDateTime;
    private PROCESS lastModifyingProcess;
    private USER lastModifiedBy;

  }

}
