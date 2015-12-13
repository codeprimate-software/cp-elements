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
