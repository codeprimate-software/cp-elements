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

import java.util.Calendar;

import org.cp.elements.lang.Auditable;
import org.cp.elements.lang.Visitable;
import org.cp.elements.test.AbstractMockingTestSuite;
import org.cp.elements.test.TestUtils;
import org.jmock.Expectations;
import org.junit.Test;

/**
 * The CommitVisitorTest class is a test suite of test cases testing the contract and functionality
 * of the CommitVisitor class.
 *
 * @author John J. Blum
 * @see org.jmock.Mockery
 * @see org.junit.Test
 * @see org.cp.elements.lang.Auditable
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.Visitor
 * @see org.cp.elements.lang.support.CommitVisitor
 * @since 1.0.0
 */
public class CommitVisitorTest extends AbstractMockingTestSuite {

  @Test
  public void testIsCommittableWithAuditable() {
    Auditable<?, ?> mockAuditable = mock(Auditable.class, "testIsCommittableWithAuditable");

    assertTrue(new CommitVisitor().isCommittable(mockAuditable));
    assertTrue(new CommitVisitor(mockAuditable).isCommittable(mockAuditable));
  }

  @Test
  public void testIsCommittableWithNonAuditable() {
    assertFalse(new CommitVisitor().isCommittable(new Object()));
  }

  @Test
  public void testIsCommittableWithNonTargetedAuditable() {
    assertFalse(new CommitVisitor(mock(Auditable.class, "testIsCommittableWithNonTargetedAuditable.target"))
      .isCommittable(mock(Auditable.class, "testIsCommittableWithNonTargetedAuditable.visitable")));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testVisit() {
    final Calendar expectedDateTime = TestUtils.createCalendar(2014, Calendar.DECEMBER, 17);

    final AuditableVisitable<String, String> mockAuditableVisitable = mock(AuditableVisitable.class, "testVisit");

    checking(new Expectations() {{
      oneOf(mockAuditableVisitable).getModifiedBy();
      will(returnValue("ExpectedUser"));
      oneOf(mockAuditableVisitable).getModifiedDateTime();
      will(returnValue(expectedDateTime));
      oneOf(mockAuditableVisitable).getModifyingProcess();
      will(returnValue("ExpectedProcess"));
    }});

    assertNull(mockAuditableVisitable.lastModifiedBy);
    assertNull(mockAuditableVisitable.lastModifiedDateTime);
    assertNull(mockAuditableVisitable.lastModifyingProcess);

    new CommitVisitor().visit(mockAuditableVisitable);

    assertEquals("ExpectedUser", mockAuditableVisitable.lastModifiedBy);
    assertEquals(expectedDateTime, mockAuditableVisitable.lastModifiedDateTime);
    assertEquals("ExpectedProcess", mockAuditableVisitable.lastModifyingProcess);

    final Calendar differentExpectedDateTime = TestUtils.createCalendar(2014, Calendar.DECEMBER, 18);

    checking(new Expectations() {{
      oneOf(mockAuditableVisitable).getModifiedBy();
      will(returnValue("DifferentExpectedUser"));
      oneOf(mockAuditableVisitable).getModifiedDateTime();
      will(returnValue(differentExpectedDateTime));
      oneOf(mockAuditableVisitable).getModifyingProcess();
      will(returnValue("DifferentExpectedProcess"));
    }});

    new CommitVisitor(mockAuditableVisitable).visit(mockAuditableVisitable);

    assertEquals("DifferentExpectedUser", mockAuditableVisitable.lastModifiedBy);
    assertEquals(differentExpectedDateTime, mockAuditableVisitable.lastModifiedDateTime);
    assertEquals("DifferentExpectedProcess", mockAuditableVisitable.lastModifyingProcess);
  }

  @Test
  public void testVisitWithNonAuditable() {
    new CommitVisitor().visit(mock(Visitable.class, "testVisitWithNonAuditable"));
  }

  @Test
  public void testVisitWithNonTargetedAuditable() {
    final AuditableVisitable<?, ?> mockAuditableVisitable = mock(AuditableVisitable.class, "testVisitWithNonTargetedAuditable");

    checking(new Expectations() {{
      never(mockAuditableVisitable).getModifiedBy();
      never(mockAuditableVisitable).getModifiedDateTime();
      never(mockAuditableVisitable).getModifyingProcess();
    }});

    assertNull(mockAuditableVisitable.lastModifiedBy);
    assertNull(mockAuditableVisitable.lastModifiedDateTime);
    assertNull(mockAuditableVisitable.lastModifyingProcess);

    new CommitVisitor(new Object()).visit(mockAuditableVisitable);

    assertNull(mockAuditableVisitable.lastModifiedBy);
    assertNull(mockAuditableVisitable.lastModifiedDateTime);
    assertNull(mockAuditableVisitable.lastModifyingProcess);
  }

  @SuppressWarnings("unused")
  public static abstract class AuditableVisitable<USER, PROCESS> implements Auditable<USER, PROCESS>, Visitable {

    private Calendar lastModifiedDateTime;
    private PROCESS lastModifyingProcess;
    private USER lastModifiedBy;

  }

}
