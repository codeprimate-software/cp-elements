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

import java.util.Calendar;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Auditable;
import org.cp.elements.lang.DateTimeUtils;
import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.Visitor;

/**
 * The AuditableVisitor class is a Visitor implementation visiting the an object graph to set the auditable properties
 * of an Auditable object.
 * <p/>
 * @author John J. Blum
 * @see java.util.Calendar
 * @see org.cp.elements.lang.Auditable
 * @see org.cp.elements.lang.Identifiable
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.Visitor
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AuditableVisitor<USER, PROCESS> implements Visitor {

  private final Calendar dateTime;

  private final PROCESS process;

  private final USER user;

  /**
   * Constructs an instance of the AuditableVisitor class initialized with a creating/modifying user and process.
   * <p/>
   * @param user the user authorized and responsible for changing the Auditable object.
   * @param process the process authorized and responsible for changing the Auditable object.
   * @see #AuditableVisitor(Object, Object, java.util.Calendar)
   */
  public AuditableVisitor(final USER user, final PROCESS process) {
    this(user, process, Calendar.getInstance());
  }

  /**
   * Constructs an instance of the AuditableVisitor class initialized with a creating/modifying user and process
   * as well as the date and time the Auditable object was created or modified.
   * <p/>
   * @param user the user authorized and responsible for changing the Auditable object.
   * @param process the process authorized and responsible for changing the Auditable object.
   * @param dateTime a Calendar instance indicating the date and time the Auditable object was changed.
   * @see #AuditableVisitor(Object, Object)
   * @see java.util.Calendar
   */
  public AuditableVisitor(final USER user, final PROCESS process, final Calendar dateTime) {
    Assert.notNull(user, "The user cannot be null!");
    Assert.notNull(process, "The process cannot be null!");
    this.user = user;
    this.process = process;
    this.dateTime = (dateTime != null ? dateTime : Calendar.getInstance());
  }

  /**
   * Gets the user authorized and responsible for changing the Auditable object.
   * <p/>
   * @return the user authorized and responsible for changing the Auditable object.
   */
  public USER getUser() {
    return user;
  }

  /**
   * Gets the process authorized and responsible for changing the Auditable object.
   * <p/>
   * @return the process authorized and responsible for changing the Auditable object.
   */
  public PROCESS getProcess() {
    return process;
  }

  /**
   * Gets the date and time that the Auditable object was changed.
   * <p/>
   * @return a Calendar instance indicating the date and time the Auditable object was changed.
   * @see java.util.Calendar
   */
  public Calendar getDateTime() {
    return DateTimeUtils.clone(dateTime);
  }

  /**
   * Determines whether the created Auditable properties are currently unset, specifically only evaluated the
   * createdBy and createdDateTime properties.
   * <p/>
   * @param auditable the Auditable object being evaluated for unset created properties.
   * @return a boolean value indicating whether the Auditable object's created properties are currently unset.
   * @see org.cp.elements.lang.Auditable#getCreatedBy()
   * @see org.cp.elements.lang.Auditable#getCreatedDateTime()
   */
  protected boolean isCreatedUnset(final Auditable auditable) {
    return (auditable.getCreatedBy() == null || auditable.getCreatedDateTime() == null);
  }

  /**
   * Determines whether the specified Auditable object is Identifiable and new.
   * <p/>
   * @param auditable the Auditable object being evaluated as an instance of the Identifiable interface and whether
   * the Auditable object is new.
   * @return a boolean value indicating whether the Auditable object is Identifiable and new.
   * @see org.cp.elements.lang.Identifiable#isNew()
   */
  protected boolean isNew(final Auditable auditable) {
    return (auditable instanceof Identifiable && ((Identifiable) auditable).isNew());
  }

  /**
   * Visits Auditable objects in an object graph/hierarchy setting auditable information (created/modified
   * by/date-time/process).
   * <p/>
   * @param visitable the Visitable object visited by this Visitor.
   * @see org.cp.elements.lang.Auditable
   */
  @Override
  @SuppressWarnings("unchecked")
  public void visit(final Visitable visitable) {
    if (visitable instanceof Auditable) {
      final Auditable<USER, PROCESS> auditable = (Auditable<USER, PROCESS>) visitable;

      if (isNew(auditable) || isCreatedUnset(auditable)) {
        auditable.setCreatedBy(getUser());
        auditable.setCreatedDateTime(getDateTime());
        auditable.setCreatingProcess(getProcess());
      }

      if (auditable.isModified()) {
        auditable.setModifiedBy(getUser());
        auditable.setModifiedDateTime(getDateTime());
        auditable.setModifyingProcess(getProcess());
      }
    }
  }

}
