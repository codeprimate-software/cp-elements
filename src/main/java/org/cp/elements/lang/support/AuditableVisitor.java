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

  public AuditableVisitor(final USER user, final PROCESS process) {
    this(user, process, Calendar.getInstance());
  }

  public AuditableVisitor(final USER user, final PROCESS process, final Calendar dateTime) {
    Assert.notNull(user, "The user cannot be null!");
    Assert.notNull(process, "The process cannot be null!");
    this.user = user;
    this.process = process;
    this.dateTime = (dateTime != null ? dateTime : Calendar.getInstance());
  }

  public USER getUser() {
    return user;
  }

  public PROCESS getProcess() {
    return process;
  }

  public Calendar getDateTime() {
    return DateTimeUtils.clone(dateTime);
  }

  protected boolean isCreatedUnset(final Auditable auditable) {
    return (auditable.getCreatedBy() == null || auditable.getCreatedDateTime() == null);
  }

  protected boolean isNew(final Auditable auditable) {
    return (auditable instanceof Identifiable && ((Identifiable) auditable).isNew());
  }

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
