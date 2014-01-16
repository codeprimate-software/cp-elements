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

import org.cp.elements.lang.Auditable;
import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.Visitor;

/**
 * The IsModifiedVisitor class is a Visitor implementation that visits a Vistiable object graph/hierarchy in search of
 * any Auditable objects that may have been modified.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.lang.Auditable
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.Visitor
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class IsModifiedVisitor implements Visitor {

  private volatile boolean modified = false;

  /**
   * Determines whether any of the Auditable, Visitable object visited were modified.
   * <p/>
   * @return a boolean value indicating whether any of the Auditable, Visitable objects in the object graph hierarchy
   * were modified.
   */
  public boolean isModified() {
    return modified;
  }

  /**
   * Visits all Auditable, Visitable objects in an object graph hierarchy in search of any modified objects.
   * <p/>
   * @param visitable the Visitable object visited by this Visitor.
   * @see org.cp.elements.lang.Auditable#isModified()
   */
  @Override
  public void visit(final Visitable visitable) {
    if (visitable instanceof Auditable) {
      modified |= ((Auditable) visitable).isModified();
    }
  }

}
