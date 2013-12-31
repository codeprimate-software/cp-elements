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

import java.util.Map;

import org.cp.elements.lang.Initable;
import org.cp.elements.lang.ParameterizedInitable;
import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.Visitor;

/**
 * The InitiableVisitor class is a Visitor implementation visiting an object object initializing each object in the
 * hierarchy.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.lang.Initable
 * @see org.cp.elements.lang.ParameterizedInitable
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.Visitor
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class InitableVisitor implements Visitor {

  private final Map<?, ?> parameters;

  private final Object[] args;

  public InitableVisitor() {
    this.args = null;
    this.parameters = null;
  }

  public InitableVisitor(final Object... args) {
    this.args = args;
    this.parameters = null;
  }

  public InitableVisitor(final Map<?, ?> parameters) {
    this.parameters = parameters;
    this.args = null;
  }

  public Object[] getArgs() {
    return args;
  }

  public Map<?, ?> getParameters() {
    return parameters;
  }

  @Override
  public void visit(final Visitable visitable) {
    if (visitable instanceof ParameterizedInitable) {
      if (getArgs() != null) {
        ((ParameterizedInitable) visitable).init(getArgs());
      }
      else if (getParameters() != null) {
        ((ParameterizedInitable) visitable).init(getParameters());
      }
      else {
        ((ParameterizedInitable) visitable).init();
      }
    }
    else if (visitable instanceof Initable) {
      ((Initable) visitable).init();
    }
  }

}
