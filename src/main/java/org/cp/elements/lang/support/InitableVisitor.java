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
 * The InitiableVisitor class is a Visitor implementation visiting a graph of objects initializing each object visited
 * in the hierarchy.
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

  /**
   * Constructs an instance of the InitableVisitor class with no argument or parameters.
   */
  public InitableVisitor() {
    this.args = null;
    this.parameters = null;
  }

  /**
   * Constructs an instance of the InitableVisitor class initialized with the specified arguments.
   * <p/>
   * @param args an array of Object arguments used by this Visitor when initializing both ParameterizedInitable
   * and Initable objects it visits.
   */
  public InitableVisitor(final Object... args) {
    this.args = args;
    this.parameters = null;
  }

  /**
   * Constructs an instance of the InitableVisitor class initialized with the specified parameters.
   * <p/>
   * @param parameters a Map of parameters used by this Visitor when initializing both ParameterizedInitable
   * and Initable objects it visits.
   * @see java.util.Map
   */
  public InitableVisitor(final Map<?, ?> parameters) {
    this.parameters = parameters;
    this.args = null;
  }

  /**
   * Gets the array of arguments used to initialize ParameterizedInitable and Initable objects visited by this Visitor.
   * <p/>
   * @return an array or arguments used to initialize ParameterizedInitable and Initable objects visited by this Visitor.
   */
  public Object[] getArguments() {
    return args;
  }

  /**
   * Gets the mapping of parameters used to initialize ParameterizedInitable and Initable objects visited
   * by this Visitor.
   * <p/>
   * @return the mapping of parameters used to initialize ParameterizedInitable and Initable objects visited
   * by this Visitor.
   * @see java.util.Map
   */
  public Map<?, ?> getParameters() {
    return parameters;
  }

  /**
   * Visits ParameterizedInitable and Initable objects in a graph initializing them with either the arguments
   * or parameters supplied to this Visitor before the visitation, or otherwise calls the no-arg init() method
   * if no arguments or parameters were supplied or the object is a plain Initable object.
   * <p/>
   * @param visitable the Visitable object visited by this Visitor.
   * @see org.cp.elements.lang.Initable#init()
   * @see org.cp.elements.lang.ParameterizedInitable#init()
   * @see org.cp.elements.lang.ParameterizedInitable#init(Object...)
   * @see org.cp.elements.lang.ParameterizedInitable#init(java.util.Map)
   * @see org.cp.elements.lang.Visitable
   */
  @Override
  public void visit(final Visitable visitable) {
    if (visitable instanceof ParameterizedInitable) {
      if (getParameters() != null) {
        ((ParameterizedInitable) visitable).init(getParameters());
      }
      else if (getArguments() != null) {
        ((ParameterizedInitable) visitable).init(getArguments());
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
