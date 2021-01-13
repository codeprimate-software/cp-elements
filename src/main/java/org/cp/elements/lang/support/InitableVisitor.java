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

import java.util.Map;

import org.cp.elements.lang.Initable;
import org.cp.elements.lang.ParameterizedInitable;
import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.Visitor;

/**
 * The InitiableVisitor class is a Visitor implementation visiting a graph of objects initializing each object visited
 * in the hierarchy.
 *
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
   *
   * @param args an array of Object arguments used by this Visitor when initializing both ParameterizedInitable
   * and Initable objects it visits.
   */
  public InitableVisitor(final Object... args) {
    this.args = args;
    this.parameters = null;
  }

  /**
   * Constructs an instance of the InitableVisitor class initialized with the specified parameters.
   *
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
   *
   * @return an array or arguments used to initialize ParameterizedInitable and Initable objects visited by this Visitor.
   */
  public Object[] getArguments() {
    return args;
  }

  /**
   * Gets the mapping of parameters used to initialize ParameterizedInitable and Initable objects visited
   * by this Visitor.
   *
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
   *
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
