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

package org.cp.elements.lang;

/**
 * The {@link Visitor} interface define a contract for objects who's classes implement this interface in order to walk
 * an object graph for carrying out some operation of evalution of particular objects or types of objects.  The Visitor
 * interface is an expression of the Visitor design pattern separating algorithm from object structure.
 *
 * @author John J. Blum
 * @see java.lang.FunctionalInterface
 * @see org.cp.elements.lang.Visitable
 * @since 1.0.0
 */
@FunctionalInterface
public interface Visitor {

  /**
   * Visits the Visitable object in order to perform a function or evaluation of the target object.
   *
   * @param visitable the Visitable object visited by this Visitor.
   */
  void visit(Visitable visitable);

}
