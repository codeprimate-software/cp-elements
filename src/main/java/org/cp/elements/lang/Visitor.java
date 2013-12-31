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

package org.cp.elements.lang;

/**
 * The Visitor interface define a contract for objects who's classes implement this interface in order to walk
 * an object graph for carrying out some operation of evalution of particular objects or types of objects.  The Visitor
 * interface is an expression of the Visitor design pattern separating algorithm from object structure.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.lang.Visitable
 * @since 1.0.0
 */
public interface Visitor {

  /**
   * Visits the Visitable object in order to perform a function or evaluation of the target object.
   * <p/>
   * @param visitable the Visitable object visited by this Visitor.
   */
  public void visit(Visitable visitable);

}
