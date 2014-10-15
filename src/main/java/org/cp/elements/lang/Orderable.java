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
 * The Orderable interface defines a contract for classes whose objects can be organized in an ordered context, such as
 * an ordered data structure using arrays or Lists.  In general, the order of objects can be applied in many different
 * ways including sort order, prioritization, or to ascertain precedence between Orderable objects of the same type.
 * <p/>
 * @author John J. Blum
 * @param <T> a type parameter indicating the class of the Orderable type.
 * @see java.lang.Comparable
 * @see org.cp.elements.lang.Ordered
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Orderable<T extends Comparable<T>> {

  /**
   * Gets the value of the order property which indicates the order of this object relative to it's peers.
   * <p/>
   * @return a Comparable value of type T indicating this object's order of precedence.
   */
  T getOrder();

}
