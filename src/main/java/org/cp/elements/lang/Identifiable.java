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
 * The Identifiable interface defines a contract for uniquely identifying objects of the specified type.
 * <p/>
 * @author John J. Blum
 * @see java.lang.Comparable
 * @see org.cp.elements.lang.Auditable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Identifiable<T extends Comparable<T>> {

  /**
   * Gets the identifier uniquely identifying this object.
   * <p/>
   * @return the value of type T indicating this object's assigned unique identifier.
   */
  T getId();

  /**
   * Sets the identifier uniquely identifying this object.
   * <p/>
   * @param id a value of type T assigned as this object's unique identifier.
   */
  void setId(T id);

  /**
   * Determines whether this Identifiable object is new, which is signified by a null identifier.
   * <p/>
   * @return a boolean value indicating whether this Identifiable object is new.
   */
  boolean isNew();

}
