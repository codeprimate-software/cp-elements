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
 * The Transformer interface defines a contract for implementing classes who's objects transform data from one value
 * to another value of the same class type.
 *
 * @author John J. Blum
 * @param <T> the Class type of the data value (datum) to transform.
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Transformer<T> {

  /**
   * Transforms the given value of class type T into another value of class type T.
   *
   * @param value the value to transform.
   * @return the transformed value.
   */
  T transform(T value);

}
