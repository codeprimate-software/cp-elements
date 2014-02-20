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

package org.cp.elements.util.search.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.cp.elements.util.search.Matcher;

/**
 * The @Searchable annotation annotates a Class with configuration meta-data used by the Searcher to determine the
 * Matcher to use for the elements to search during the search operation.
 * <p/>
 * @author John J. Blum
 * @see java.lang.annotation.Annotation
 * @see java.lang.annotation.Documented
 * @see java.lang.annotation.Inherited
 * @see java.lang.annotation.Retention
 * @see java.lang.annotation.Target
 * @see java.lang.annotation.ElementType#TYPE
 * @see java.lang.annotation.RetentionPolicy#RUNTIME
 * @see org.cp.elements.util.search.Searchable
 * @see org.cp.elements.util.search.Searcher
 * @since 1.0.0
 */
@Documented
@Inherited
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@SuppressWarnings("unused")
public @interface Searchable {

  /**
   * Searchable meta-data attribute to specify the name of method on the Class annotated with this annotation to use
   * to get the List of elements to search.
   * <p/>
   * @return the name of method on the Class annotated with this annotation to get the List of elements to search.
   * The default value is "asList".
   */
  public String listMethod() default "asList";

  /**
   * Searchable meta-data attribute defining the Matcher class to use when searching for elements in the collection.
   * <p/>
   * @return a Matcher class type used to match and find elements in the collection during the search.
   * @see java.lang.Class
   * @see org.cp.elements.util.search.Matcher
   */
  public Class<? extends Matcher> matcher() default Matcher.class;

}
