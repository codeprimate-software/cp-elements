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
 *
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
   *
   * @return the name of method on the Class annotated with this annotation to get the List of elements to search.
   * The default value is "asList".
   */
  String listMethod() default "asList";

  /**
   * Searchable meta-data attribute defining the Matcher class to use when searching for elements in the collection.
   *
   * @return a Matcher class type used to match and find elements in the collection during the search.
   * @see java.lang.Class
   * @see org.cp.elements.util.search.Matcher
   */
  Class<? extends Matcher> matcher() default Matcher.class;

}
