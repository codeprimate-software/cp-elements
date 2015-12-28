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

package org.cp.elements.util.sort.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.Comparator;

/**
 * The @Sortable annotation annotates a Class with configuration meta-data used by the Sorter to determine the
 * Comparator to use for the elements when sorting during the sort operation.
 *
 * @author John J. Blum
 * @see java.lang.annotation.Annotation
 * @see java.lang.annotation.Documented
 * @see java.lang.annotation.Inherited
 * @see java.lang.annotation.Retention
 * @see java.lang.annotation.Target
 * @see java.lang.annotation.ElementType#TYPE
 * @see java.lang.annotation.RetentionPolicy#RUNTIME
 * @see org.cp.elements.util.sort.Sortable
 * @see org.cp.elements.util.sort.Sorter
 * @since 1.0.0
 */
@Documented
@Inherited
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@SuppressWarnings("unused")
public @interface Sortable {

  /**
   * Attribute to specify the name of method on the Class annotated with this annotation to use in order to get
   * the List of elements to sort.
   *
   * @return the name of method on the Class annotated with this annotation to get the List of elements to sort.
   * The default value is "asList".
   */
  String listMethod() default "asList";

  /**
   * Attribute defining the Comparator class to use when sorting and ordering the elements in the collection (List).
   *
   * @return a Comparator class type used to sort and order the elements in the collection (List) during the sort.
   * @see java.lang.Class
   * @see java.util.Comparator
   */
  Class<? extends Comparator> orderBy() default Comparator.class;

}
