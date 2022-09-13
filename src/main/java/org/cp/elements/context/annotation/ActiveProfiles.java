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
package org.cp.elements.context.annotation;

import java.lang.annotation.Annotation;
import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Java {@link Annotation} used to declare the {@link Profile Profiles} that are active in an Java application (program)
 * or test class.
 *
 * @author John Blum
 * @see java.lang.annotation.Annotation
 * @see java.lang.annotation.Documented
 * @see java.lang.annotation.Inherited
 * @see java.lang.annotation.Retention
 * @see java.lang.annotation.RetentionPolicy#RUNTIME
 * @see java.lang.annotation.Target
 * @see java.lang.annotation.ElementType#ANNOTATION_TYPE
 * @see java.lang.annotation.ElementType#TYPE
 * @see org.cp.elements.context.annotation.Profile
 * @since 1.0.0
 */
@Documented
@Inherited
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.ANNOTATION_TYPE, ElementType.TYPE })
@SuppressWarnings("unused")
public @interface ActiveProfiles {

  /**
   * Array of {@link String Strings} declaring the {@literal names} of {@link Profile Profiles} that should be
   * activated in this Java application (program) or test.
   *
   * @return an array of {@link String Strings} declaring the {@literal names} of {@link Profile Profiles}
   * that should be activated in this Java application (program) or test.
   */
  String[] names();

}
