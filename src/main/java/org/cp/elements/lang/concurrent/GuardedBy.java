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

package org.cp.elements.lang.concurrent;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

/**
 * The {@link GuardedBy} annotation declares that a Java {@link Class} {@link Field} or {@link Method}
 * is guarded by the "named" lock in a multi-threaded, highly concurrent application.
 *
 * @author John J. Blum
 * @see java.lang.annotation.Documented
 * @see java.lang.annotation.ElementType#FIELD
 * @see java.lang.annotation.ElementType#METHOD
 * @see java.lang.annotation.Inherited
 * @see java.lang.annotation.RetentionPolicy#SOURCE
 * @since 1.0.0
 */
@Documented
@Inherited
@Retention(RetentionPolicy.SOURCE)
@Target({ElementType.FIELD, ElementType.METHOD})
@SuppressWarnings("unused")
public @interface GuardedBy {

  /**
   * Name of the Java lock.
   *
   * @return a {@link String} containing the name of the Java lock.
   */
  String value();

}
