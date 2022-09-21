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
package org.cp.elements.lang.annotation;

import java.lang.annotation.Annotation;
import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Java {@link Annotation} used to specify that a Java program {@link java.lang.reflect.Member element}, for example,
 * such as a {@link java.lang.reflect.Field} or a {@link java.lang.reflect.Method}) is an {@literal alias} to
 * some other Java program {@link java.lang.reflect.Member element}.
 *
 * @author John Blum
 * @see java.lang.annotation.Annotation
 * @since 1.0.0
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.ANNOTATION_TYPE, ElementType.FIELD, ElementType.METHOD, ElementType.TYPE })
@SuppressWarnings("unused")
public @interface Alias {

  /**
   * Returns a {@link String} containing {@literal name} of the Java program {@link java.lang.reflect.Member element}
   * for which this {@link Alias @Alias} annotated Java program {@link java.lang.reflect.Member element}
   * is an {@literal alias} for.
   *
   * @return a {@link String name} for the {@literal aliased} Java program {@link java.lang.reflect.Member element}.
   */
  String forMember();

}
