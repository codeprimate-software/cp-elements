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
 * Java {@link Annotation} used to qualify a reference to an {@link Object} used as a collaborator
 * ({@literal has-a} relationship) in another {@link Object} to carry out its function or service.
 *
 * This is useful in application service component type design when {@literal programming to interfaces},
 * and more than 1 application service component of the {@link Class#isInterface() interface type} exists
 * or is available to be used as a collaborator in the dependent {@link Object}.
 *
 * @author John Blum
 * @see java.lang.annotation.Annotation
 * @since 1.0.0
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({
  ElementType.ANNOTATION_TYPE,
  ElementType.FIELD,
  ElementType.METHOD,
  ElementType.PARAMETER,
  ElementType.TYPE
})
@SuppressWarnings("unused")
public @interface Qualifier {

  /**
   * {@link String Name} referring to the specific application service component instance to use
   * when multiple instances of the application service component / collaborator exist or are available.
   *
   * @return {@link String Name} referring to the specific application service component instance to configure
   * as the collaborator used in another {@link Object}.
   */
  String name();

}
