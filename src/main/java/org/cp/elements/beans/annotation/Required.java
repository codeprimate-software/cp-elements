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
package org.cp.elements.beans.annotation;

import java.lang.annotation.Annotation;
import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * {@link Annotation} defining bean property metadata used to indicate that a bean property requires a value.
 *
 * @author John J. Blum
 * @see java.lang.annotation.Annotation
 * @see java.lang.annotation.Documented
 * @see java.lang.annotation.ElementType#ANNOTATION_TYPE
 * @see java.lang.annotation.ElementType#FIELD
 * @see java.lang.annotation.ElementType#METHOD
 * @see java.lang.annotation.ElementType#PARAMETER
 * @see java.lang.annotation.Retention
 * @see java.lang.annotation.RetentionPolicy#RUNTIME
 * @see java.lang.annotation.Target
 * @since 1.0.0
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({
  ElementType.ANNOTATION_TYPE,
  ElementType.FIELD,
  ElementType.METHOD,
  ElementType.PARAMETER
})
@SuppressWarnings("unused")
public @interface Required {

}
