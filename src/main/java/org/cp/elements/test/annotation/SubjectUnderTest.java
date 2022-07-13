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
package org.cp.elements.test.annotation;

import java.lang.annotation.Annotation;
import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * The {@link SubjectUnderTest} annotation is a stereotype {@link Annotation} identifying an object that is the subject
 * of a test case or an entire test class, i.e. the Subject Under Test (SUT).
 *
 * This {@link Annotation} can be used on instance fields, instance methods, local variables or method parameters.
 *
 * @author John Blum
 * @see java.lang.annotation.Annotation
 * @see java.lang.annotation.Documented
 * @see java.lang.annotation.Inherited
 * @see java.lang.annotation.Retention
 * @see java.lang.annotation.RetentionPolicy#RUNTIME
 * @see java.lang.annotation.Target
 * @since 1.0.0
 */
@Documented
@Inherited
@SuppressWarnings("unused")
@Retention(RetentionPolicy.RUNTIME)
@Target({
  ElementType.ANNOTATION_TYPE,
  ElementType.FIELD,
  ElementType.LOCAL_VARIABLE,
  ElementType.METHOD,
  ElementType.PARAMETER,
  ElementType.TYPE
})
public @interface SubjectUnderTest {

}
