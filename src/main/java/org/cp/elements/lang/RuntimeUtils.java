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
package org.cp.elements.lang;

/**
 * Abstract utility class used to inspect the runtime environment of the Java program.
 *
 * @author John Blum
 * @see java.lang.Runtime
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class RuntimeUtils {

  /**
   * Determines whether the Java program was initiated from Gradle.
   *
   * @return a boolean value indicating whether the Java program was initiated from Gradle.
   */
  public static boolean isRunWithGradle() {
    return StringUtils.hasText(System.getProperty("gradle.user.home"));
  }

  /**
   * Determines whether the Java program was initiated from Maven.
   *
   * @return a boolean value determining whether the Java program was initiated from Maven.
   */
  public static boolean isRunWithMaven() {
    return StringUtils.hasText(System.getenv("MAVEN_PROJECTBASEDIR"))
      || StringUtils.hasText(System.getProperty("maven.projectBasedir"));
  }
}
