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

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for the Java language.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @since 1.0.0
 */
public class JavaUnitTests {

  @Test
  @SuppressWarnings("all")
  public void nullIsNotInstanceOfCloneable() {

    Object object = null;

    assertThat(null instanceof Cloneable).isFalse();
    assertThat(Cloneable.class.isInstance(null)).isFalse();
    //assertThat(object).isNotInstanceOf(Cloneable.class);
  }

  @Test
  @SuppressWarnings("all")
  public void objectIsNotInstanceOfCloneable() {

    Object object = new Object();

    assertThat(object instanceof Cloneable).isFalse();
    assertThat(Cloneable.class.isInstance(object)).isFalse();
    assertThat(object).isNotInstanceOf(Cloneable.class);
  }
}
