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

package org.cp.elements.lang;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.function.Function;

import org.junit.Test;

/**
 * Unit tests for the {@link Transformer} interface.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.cp.elements.lang.Transformer
 * @since 1.0.0
 */
public class TransformerTests {

  @Test
  public void asFunctionIsSuccessful() {

    Transformer<String> uppercaseTransformer = String::toUpperCase;

    Function<String, String> function = uppercaseTransformer.asFunction();

    assertThat(function).isNotNull();
    assertThat(function.apply("test")).isEqualTo("TEST");
  }
}
