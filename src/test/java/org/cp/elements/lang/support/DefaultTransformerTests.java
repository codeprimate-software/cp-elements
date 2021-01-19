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
package org.cp.elements.lang.support;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.Test;

/**
 * Unit Tests for {@link DefaultTransformer}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.lang.support.DefaultTransformer
 * @since 1.0.0
 */
public class DefaultTransformerTests {

  private DefaultTransformer<Object> transformer = DefaultTransformer.INSTANCE;

  @Test
  public void transformReturnsObjectUnaltered() {

    assertThat(transformer.transform(null)).isNull();
    assertThat(transformer.transform(Boolean.TRUE)).isEqualTo(Boolean.TRUE);
    assertThat(transformer.transform('x')).isEqualTo('x');
    assertThat(transformer.transform(2)).isEqualTo(2);
    assertThat(transformer.transform(Math.PI)).isEqualTo(Math.PI);
    assertThat(transformer.transform("test")).isEqualTo("test");
  }
}
