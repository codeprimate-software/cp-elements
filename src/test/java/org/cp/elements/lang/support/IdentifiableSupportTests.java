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
 * Unit tests for {@link IdentifiableSupport}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see IdentifiableSupport
 * @since 1.0.0
 */
public class IdentifiableSupportTests {

  @Test
  @SuppressWarnings("unchecked")
  public void setAndGetId() {

    IdentifiableSupport<Integer> mockIdentifiableSupport = new TestIdentifiableSupport();

    assertThat(mockIdentifiableSupport.getId()).isNull();

    mockIdentifiableSupport.setId(1);

    assertThat(mockIdentifiableSupport.getId()).isEqualTo(1);

    mockIdentifiableSupport.setId(null);

    assertThat(mockIdentifiableSupport.getId()).isNull();
  }

  static final class TestIdentifiableSupport extends IdentifiableSupport<Integer> { }

}
