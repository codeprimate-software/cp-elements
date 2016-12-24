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

package org.cp.elements.util;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Properties;

import org.junit.Test;

/**
 * Unit tests for {@link PropertiesUtils}.
 *
 * @author John Blum
 * @see java.util.Properties
 * @see org.junit.Test
 * @see org.cp.elements.util.PropertiesUtils
 * @since 1.0.0
 */
public class PropertiesUtilsTests {

  @Test
  public void singletonProperties() {
    Properties properties = PropertiesUtils.singletonProperties("one", "1");

    assertThat(properties).isNotNull();
    assertThat(properties.size()).isEqualTo(1);
    assertThat(properties.containsKey("one"));
    assertThat(properties.getProperty("one")).isEqualTo("1");
  }
}
