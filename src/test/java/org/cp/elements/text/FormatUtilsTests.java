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

package org.cp.elements.text;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.Test;

/**
 * Unit tests for {@link FormatUtils}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.text.FormatUtils
 * @since 1.0.0
 */
public class FormatUtilsTests {

  @Test
  public void formatPlainText() {
    assertThat(FormatUtils.format("This is plain text!", true, 'X', 1, Math.PI, "test"))
      .isEqualTo("This is plain text!");
  }

  @Test
  public void formatMessageFormattedText() {
    assertThat(FormatUtils.format("This is {0} {1} text{2}", "message", "formatted", "!"))
      .isEqualTo("This is message formatted text!");
  }

  @Test
  public void formatStringFormattedText() {
    assertThat(FormatUtils.format("This is %1$s %2$s text%3$s", "string", "formatted", "!"))
      .isEqualTo("This is string formatted text!");
  }

  @Test
  public void formatStringAndFormattedText() {
    assertThat(FormatUtils.format("This is %1$s {1} text%3$s", "string and message", "formatted", "!"))
      .isEqualTo("This is string and message formatted text!");
  }
}
