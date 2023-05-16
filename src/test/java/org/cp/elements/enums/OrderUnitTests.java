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
package org.cp.elements.enums;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Arrays;

import org.cp.elements.lang.StringUtils;
import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link Order}.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.enums.Order
 * @since 1.0.0
 */
public class OrderUnitTests {

  @Test
  public void valueOfReturnsOrder() {

    Arrays.stream(Order.values()).forEach(order -> {
      assertThat(Order.valueOf(order.name())).isEqualTo(order);
      assertThat(order.getName()).isEqualTo(StringUtils.capitalize(order.name().toLowerCase()));
      assertThat(order.getAbbreviation()).startsWith(order.name().toUpperCase().substring(0, 3));
    });
  }

  @Test
  public void valueOfAbbreviationIsCorrect() {

    Arrays.stream(Order.values()).forEach(order ->
      assertThat(Order.valueOfAbbreviation(order.getAbbreviation())).isEqualTo(order));
  }

  @Test
  public void valueOfAbbreviationIsLenient() {

    assertThat(Order.valueOfAbbreviation("asc")).isEqualTo(Order.ASCENDING);
    assertThat(Order.valueOfAbbreviation("Desc")).isEqualTo(Order.DESCENDING);
    assertThat(Order.valueOfAbbreviation("ASC")).isEqualTo(Order.ASCENDING);
  }

  @Test
  public void valueOfAbbreviationIsNullSafeReturnsNull() {
    assertThat(Order.valueOfAbbreviation(null)).isNull();
  }

  @Test
  public void valueOfAbbreviationUsingNameReturnsNull() {

    assertThat(Order.valueOfAbbreviation("Ascending")).isNull();
    assertThat(Order.valueOfAbbreviation("DESCending")).isNull();
  }

  @Test
  public void valueOfInvalidAbbreviationsReturnsNull() {

    assertThat(Order.valueOfAbbreviation("")).isNull();
    assertThat(Order.valueOfAbbreviation("  ")).isNull();
    assertThat(Order.valueOfAbbreviation("ascend")).isNull();
    assertThat(Order.valueOfAbbreviation("Descend")).isNull();
    assertThat(Order.valueOfAbbreviation("SIDE")).isNull();
    assertThat(Order.valueOfAbbreviation("Sideways")).isNull();
  }

  @Test
  public void valueOfNameIsCorrect() {

    Arrays.stream(Order.values()).forEach(order ->
      assertThat(Order.valueOfName(order.getName())).isEqualTo(order));
  }

  @Test
  public void valueOfNameIsLenient() {

    assertThat(Order.valueOfName("ascending")).isEqualTo(Order.ASCENDING);
    assertThat(Order.valueOfName("Descending")).isEqualTo(Order.DESCENDING);
    assertThat(Order.valueOfName("ASCENDING")).isEqualTo(Order.ASCENDING);
  }

  @Test
  public void valueOfNameIsNullSafeReturnsNull() {
    assertThat(Order.valueOfName(null)).isNull();
  }

  @Test
  public void valueOfNameUsingAbbreviationReturnsNull() {

    assertThat(Order.valueOfName("asc")).isNull();
    assertThat(Order.valueOfName("Desc")).isNull();
    assertThat(Order.valueOfName("ASC")).isNull();
    assertThat(Order.valueOfName("side")).isNull();
  }

  @Test
  public void valueOfInvalidNameReturnsNull() {

    assertThat(Order.valueOfName("")).isNull();
    assertThat(Order.valueOfName("  ")).isNull();
    assertThat(Order.valueOfName("Horizontal")).isNull();
    assertThat(Order.valueOfName("Sideways")).isNull();
    assertThat(Order.valueOfName("Vertical")).isNull();
  }
}
