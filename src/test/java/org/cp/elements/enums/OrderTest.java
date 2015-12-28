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

package org.cp.elements.enums;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Test;

/**
 * The OrderTest class is a test suite of test cases testing the contract and functionality of the Order Enum.
 *
 * @author John J. Blum
 * @see org.cp.elements.enums.Order
 * @see org.junit.Test
 * @since 1.0.0
 */
public class OrderTest {

  @Test
  public void testValueOf() {
    for (Order order : Order.values()) {
      assertEquals(order, Order.valueOf(order.name()));
    }
  }

  @Test
  public void testValueOfAbbreviation() {
    for (Order order : Order.values()) {
      assertEquals(order, Order.valueOfAbbreviation(order.getAbbreviation()));
    }

    assertEquals(Order.ASCENDING, Order.valueOfAbbreviation("asc"));
    assertEquals(Order.DESCENDING, Order.valueOfAbbreviation("Desc"));
    assertEquals(Order.ASCENDING, Order.valueOfAbbreviation("ASC"));
    assertNull(Order.valueOfAbbreviation("Ascending"));
    assertNull(Order.valueOfAbbreviation("DES"));
    assertNull(Order.valueOfAbbreviation("sideways"));
  }

  @Test
  public void testValueOfName() {
    for (Order order : Order.values()) {
      assertEquals(order, Order.valueOfName(order.getName()));
    }

    assertEquals(Order.ASCENDING, Order.valueOfName("ascending"));
    assertEquals(Order.DESCENDING, Order.valueOfName("Descending"));
    assertEquals(Order.ASCENDING, Order.valueOfName("ASCENDING"));
    assertNull(Order.valueOfName("ASC"));
    assertNull(Order.valueOfName("Descent"));
  }

}
