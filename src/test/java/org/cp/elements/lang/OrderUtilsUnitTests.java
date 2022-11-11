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

import static org.cp.elements.lang.LangExtensions.assertThat;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.withSettings;

import org.junit.Test;

import org.cp.elements.lang.annotation.Order;

import org.mockito.quality.Strictness;

/**
 * Unit Tests for {@link OrderUtils}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Orderable
 * @see org.cp.elements.lang.Ordered
 * @see org.cp.elements.lang.OrderUtils
 * @see org.cp.elements.lang.annotation.Order
 * @since 1.0.0
 */
public class OrderUtilsUnitTests {

  @Test
  public void getOrderFromOrderableObjectReturningNumericValue() {

    Orderable<?> mockOrderable = mock(Orderable.class);

    doReturn(2).when(mockOrderable).getOrder();

    assertThat(OrderUtils.getOrder(mockOrderable)).isEqualTo(2);

    verify(mockOrderable, times(2)).getOrder();
    verifyNoMoreInteractions(mockOrderable);
  }

  @Test
  public void getOrderFromOrderableObjectReturningNonNumericValue() {

    Comparable<?> mockComparable = mock(Comparable.class);

    Orderable<?> mockOrderable = mock(Orderable.class);

    doReturn(mockComparable).when(mockOrderable).getOrder();

    assertThat(OrderUtils.getOrder(mockOrderable)).isEqualTo(Ordered.DEFAULT);

    verify(mockOrderable, times(1)).getOrder();
    verifyNoMoreInteractions(mockOrderable);
    verifyNoInteractions(mockComparable);
  }

  @Test
  public void getOrderFromOrderedObject() {

    Ordered mockOrdered = mock(Ordered.class);

    doReturn(4).when(mockOrdered).getIndex();

    assertThat(OrderUtils.getOrder(mockOrdered)).isEqualTo(4);

    verify(mockOrdered, times(1)).getIndex();
    verifyNoMoreInteractions(mockOrdered);
  }

  @Test
  public void getOrderFromOrderAnnotatedObject() {

    OrderAnnotatedType mockObject = mock(OrderAnnotatedType.class);

    assertThat(OrderUtils.getOrder(mockObject)).isEqualTo(8);
  }

  @Test
  public void getOrderFromObjectReturnsDefaultOrder() {

    assertThat(OrderUtils.getOrder(new Object())).isEqualTo(Ordered.DEFAULT);
    assertThat(OrderUtils.getOrder('x')).isEqualTo(Ordered.DEFAULT);
    assertThat(OrderUtils.getOrder(10)).isEqualTo(Ordered.DEFAULT);
    assertThat(OrderUtils.getOrder(Math.PI)).isEqualTo(Ordered.DEFAULT);
    assertThat(OrderUtils.getOrder("test")).isEqualTo(Ordered.DEFAULT);
  }

  @Test
  public void getOrderFromNullIsNullSafeReturnsDefaultOrder() {
    assertThat(OrderUtils.getOrder(null)).isEqualTo(Ordered.DEFAULT);
  }

  @Test
  public void getOrderFromOrderableOrderedAndOrderAnnotatedObject() {

    OrderableOrderedAndOrderAnnotatedType mockObject =
      mock(OrderableOrderedAndOrderAnnotatedType.class, withSettings().strictness(Strictness.LENIENT));

    doReturn(4).when(mockObject).getIndex();
    doReturn(8).when(mockObject).getOrder();

    assertThat(OrderUtils.getOrder(mockObject)).isEqualTo(8);

    verify(mockObject, times(2)).getOrder();
    verifyNoMoreInteractions(mockObject);
  }

  @Test
  public void getOrderFromOrderedAndOrderAnnotatedObject() {

    OrderedAndOrderAnnotatedType mockObject = mock(OrderedAndOrderAnnotatedType.class);

    doReturn(5).when(mockObject).getIndex();

    assertThat(OrderUtils.getOrder(mockObject)).isEqualTo(5);

    verify(mockObject, times(1)).getIndex();
    verifyNoMoreInteractions(mockObject);
  }

  @Order(8)
  interface OrderAnnotatedType { }

  @Order(2)
  interface OrderableOrderedAndOrderAnnotatedType extends Orderable<Integer>, Ordered { }

  @Order(4)
  interface OrderedAndOrderAnnotatedType extends Ordered { }

}
