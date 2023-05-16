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
package org.cp.elements.beans;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.beans.BeanDescriptor;
import java.beans.BeanInfo;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link DescribableBean}.
 *
 * @author John Blum
 * @see java.beans.BeanDescriptor
 * @see java.beans.BeanInfo
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see DescribableBean
 * @since 1.0.0
 */
public class DescribableBeanUnitTests {

  @Test
  public void getDescriptionCallsBeanDescriptorGetShortDescription() {

    BeanDescriptor mockBeanDescriptor = mock(BeanDescriptor.class);

    doReturn("Mock Description").when(mockBeanDescriptor).getShortDescription();

    DescribableBean bean = mock(DescribableBean.class);

    doReturn(mockBeanDescriptor).when(bean).getDescriptor();
    doCallRealMethod().when(bean).getDescription();

    assertThat(bean.getDescription()).isEqualTo("Mock Description");

    verify(mockBeanDescriptor, times(1)).getShortDescription();
    verifyNoMoreInteractions(mockBeanDescriptor);
  }

  @Test
  public void getDescriptorCallsDescribe() {

    BeanInfo mockBeanInfo = mock(BeanInfo.class);

    BeanDescriptor mockBeanDescriptor = mock(BeanDescriptor.class);

    DescribableBean bean = mock(DescribableBean.class);

    doReturn(mockBeanDescriptor).when(mockBeanInfo).getBeanDescriptor();
    doReturn(mockBeanInfo).when(bean).describe();
    doCallRealMethod().when(bean).getDescriptor();

    assertThat(bean.getDescriptor()).isEqualTo(mockBeanDescriptor);

    verify(mockBeanInfo, times(1)).getBeanDescriptor();
    verifyNoMoreInteractions(mockBeanInfo);
    verifyNoInteractions(mockBeanDescriptor);
  }

  @Test
  public void getSummaryCallsBeanDescriptorGetName() {

    BeanDescriptor mockBeanDescriptor = mock(BeanDescriptor.class);

    doReturn("Test Summary").when(mockBeanDescriptor).getName();

    DescribableBean bean = mock(DescribableBean.class);

    doReturn(mockBeanDescriptor).when(bean).getDescriptor();
    doCallRealMethod().when(bean).getSummary();

    assertThat(bean.getSummary()).isEqualTo("Test Summary");

    verify(mockBeanDescriptor, times(1)).getName();
    verifyNoMoreInteractions(mockBeanDescriptor);
  }
}
