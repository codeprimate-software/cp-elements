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
package org.cp.elements.service.loader;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.cp.elements.lang.ThrowableAssertions.assertThatThrowableOfType;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.awt.Point;
import java.util.function.Predicate;

import org.junit.Test;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.service.ServiceUnavailableException;
import org.cp.elements.service.annotation.Service;
import org.cp.elements.service.loader.provider.GoogleGeocodingService;
import org.cp.elements.service.loader.provider.TestService;
import org.cp.elements.service.loader.provider.TomTomGeocodingService;

/**
 * Unit Tests for {@link ServiceLoaderSupport}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.service.loader.ServiceLoaderSupport
 * @since 1.0.0
 */
public class ServiceLoaderSupportUnitTests {

  @Test
  public void getServiceClassLoaderIsSameAsCurrentThreadContextClassLoader() {
    assertThat(MockService.getInstance().getClassLoader()).isSameAs(Thread.currentThread().getContextClassLoader());
  }

  @Test
  @SuppressWarnings("unchecked")
  public void getServiceInstanceCallsGetServiceInstanceWithPredicate() {

    MockService mockService = spy(MockService.getInstance());

    ServiceLoaderSupport<MockService> serviceLoaderSupport = mock(ServiceLoaderSupport.class);

    doCallRealMethod().when(serviceLoaderSupport).getServiceInstance();
    doReturn(mockService).when(serviceLoaderSupport).getServiceInstance(any(Predicate.class));

    assertThat(serviceLoaderSupport.getServiceInstance()).isEqualTo(mockService);

    verify(serviceLoaderSupport, times(1)).getServiceInstance();
    verify(serviceLoaderSupport, times(1)).getServiceInstance(isA(Predicate.class));
    verifyNoMoreInteractions(serviceLoaderSupport);
    verifyNoInteractions(mockService);
  }

  @Test
  public void getServiceInstanceWithMultipleAvailableServiceInstances() {

    MockGeocodingService mockGeocodingService = MockGeocodingService.getLoader().getServiceInstance();

    assertThat(mockGeocodingService).isInstanceOf(GoogleGeocodingService.class);

    Point coordinates = mockGeocodingService.geocode("200 This Is The Way Seattle, WA 55055");

    assertThat(coordinates).isNotNull();
    assertThat(coordinates).isEqualTo(new Point(4, 16));

    String address = mockGeocodingService.reverseGeocode(coordinates);

    assertThat(address).isEqualTo("100 Main St. Portland, OR 97205");
  }

  @Test
  public void getServiceInstanceWithQualifierReturnsTargetedServiceInstance() {

    MockGeocodingService mockGeocodingService =
      MockGeocodingService.getLoader().getServiceInstance("TomTom");

    assertThat(mockGeocodingService).isInstanceOf(TomTomGeocodingService.class);

    Point coordinates = mockGeocodingService.geocode("404 Walk This Way, San Diego, CA 69001");

    assertThat(coordinates).isNotNull();
    assertThat(coordinates).isEqualTo(new Point(16, 64));

    String address = mockGeocodingService.reverseGeocode(coordinates);

    assertThat(address).isEqualTo("1024 One Way Portland, OR 97205");
  }

  @Test
  public void getServiceInstanceWithNullPredicateThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> MockService.getInstance().getServiceInstance((Predicate<MockService>) null))
      .withMessage("A Predicate used to match the service instance is required")
      .withNoCause();
  }

  @Test
  public void getTestServiceInstance() {

    MockService testService = MockService.getInstance().getServiceInstance();

    assertThat(testService).isInstanceOf(TestService.class);
  }

  @Test
  public void getUnavailableServiceInstanceThrowsServiceUnavailableException() {

    assertThatThrowableOfType(ServiceUnavailableException.class)
      .isThrownBy(args -> new UnavailableService().getServiceInstance())
      .havingMessageMatching("Failed to find a service instance matching Predicate \\[.*]")
      .withNoCause();
  }

  @Test
  public void getUnavailableServiceInstanceWithQualifierThrowsUnavailableServiceException() {

    assertThatThrowableOfType(ServiceUnavailableException.class)
      .isThrownBy(args -> MockGeocodingService.getLoader().getServiceInstance("testQualifier"))
      .havingMessageContaining("Failed to find a service instance with named qualifier [testQualifier]")
      .causedBy(ServiceUnavailableException.class)
      .havingMessageMatching("Failed to find a service instance matching Predicate \\[.*]")
      .withNoCause();
  }

  @Test
  public void getTypeReturnsTypeOfServiceLoaderSupportImplementingClass() {

    MockService mockService = MockService.getInstance();

    assertThat(mockService).isNotNull();
    assertThat(mockService.getType()).isEqualTo(MockService.class);
    assertThat(mockService.getType()).isNotEqualTo(mockService.getClass());
  }

  @Test
  public void getTypeOfServiceClassWithLoaderReturnsTypeOfServiceClass() {

    MockLoadableService mockService = new MockLoadableService();

    assertThat(mockService.getLoader().getType()).isEqualTo(mockService.getClass());
  }

  @Service
  static final class MockLoadableService {

    @NotNull Loader getLoader() {
      return Loader.INSTANCE;
    }

    interface Loader extends ServiceLoaderSupport<MockLoadableService> {

      Loader INSTANCE = new Loader() { };

      @Override
      default Class<MockLoadableService> getType() {
        return MockLoadableService.class;
      }
    }
  }

  @Service
  static final class UnavailableService implements ServiceLoaderSupport<UnavailableService> { }

}
