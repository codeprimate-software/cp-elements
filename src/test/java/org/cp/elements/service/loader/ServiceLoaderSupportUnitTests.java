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
import org.cp.elements.service.loader.provider.AppleMapsGeocodingService;
import org.cp.elements.service.loader.provider.BingGeocodingService;
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

    MockService service = spy(MockService.getInstance());

    ServiceLoaderSupport<MockService> serviceLoaderSupport = mock(ServiceLoaderSupport.class);

    doCallRealMethod().when(serviceLoaderSupport).getServiceInstance();
    doReturn(service).when(serviceLoaderSupport).getServiceInstance(any(Predicate.class));

    assertThat(serviceLoaderSupport.getServiceInstance()).isEqualTo(service);

    verify(serviceLoaderSupport, times(1)).getServiceInstance();
    verify(serviceLoaderSupport, times(1)).getServiceInstance(isA(Predicate.class));
    verifyNoMoreInteractions(serviceLoaderSupport);
    verifyNoInteractions(service);
  }

  @Test
  public void getServiceInstanceWithMultipleAvailableServiceInstances() {

    MockGeocodingService geocodingService = MockGeocodingService.getLoader().getServiceInstance();

    assertThat(geocodingService).isInstanceOf(GoogleGeocodingService.class);

    Point coordinates = geocodingService.geocode("200 This Is The Way San Francisco, CA 55055");

    assertThat(coordinates).isNotNull();
    assertThat(coordinates).isEqualTo(new Point(4, 16));

    String address = geocodingService.reverseGeocode(coordinates);

    assertThat(address).isEqualTo("100 Main St. Portland, OR 97205");
  }

  @Test
  public void getServiceInstanceWithNameReturnsTargetedServiceInstance() {

    MockGeocodingService geocodingService =
      MockGeocodingService.getLoader().getServiceInstance("Apple Maps");

    assertThat(geocodingService).isInstanceOf(AppleMapsGeocodingService.class);

    Point coordinates = geocodingService.geocode("5050 Fortune Ave. Portland, OR 97205");

    assertThat(coordinates).isNotNull();
    assertThat(coordinates).isEqualTo(new Point(256, 1024));

    String address = geocodingService.reverseGeocode(coordinates);

    assertThat(address).isEqualTo("2 Cents Ave. San Francisco, CA 90210");
  }

  @Test
  public void getServiceInstanceWithNameAndQualifierReturnsTargetedServiceInstance() {

    MockGeocodingService geocodingService =
      MockGeocodingService.getLoader().getServiceInstance("Bing");

    assertThat(geocodingService).isInstanceOf(BingGeocodingService.class);

    assertThat(geocodingService)
      .isEqualTo(MockGeocodingService.getLoader().getServiceInstance("BingBangBoom"));

    Point coordinates = geocodingService.geocode("5276 NE Oregon St. Portland, OR 97213");

    assertThat(coordinates).isNotNull();
    assertThat(coordinates).isEqualTo(new Point(64, 256));

    String address = geocodingService.reverseGeocode(coordinates);

    assertThat(address).isEqualTo("4 Highway 101 Los Angeles, CA 96069");
  }

  @Test
  public void getServiceInstanceWithQualifierReturnsTargetedServiceInstance() {

    MockGeocodingService geocodingService =
      MockGeocodingService.getLoader().getServiceInstance("TomTom");

    assertThat(geocodingService).isInstanceOf(TomTomGeocodingService.class);

    Point coordinates = geocodingService.geocode("404 Walk This Way, San Diego, CA 69096");

    assertThat(coordinates).isNotNull();
    assertThat(coordinates).isEqualTo(new Point(16, 64));

    String address = geocodingService.reverseGeocode(coordinates);

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
      .havingMessageContaining("Failed to find a service instance with the declared name [testQualifier]")
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
