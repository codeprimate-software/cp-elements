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
package org.cp.elements.lang.factory;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;

import org.cp.elements.test.TestUtils;
import org.junit.Before;
import org.junit.Test;

import edu.umd.cs.mtc.MultithreadedTestCase;
import edu.umd.cs.mtc.TestFramework;

/**
 * Unit Tests for {@link ObjectFactoryReferenceHolderTests}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.factory.ObjectFactoryReferenceHolder
 * @see org.cp.elements.test.TestUtils
 * @see edu.umd.cs.mtc.MultithreadedTestCase
 * @see edu.umd.cs.mtc.TestFramework
 * @since 1.0.0
 */
public class ObjectFactoryReferenceHolderTests {

  @Before
  public void setup() {

    ObjectFactoryReferenceHolder.clear();

    assertThat(ObjectFactoryReferenceHolder.hasReference()).isFalse();
  }

  @Test
  public void setGetClearAndHasReference() {

    assertThat(ObjectFactoryReferenceHolder.hasReference()).isFalse();

    ObjectFactory mockObjectFactory = mock(ObjectFactory.class);
    ObjectFactoryReferenceHolder.set(mockObjectFactory);

    assertThat(ObjectFactoryReferenceHolder.hasReference()).isTrue();
    assertThat(ObjectFactoryReferenceHolder.get()).isSameAs(mockObjectFactory);

    ObjectFactoryReferenceHolder.clear();

    assertThat(ObjectFactoryReferenceHolder.hasReference()).isFalse();
  }

  @Test(expected = IllegalStateException.class)
  public void getWhenReferenceUnset() {

    assertThat(ObjectFactoryReferenceHolder.hasReference()).isFalse();

    TestUtils.doIllegalStateExceptionThrowingOperation(ObjectFactoryReferenceHolder::get,
      () -> "ObjectFactory was not properly initialized!");
  }

  @Test(expected = IllegalStateException.class)
  public void setWhenReferenceSet() {

    assertThat(ObjectFactoryReferenceHolder.hasReference()).isFalse();

    ObjectFactory mockObjectFactory = mock(ObjectFactory.class, "Expected ObjectFactory");
    ObjectFactoryReferenceHolder.set(mockObjectFactory);

    assertThat(ObjectFactoryReferenceHolder.hasReference()).isTrue();
    assertThat(ObjectFactoryReferenceHolder.get()).isSameAs(mockObjectFactory);

    try {
      ObjectFactoryReferenceHolder.set(mock(ObjectFactory.class, "Illegal ObjectFactory"));
    }
    catch (IllegalStateException expected) {
      assertThat(expected.getMessage()).isEqualTo(
        String.format("The ObjectFactory reference is already set to (%1$s)", mockObjectFactory));
      assertThat(ObjectFactoryReferenceHolder.hasReference()).isTrue();
      assertThat(ObjectFactoryReferenceHolder.get()).isSameAs(mockObjectFactory);
      throw expected;
    }
  }

  @Test
  public void compareAndSet() {

    assertThat(ObjectFactoryReferenceHolder.hasReference()).isFalse();

    ObjectFactory mockObjectFactory1 = mock(ObjectFactory.class, "ObjectFactory 1");
    ObjectFactoryReferenceHolder.compareAndSet(null, mockObjectFactory1);

    assertThat(ObjectFactoryReferenceHolder.hasReference()).isTrue();
    assertThat(ObjectFactoryReferenceHolder.get()).isSameAs(mockObjectFactory1);

    ObjectFactory mockObjectFactory2 = mock(ObjectFactory.class, "ObjectFactory 2");
    ObjectFactoryReferenceHolder.compareAndSet(null, mockObjectFactory2);

    assertThat(ObjectFactoryReferenceHolder.hasReference()).isTrue();
    assertThat(ObjectFactoryReferenceHolder.get()).isNotSameAs(mockObjectFactory2);
    assertThat(ObjectFactoryReferenceHolder.get()).isSameAs(mockObjectFactory1);

    ObjectFactoryReferenceHolder.compareAndSet(mockObjectFactory1, mockObjectFactory2);

    assertThat(ObjectFactoryReferenceHolder.hasReference()).isTrue();
    assertThat(ObjectFactoryReferenceHolder.get()).isSameAs(mockObjectFactory2);

    ObjectFactory mockObjectFactory3 = mock(ObjectFactory.class, "ObjectFactory 3");
    ObjectFactoryReferenceHolder.compareAndSet(mockObjectFactory1, mockObjectFactory3);

    assertThat(ObjectFactoryReferenceHolder.hasReference()).isTrue();
    assertThat(ObjectFactoryReferenceHolder.get()).isNotSameAs(mockObjectFactory3);
    assertThat(ObjectFactoryReferenceHolder.get()).isSameAs(mockObjectFactory2);
  }

  @Test
  public void testObjectFactoryReferenceHolderThreadSafety() throws Throwable {
    TestFramework.runOnce(new ObjectFactoryReferenceHolderThreadSafteyMultithreadedTestCase());
  }

  @SuppressWarnings("unused")
  private static class ObjectFactoryReferenceHolderThreadSafteyMultithreadedTestCase extends MultithreadedTestCase {

    private ObjectFactory mockGetterObjectFactory;
    private ObjectFactory mockSetterObjectFactory;

    @Override
    public void initialize() {

      super.initialize();

      mockGetterObjectFactory = mock(ObjectFactory.class, "Getter ObjectFactory");
      mockSetterObjectFactory = mock(ObjectFactory.class, "Setter ObjectFactory");
    }

    public void thread1() {

      Thread.currentThread().setName("Setter Thread!");

      assertTick(0);
      assertFalse(ObjectFactoryReferenceHolder.hasReference());

      ObjectFactoryReferenceHolder.set(mockGetterObjectFactory);

      assertTrue(ObjectFactoryReferenceHolder.hasReference());
      assertSame(mockGetterObjectFactory, ObjectFactoryReferenceHolder.get());

      waitForTick(2);

      assertTrue(ObjectFactoryReferenceHolder.hasReference());
      assertSame(mockSetterObjectFactory, ObjectFactoryReferenceHolder.get());
    }

    public void thread2() {

      waitForTick(1);

      Thread.currentThread().setName("Getter Thread!");

      assertTick(1);
      assertTrue(ObjectFactoryReferenceHolder.hasReference());
      assertSame(mockGetterObjectFactory, ObjectFactoryReferenceHolder.get());

      ObjectFactoryReferenceHolder.compareAndSet(mockGetterObjectFactory, mockSetterObjectFactory);

      assertTrue(ObjectFactoryReferenceHolder.hasReference());
      assertSame(mockSetterObjectFactory, ObjectFactoryReferenceHolder.get());
    }

    @Override
    public void finish() {

      super.finish();

      ObjectFactoryReferenceHolder.clear();

      assertFalse(ObjectFactoryReferenceHolder.hasReference());
    }
  }
}
