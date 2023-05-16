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
import static org.mockito.Mockito.verifyNoInteractions;

import org.cp.elements.test.TestUtils;
import org.junit.Before;
import org.junit.jupiter.api.Test;

import edu.umd.cs.mtc.MultithreadedTestCase;
import edu.umd.cs.mtc.TestFramework;

/**
 * Unit Tests for {@link ObjectFactoryReferenceHolderUnitTests}.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.factory.ObjectFactoryReferenceHolder
 * @see org.cp.elements.test.TestUtils
 * @see edu.umd.cs.mtc.MultithreadedTestCase
 * @see edu.umd.cs.mtc.TestFramework
 * @since 1.0.0
 */
public class ObjectFactoryReferenceHolderUnitTests {

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

    verifyNoInteractions(mockObjectFactory);
  }

  @Test(expected = IllegalStateException.class)
  public void getWhenReferenceIsUnset() {

    assertThat(ObjectFactoryReferenceHolder.hasReference()).isFalse();

    TestUtils.doIllegalStateExceptionThrowingOperation(ObjectFactoryReferenceHolder::get,
      () -> "An ObjectFactory was not properly initialized");
  }

  @Test(expected = IllegalStateException.class)
  public void setWhenReferenceIsSet() {

    assertThat(ObjectFactoryReferenceHolder.hasReference()).isFalse();

    ObjectFactory mockObjectFactory = mock(ObjectFactory.class, "Expected ObjectFactory");

    ObjectFactoryReferenceHolder.set(mockObjectFactory);

    assertThat(ObjectFactoryReferenceHolder.hasReference()).isTrue();
    assertThat(ObjectFactoryReferenceHolder.get()).isSameAs(mockObjectFactory);

    try {
      ObjectFactoryReferenceHolder.set(mock(ObjectFactory.class, "Illegal ObjectFactory"));
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessage("An ObjectFactory reference is already set to [%s]", mockObjectFactory);
      assertThat(ObjectFactoryReferenceHolder.hasReference()).isTrue();
      assertThat(ObjectFactoryReferenceHolder.get()).isSameAs(mockObjectFactory);

      throw expected;
    }
    finally {
      verifyNoInteractions(mockObjectFactory);
    }
  }

  @Test
  public void compareAndSet() {

    assertThat(ObjectFactoryReferenceHolder.hasReference()).isFalse();

    ObjectFactory mockObjectFactoryOne = mock(ObjectFactory.class, "ObjectFactory 1");

    ObjectFactoryReferenceHolder.compareAndSet(null, mockObjectFactoryOne);

    assertThat(ObjectFactoryReferenceHolder.hasReference()).isTrue();
    assertThat(ObjectFactoryReferenceHolder.get()).isSameAs(mockObjectFactoryOne);

    ObjectFactory mockObjectFactoryTwo = mock(ObjectFactory.class, "ObjectFactory 2");

    ObjectFactoryReferenceHolder.compareAndSet(null, mockObjectFactoryTwo);

    assertThat(ObjectFactoryReferenceHolder.hasReference()).isTrue();
    assertThat(ObjectFactoryReferenceHolder.get()).isNotSameAs(mockObjectFactoryTwo);
    assertThat(ObjectFactoryReferenceHolder.get()).isSameAs(mockObjectFactoryOne);

    ObjectFactoryReferenceHolder.compareAndSet(mockObjectFactoryOne, mockObjectFactoryTwo);

    assertThat(ObjectFactoryReferenceHolder.hasReference()).isTrue();
    assertThat(ObjectFactoryReferenceHolder.get()).isSameAs(mockObjectFactoryTwo);

    ObjectFactory mockObjectFactoryThree = mock(ObjectFactory.class, "ObjectFactory 3");

    ObjectFactoryReferenceHolder.compareAndSet(mockObjectFactoryOne, mockObjectFactoryThree);

    assertThat(ObjectFactoryReferenceHolder.hasReference()).isTrue();
    assertThat(ObjectFactoryReferenceHolder.get()).isNotSameAs(mockObjectFactoryThree);
    assertThat(ObjectFactoryReferenceHolder.get()).isSameAs(mockObjectFactoryTwo);
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
