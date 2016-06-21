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

package org.cp.elements.lang.factory;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import edu.umd.cs.mtc.MultithreadedTestCase;
import edu.umd.cs.mtc.TestFramework;

/**
 * Test suite of test cases testing the contract and functionality of the {@link ObjectFactoryReferenceHolderTests} class.
 *
 * @author John J. Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.rules.ExpectedException
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.factory.ObjectFactoryReferenceHolder
 * @see edu.umd.cs.mtc.MultithreadedTestCase
 * @see edu.umd.cs.mtc.TestFramework
 * @since 1.0.0
 */
public class ObjectFactoryReferenceHolderTests {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @Before
  public void setup() {
    ObjectFactoryReferenceHolder.clear();
    assertFalse(ObjectFactoryReferenceHolder.hasReference());
  }

  @Test
  public void setGetClearAndHasReference() {
    assertFalse(ObjectFactoryReferenceHolder.hasReference());

    ObjectFactory mockObjectFactory = mock(ObjectFactory.class);
    ObjectFactoryReferenceHolder.set(mockObjectFactory);

    assertTrue(ObjectFactoryReferenceHolder.hasReference());
    assertSame(mockObjectFactory, ObjectFactoryReferenceHolder.get());

    ObjectFactoryReferenceHolder.clear();

    assertFalse(ObjectFactoryReferenceHolder.hasReference());
  }

  @Test
  public void getWhenReferenceUnset() {
    exception.expect(IllegalStateException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("ObjectFactory was not properly initialized");

    assertThat(ObjectFactoryReferenceHolder.hasReference(), is(false));

    ObjectFactoryReferenceHolder.get();
  }

  @Test(expected = IllegalStateException.class)
  public void setWhenReferenceSet() {
    assertFalse(ObjectFactoryReferenceHolder.hasReference());

    ObjectFactory mockObjectFactory = mock(ObjectFactory.class, "Expected ObjectFactory");
    ObjectFactoryReferenceHolder.set(mockObjectFactory);

    assertTrue(ObjectFactoryReferenceHolder.hasReference());
    assertSame(mockObjectFactory, ObjectFactoryReferenceHolder.get());

    try {
      ObjectFactoryReferenceHolder.set(mock(ObjectFactory.class, "Illegal ObjectFactory"));
    }
    catch (IllegalStateException expected) {
      assertEquals(String.format("The ObjectFactory reference is already set to (%1$s)", mockObjectFactory),
        expected.getMessage());
      assertTrue(ObjectFactoryReferenceHolder.hasReference());
      assertSame(mockObjectFactory, ObjectFactoryReferenceHolder.get());
      throw expected;
    }
  }

  @Test
  public void compareAndSet() {
    assertFalse(ObjectFactoryReferenceHolder.hasReference());

    ObjectFactory mockObjectFactory1 = mock(ObjectFactory.class, "ObjectFactory 1");
    ObjectFactoryReferenceHolder.compareAndSet(null, mockObjectFactory1);

    assertTrue(ObjectFactoryReferenceHolder.hasReference());
    assertSame(mockObjectFactory1, ObjectFactoryReferenceHolder.get());

    ObjectFactory mockObjectFactory2 = mock(ObjectFactory.class, "ObjectFactory 2");
    ObjectFactoryReferenceHolder.compareAndSet(null, mockObjectFactory2);

    assertTrue(ObjectFactoryReferenceHolder.hasReference());
    assertNotSame(mockObjectFactory2, ObjectFactoryReferenceHolder.get());
    assertSame(mockObjectFactory1, ObjectFactoryReferenceHolder.get());

    ObjectFactoryReferenceHolder.compareAndSet(mockObjectFactory1, mockObjectFactory2);

    assertTrue(ObjectFactoryReferenceHolder.hasReference());
    assertSame(mockObjectFactory2, ObjectFactoryReferenceHolder.get());

    ObjectFactory mockObjectFactory3 = mock(ObjectFactory.class, "ObjectFactory 3");
    ObjectFactoryReferenceHolder.compareAndSet(mockObjectFactory1, mockObjectFactory3);

    assertTrue(ObjectFactoryReferenceHolder.hasReference());
    assertNotSame(mockObjectFactory3, ObjectFactoryReferenceHolder.get());
    assertSame(mockObjectFactory2, ObjectFactoryReferenceHolder.get());
  }

  @Test
  public void testObjectFactoryReferenceHolderThreadSafety() throws Throwable {
    TestFramework.runOnce(new ObjectFactoryReferenceHolderThreadSafteyMultithreadedTestCase());
  }

  @SuppressWarnings("unused")
  protected final class ObjectFactoryReferenceHolderThreadSafteyMultithreadedTestCase extends MultithreadedTestCase {

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
