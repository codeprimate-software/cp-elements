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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import org.cp.elements.test.AbstractMockingTestSuite;
import org.junit.Before;
import org.junit.Test;

import edu.umd.cs.mtc.MultithreadedTestCase;
import edu.umd.cs.mtc.TestFramework;

/**
 * The ObjectFactoryReferenceHolderTest class is a test suite of test cases testing the contract and functionality
 * of the ObjectFactoryReferenceHolderTest class.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.factory.ObjectFactoryReferenceHolder
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.junit.Test
 * @see edu.umd.cs.mtc.MultithreadedTestCase
 * @see edu.umd.cs.mtc.TestFramework
 * @since 1.0.0
 */
public class ObjectFactoryReferenceHolderTest extends AbstractMockingTestSuite {

  @Before
  public void setup() {
    ObjectFactoryReferenceHolder.clear();
    assertFalse(ObjectFactoryReferenceHolder.hasReference());
  }

  @Test
  public void testSetGetClearAndHasReference() {
    assertFalse(ObjectFactoryReferenceHolder.hasReference());

    ObjectFactory mockObjectFactory = mockContext.mock(ObjectFactory.class);
    ObjectFactoryReferenceHolder.set(mockObjectFactory);

    assertTrue(ObjectFactoryReferenceHolder.hasReference());
    assertSame(mockObjectFactory, ObjectFactoryReferenceHolder.get());

    ObjectFactoryReferenceHolder.clear();

    assertFalse(ObjectFactoryReferenceHolder.hasReference());
  }

  @Test(expected = IllegalStateException.class)
  public void testGetWhenReferenceUnset() {
    try {
      assertFalse(ObjectFactoryReferenceHolder.hasReference());
      ObjectFactoryReferenceHolder.get();
    }
    catch (IllegalStateException expected) {
      assertEquals("The ObjectFactory reference was not properly initialized!", expected.getMessage());
      throw expected;
    }
  }

  @Test(expected = IllegalStateException.class)
  public void testSetWhenReferenceSet() {
    assertFalse(ObjectFactoryReferenceHolder.hasReference());

    ObjectFactory mockObjectFactory = mockContext.mock(ObjectFactory.class, "Expected ObjectFactory");
    ObjectFactoryReferenceHolder.set(mockObjectFactory);

    assertTrue(ObjectFactoryReferenceHolder.hasReference());
    assertSame(mockObjectFactory, ObjectFactoryReferenceHolder.get());

    try {
      ObjectFactoryReferenceHolder.set(mockContext.mock(ObjectFactory.class, "Illegal ObjectFactory"));
    }
    catch (IllegalStateException expected) {
      assertEquals(String.format("The ObjectFactory reference is already set to (%1$s)!", mockObjectFactory),
        expected.getMessage());
      assertTrue(ObjectFactoryReferenceHolder.hasReference());
      assertSame(mockObjectFactory, ObjectFactoryReferenceHolder.get());
      throw expected;
    }
  }

  @Test
  public void testCompareAndSet() {
    assertFalse(ObjectFactoryReferenceHolder.hasReference());

    ObjectFactory mockObjectFactory1 = mockContext.mock(ObjectFactory.class, "ObjectFactory 1");
    ObjectFactoryReferenceHolder.compareAndSet(null, mockObjectFactory1);

    assertTrue(ObjectFactoryReferenceHolder.hasReference());
    assertSame(mockObjectFactory1, ObjectFactoryReferenceHolder.get());

    ObjectFactory mockObjectFactory2 = mockContext.mock(ObjectFactory.class, "ObjectFactory 2");
    ObjectFactoryReferenceHolder.compareAndSet(null, mockObjectFactory2);

    assertTrue(ObjectFactoryReferenceHolder.hasReference());
    assertNotSame(mockObjectFactory2, ObjectFactoryReferenceHolder.get());
    assertSame(mockObjectFactory1, ObjectFactoryReferenceHolder.get());

    ObjectFactoryReferenceHolder.compareAndSet(mockObjectFactory1, mockObjectFactory2);

    assertTrue(ObjectFactoryReferenceHolder.hasReference());
    assertSame(mockObjectFactory2, ObjectFactoryReferenceHolder.get());

    ObjectFactory mockObjectFactory3 = mockContext.mock(ObjectFactory.class, "ObjectFactory 3");
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
      mockGetterObjectFactory = mockContext.mock(ObjectFactory.class, "Getter ObjectFactory");
      mockSetterObjectFactory = mockContext.mock(ObjectFactory.class, "Setter ObjectFactory");
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
