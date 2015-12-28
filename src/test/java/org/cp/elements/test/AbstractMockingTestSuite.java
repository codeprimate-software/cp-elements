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

package org.cp.elements.test;

import org.jmock.Expectations;
import org.jmock.Mockery;
import org.jmock.lib.concurrent.Synchroniser;
import org.jmock.lib.legacy.ClassImposteriser;
import org.junit.After;
import org.junit.Before;

/**
 * The AbstractMockingTestSuite class is an abstract base class encapsulating mocking functionality common to all
 * unit tests.
 *
 * @author John J. Blum
 * @see AbstractBaseTestSuite
 * @see org.jmock.Mockery
 * @see org.jmock.lib.concurrent.Synchroniser
 * @see org.jmock.lib.legacy.ClassImposteriser
 * @since 1.0.0
 * @deprecated replacing jMock with Mockito.
 */
@SuppressWarnings("unused")
@Deprecated
public abstract class AbstractMockingTestSuite extends AbstractBaseTestSuite {

  protected Mockery mockContext;

  protected void checking(final Expectations expectations) {
    mockContext.checking(expectations);
  }

  protected <T> T mock(final Class<T> type, final String name) {
    return mockContext.mock(type, name);
  }

  @Before
  public void preTestCaseSetup() {
    mockContext = new Mockery();
    mockContext.setImposteriser(ClassImposteriser.INSTANCE);
    mockContext.setThreadingPolicy(new Synchroniser());
  }

  @After
  public void postTestCaseTearDown() {
    mockContext.assertIsSatisfied();
    mockContext = null;
  }

}
