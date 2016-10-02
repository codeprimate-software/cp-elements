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

package org.cp.elements.test.mock;

import java.util.Arrays;

import org.cp.elements.util.ArrayUtils;
import org.hamcrest.BaseMatcher;
import org.hamcrest.Description;
import org.hamcrest.Matcher;
import org.mockito.internal.matchers.VarargMatcher;

/**
 * {@link MockitoMatchers} is a utility class encapsulating custom Hamcrest {@link Matcher Matchers} used by Mockito
 * in the {@literal cp-elements} project test suite.
 *
 * @author John Blum
 * @see org.hamcrest.Matcher
 * @see org.mockito.internal.matchers.VarargMatcher
 * @since 1.9.0
 */
@SuppressWarnings("unused")
public abstract class MockitoMatchers {

	/* (non-Javadoc) */
	public static Matcher<String> stringArrayMatcher(String... expected) {
		return new ArrayMatcher<>(expected);
	}

	/* (non-Javadoc) */
	protected static class ArrayMatcher<T> extends BaseMatcher<T> implements VarargMatcher {

		private final T[] expected;

    @SafeVarargs
		protected ArrayMatcher(T... expected) {
			this.expected = expected;
		}

		@Override
		public boolean matches(Object item) {
			Object[] actual = (item instanceof Object[] ? (Object[]) item : ArrayUtils.asArray(item));

			return Arrays.equals(actual, expected);
		}

		@Override
		public void describeTo(Description description) {
			description.appendText(String.format("expected [%s]", this.expected));
		}
	}
}
