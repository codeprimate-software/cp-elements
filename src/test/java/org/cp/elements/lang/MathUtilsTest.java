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

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.data.Offset.offset;

import java.math.BigInteger;
import java.util.stream.IntStream;

import org.junit.Test;

import org.cp.elements.test.TestUtils;

/**
 * Unit Tests for {@link MathUtils}.
 *
 * @author John J. Blum
 * @see java.math.BigInteger
 * @see org.junit.Test
 * @see org.cp.elements.lang.MathUtils
 * @since 1.0.0
 */
public class MathUtilsTest {

  private void assertEqualIntArrays(int[] expected, int[] actual) {

    assertThat(actual.length).isEqualTo(expected.length);

    for (int index = 0; index < expected.length; index++) {
      assertThat(actual[index]).isEqualTo(expected[index]);
    }
  }

  private int[] toIntArray(int... numbers) {
    return numbers;
  }

  @Test
  public void testCircleArea() {
    assertThat(MathUtils.circleArea(5.0d)).isCloseTo(78.5398d, offset(0.0001d));
  }

  @Test
  public void testCircleCircumference() {
    assertThat(MathUtils.circleCircumference(5.0d)).isCloseTo(31.41592d, offset(0.000007d));
  }

  @Test
  public void testCircleDiameter() {
    assertThat(MathUtils.circleDiameter(5.0d)).isCloseTo(10.0d, offset(0.0d));
  }

  @Test
  public void testConeVolume() {
    assertThat(MathUtils.coneVolume(5.0d, 4.0d)).isCloseTo(104.7d, offset(0.02d));
  }

  @Test
  public void testCubeSurfaceArea() {
    assertThat(MathUtils.cubeSurfaceArea(5.0d)).isCloseTo(150.0d, offset(0.0d));
  }

  @Test
  public void testCubeVolume() {
    assertThat(MathUtils.cubeVolume(5.0d)).isCloseTo(125.0d, offset(0.0d));
  }

  @Test
  public void testCylinderSurfaceArea() {
    assertThat(MathUtils.cylinderSurfaceArea(5.0d, 10.d)).isCloseTo(471.2388d, offset(0.000099d));
  }

  @Test
  public void testCylinderVolume() {
    assertThat(MathUtils.cylinderVolume(5.0d, 4.0d)).isCloseTo(314.1592d, offset(0.00007d));
  }

  @Test
  public void testEllipseArea() {
    assertThat(MathUtils.ellipseArea(5.0d, 2.0d)).isCloseTo(31.41592d, offset(0.000007d));
  }

  @Test
  public void testEllipsoidVolume() {
    assertThat(MathUtils.ellipsoidVolume(5.0d, 4.0d, 3.0d)).isCloseTo(251.32d, offset(0.008d));
  }

  @Test
  public void testEquilateralTriangleArea() {
    assertThat(MathUtils.equilateralTriangleArea(5.0d)).isCloseTo(10.825d, offset(0.0004d));
  }

  @Test
  public void factorial() {

    assertThat(MathUtils.factorial(BigInteger.ZERO)).isEqualTo(BigInteger.ONE);
    assertThat(MathUtils.factorial(BigInteger.ONE)).isEqualTo(BigInteger.ONE);
    assertThat(MathUtils.factorial(MathUtils.TWO)).isEqualTo(MathUtils.TWO);
    assertThat(MathUtils.factorial(BigInteger.valueOf(3)).longValue()).isEqualTo(6L);
    assertThat(MathUtils.factorial(BigInteger.valueOf(4)).longValue()).isEqualTo(24L);
    assertThat(MathUtils.factorial(BigInteger.valueOf(5)).longValue()).isEqualTo(120L);
    assertThat(MathUtils.factorial(BigInteger.valueOf(6)).longValue()).isEqualTo(720L);
    assertThat(MathUtils.factorial(BigInteger.valueOf(7)).longValue()).isEqualTo(5040L);
    assertThat(MathUtils.factorial(BigInteger.valueOf(8)).longValue()).isEqualTo(40320L);
    assertThat(MathUtils.factorial(BigInteger.valueOf(9)).longValue()).isEqualTo(362880L);
    assertThat(MathUtils.factorial(BigInteger.valueOf(10)).longValue()).isEqualTo(3628800L);
    assertThat(MathUtils.factorial(BigInteger.valueOf(11)).longValue()).isEqualTo(39916800L);
    assertThat(MathUtils.factorial(BigInteger.valueOf(12)).longValue()).isEqualTo(479001600L);
  }

  @Test
  public void factorialHandlesNumericOverflow() {

    assertThat(MathUtils.factorial(BigInteger.valueOf(13)).longValue()).isEqualTo(6227020800L);
    assertThat(MathUtils.factorial(BigInteger.valueOf(20)).longValue()).isEqualTo(2432902008176640000L);
  }

  @Test
  public void factorialHandlesStackOverflow() {
    MathUtils.factorial(BigInteger.valueOf(20000));
  }

  @Test(expected = IllegalArgumentException.class)
  public void factorialOfNegativeOne() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(() -> MathUtils.factorial(MathUtils.NEGATIVE_ONE),
      () -> String.format(MathUtils.NUMBER_LESS_THAN_ZERO_ERROR_MESSAGE, MathUtils.NEGATIVE_ONE));
  }

  @Test(expected = IllegalArgumentException.class)
  public void factorialOfNullValue() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(() -> MathUtils.factorial(null),
      () -> "value must not be null");
  }

  @Test
  public void testFibonacciNumber() {

    assertThat(MathUtils.fibonacciNumber(1)).isEqualTo(0);
    assertThat(MathUtils.fibonacciNumber(2)).isEqualTo(1);
    assertThat(MathUtils.fibonacciNumber(3)).isEqualTo(1);
    assertThat(MathUtils.fibonacciNumber(4)).isEqualTo(2);
    assertThat(MathUtils.fibonacciNumber(5)).isEqualTo(3);
    assertThat(MathUtils.fibonacciNumber(6)).isEqualTo(5);
    assertThat(MathUtils.fibonacciNumber(7)).isEqualTo(8);
    assertThat(MathUtils.fibonacciNumber(8)).isEqualTo(13);
    assertThat(MathUtils.fibonacciNumber(9)).isEqualTo(21);
    assertThat(MathUtils.fibonacciNumber(30)).isEqualTo(514229);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testFibonacciNumberWithIllegalArgument() {
    MathUtils.fibonacciSequence(-1);
  }

  @Test
  public void testFibonacciSequence() {

    assertEqualIntArrays(toIntArray(0), MathUtils.fibonacciSequence(1));
    assertEqualIntArrays(toIntArray(0, 1), MathUtils.fibonacciSequence(2));
    assertEqualIntArrays(toIntArray(0, 1, 1), MathUtils.fibonacciSequence(3));
    assertEqualIntArrays(toIntArray(0, 1, 1, 2, 3), MathUtils.fibonacciSequence(5));
    assertEqualIntArrays(toIntArray(0, 1, 1, 2, 3, 5, 8, 13), MathUtils.fibonacciSequence(8));
    assertEqualIntArrays(toIntArray(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144), MathUtils.fibonacciSequence(13));
    assertEqualIntArrays(toIntArray(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181,
        6765), MathUtils.fibonacciSequence(21));
    assertEqualIntArrays(toIntArray(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181,
      6765, 10946, 17711, 28657, 46368, 75025, 121393, 196418, 317811), MathUtils.fibonacciSequence(29));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testFibonacciSequenceWithAnIllegalArgument() {
    MathUtils.fibonacciSequence(0);
  }

  @Test
  public void testIrregularPrismVolume() {
    assertThat(MathUtils.irregularPrismVolume(5.0d, 4.0d)).isCloseTo(20.0d, offset(0.0d));
  }

  @Test
  public void testMax() {

    assertThat(MathUtils.max((double[]) null)).isCloseTo(Double.NaN, offset(0.0d));
    assertThat(MathUtils.max(0)).isCloseTo(0.0d, offset(0.0d));
    assertThat(MathUtils.max(0, 0, 0)).isCloseTo(0.0d, offset(0.0d));
    assertThat(MathUtils.max(0, 1, 2)).isCloseTo(2.0d, offset(0.0d));
    assertThat(MathUtils.max(2, 1, 0)).isCloseTo(2.0d, offset(0.0d));
    assertThat(MathUtils.max(1, 2, 0)).isCloseTo(2.0d, offset(0.0d));
    assertThat(MathUtils.max(-100, -1, 1)).isCloseTo(1.0d, offset(0.0d));
    assertThat(MathUtils.max(Double.NaN, -1, 1)).isCloseTo(1.0d, offset(0.0d));
  }

  @Test
  public void testMin() {

    assertThat(MathUtils.min((double[]) null)).isCloseTo(Double.NaN, offset(0.0d));
    assertThat(MathUtils.min(0)).isCloseTo(0.0d, offset(0.0d));
    assertThat(MathUtils.min(0, 0, 0)).isCloseTo(0.0d, offset(0.0d));
    assertThat(MathUtils.min(0, 1, 2)).isCloseTo(0.0d, offset(0.0d));
    assertThat(MathUtils.min(2, 1, 0)).isCloseTo(0.0d, offset(0.0d));
    assertThat(MathUtils.min(1, 2, 0)).isCloseTo(0.0d, offset(0.0d));
    assertThat(MathUtils.min(-100, -1, 1)).isCloseTo(-100.0d, offset(0.0d));
    assertThat(MathUtils.min(Double.NaN, -1, 1)).isCloseTo(-1.0d, offset(0.0d));
  }

  @Test
  public void testMultiply() {

    assertThat(MathUtils.multiply((int[]) null)).isEqualTo(0);
    assertThat(MathUtils.multiply()).isEqualTo(0);
    assertThat(MathUtils.multiply(0)).isEqualTo(0);
    assertThat(MathUtils.multiply(0, 0, 0)).isEqualTo(0);
    assertThat(MathUtils.multiply(0, 0, 1)).isEqualTo(0);
    assertThat(MathUtils.multiply(1)).isEqualTo(1);
    assertThat(MathUtils.multiply(1, 1, 1)).isEqualTo(1);
    assertThat(MathUtils.multiply(1, 2, 3)).isEqualTo(6);
    assertThat(MathUtils.multiply(2, 4, 8)).isEqualTo(64);
  }

  @Test
  public void testParallelogramArea() {
    assertThat(MathUtils.parallelogramArea(5.0d, 6.0d)).isCloseTo(30.0d, offset(0.0d));
  }

  @Test
  public void testPyramidVolume() {
    assertThat(MathUtils.pyramidVolume(5.0d, 6.0d)).isCloseTo(10.d, offset(0.0d));
  }

  @Test
  public void testPythagoreanTheorem() {
    assertThat(MathUtils.pythagoreanTheorem(3.0d, 4.0d)).isCloseTo(5.0d, offset(0.0d));
  }

  @Test
  public void testRectangleArea() {
    assertThat(MathUtils.rectangleArea(5.0d, 8.0d)).isCloseTo(40.0d, offset(0.0d));
  }

  @Test
  public void testRectangularPrismSurfaceArea() {
    assertThat(MathUtils.rectangularPrismSurfaceArea(5.0d, 4.0d, 3.0d)).isCloseTo(94.0d, offset(0.0d));
  }

  @Test
  public void testRectanglePrismVolume() {
    assertThat(MathUtils.rectangularPrismVolume(5.0d, 6.0d, 8.0d)).isCloseTo(240.0d, offset(0.0d));
  }

  @Test
  public void testRoundToNearestTenth() {

    assertThat(Double.valueOf(MathUtils.roundToNearestTenth(0.0d))).isEqualTo(Double.valueOf(0.0d));
    assertThat(Double.valueOf(MathUtils.roundToNearestTenth(7.0d))).isEqualTo(Double.valueOf(7.0d));
    assertThat(Double.valueOf(MathUtils.roundToNearestTenth(13.14d))).isEqualTo(Double.valueOf(13.1d));
    assertThat(Double.valueOf(MathUtils.roundToNearestTenth(13.19d))).isEqualTo(Double.valueOf(13.2d));
    assertThat(Double.valueOf(MathUtils.roundToNearestTenth(19.419d))).isEqualTo(Double.valueOf(19.4d));
    assertThat(Double.valueOf(MathUtils.roundToNearestTenth(19.449d))).isEqualTo(Double.valueOf(19.4d));
    assertThat(Double.valueOf(MathUtils.roundToNearestTenth(19.55d))).isEqualTo(Double.valueOf(19.6d));
  }

  @Test
  public void roundToPrecision() {

    assertThat(MathUtils.roundToPrecision(123.123456789d, 1)).isEqualTo(123.1d);
    assertThat(MathUtils.roundToPrecision(123.123456789d, 2)).isEqualTo(123.12d);
    assertThat(MathUtils.roundToPrecision(123.123456789d, 3)).isEqualTo(123.123d);
    assertThat(MathUtils.roundToPrecision(123.123456789d, 4)).isEqualTo(123.1235d);
    assertThat(MathUtils.roundToPrecision(123.123456789d, 5)).isEqualTo(123.12346d);
    assertThat(MathUtils.roundToPrecision(123.123456789d, 6)).isEqualTo(123.123457d);
    assertThat(MathUtils.roundToPrecision(123.123456789d, 7)).isEqualTo(123.1234568d);
    assertThat(MathUtils.roundToPrecision(123.123456789d, 8)).isEqualTo(123.12345679d);
    assertThat(MathUtils.roundToPrecision(123.123456789d, 9)).isEqualTo(123.123456789d);
  }

  @Test
  public void roundToPrecisionWithInvalidNumberOfDecimalPlaces() {

    IntStream.of(0, -1, -2).forEach(numberOfDecimalPlaces ->
      assertThatIllegalArgumentException()
        .isThrownBy(() -> MathUtils.roundToPrecision(1.23d, numberOfDecimalPlaces))
        .withMessage("Number of decimal places [%d] must be greater than 0", numberOfDecimalPlaces)
        .withNoCause());
  }

  @Test
  public void testSphereSurfaceArea() {
    assertThat(MathUtils.sphereSurfaceArea(5.0d)).isCloseTo(314.1592d, offset(0.00007d));
  }

  @Test
  public void testSphereVolume() {
    assertThat(MathUtils.sphereVolume(3.0d)).isCloseTo(113.09d, offset(0.008d));
  }

  @Test
  public void testSquareArea() {
    assertThat(MathUtils.squareArea(5.0d)).isCloseTo(25.0d, offset(0.0d));
  }

  @Test
  public void testSum() {

    assertThat(MathUtils.sum((int[]) null)).isEqualTo(0);
    assertThat(MathUtils.sum()).isEqualTo(0);
    assertThat(MathUtils.sum(0)).isEqualTo(0);
    assertThat(MathUtils.sum(0, 0, 0)).isEqualTo(0);
    assertThat(MathUtils.sum(1, 2, 3)).isEqualTo(6);
    assertThat(MathUtils.sum(0, 1, 2, 3, 4, 5, 6)).isEqualTo(21);
  }

  @Test
  public void testTrapezoidArea() {
    assertThat(MathUtils.trapezoidArea(10.0d, 2.0d, 3.0d)).isCloseTo(25.0d, offset(0.0d));
  }

  @Test
  public void testTriangleArea() {
    assertThat(MathUtils.triangleArea(5.0d, 6.0d)).isCloseTo(15.0d, offset(0.0d));
  }

  @Test
  public void truncateToTheNumberOfDecimalPlaces() {

    assertThat(MathUtils.truncateToPrecision(0.123456789d, 1)).isEqualTo(0.1d);
    assertThat(MathUtils.truncateToPrecision(0.123456789d, 2)).isEqualTo(0.12d);
    assertThat(MathUtils.truncateToPrecision(0.123456789d, 3)).isEqualTo(0.123d);
    assertThat(MathUtils.truncateToPrecision(0.123456789d, 4)).isEqualTo(0.1234d);
    assertThat(MathUtils.truncateToPrecision(0.123456789d, 5)).isEqualTo(0.12345d);
    assertThat(MathUtils.truncateToPrecision(0.123456789d, 6)).isEqualTo(0.123456d);
    assertThat(MathUtils.truncateToPrecision(0.123456789d, 7)).isEqualTo(0.1234567d);
    assertThat(MathUtils.truncateToPrecision(0.123456789d, 8)).isEqualTo(0.12345678d);
    assertThat(MathUtils.truncateToPrecision(0.123456789d, 9)).isEqualTo(0.123456789d);
  }

  @Test
  public void truncateWithInvalidNumberOfDecimalPlaces() {

    IntStream.of(0, -1, -2).forEach(numberOfDecimalPlaces ->
      assertThatIllegalArgumentException()
        .isThrownBy(() -> MathUtils.truncateToPrecision(0.123d, numberOfDecimalPlaces))
        .withMessage("Number of decimal places [%d] must be greater than 0", numberOfDecimalPlaces)
        .withNoCause());
  }
}
