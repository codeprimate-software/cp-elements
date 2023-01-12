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

import java.math.BigInteger;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;

/**
 * {@link MathUtils} is an abstract utility class encapsulating common mathematical operations and calculations.
 *
 * @author John J. Blum
 * @see java.lang.Math
 * @see java.math.BigInteger
 * @see <a href="http://www.math.com">math.com</a>
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class MathUtils {

  public static final double BASE_TEN = 10.0d;
  public static final double BASE_TWO = 2.0d;

  public static final BigInteger NEGATIVE_ONE = BigInteger.ONE.negate();
  public static final BigInteger TWO = BigInteger.valueOf(2);

  protected static final String NUMBER_LESS_THAN_ZERO_ERROR_MESSAGE = "value (%1$d) must be greater than equal to 0";

  /**
   * Calculates the area of a circle.
   *
   * @param radius a double value indicating the radius of the circle.
   * @return the area of a circle given the radius.
   * @see #ellipseArea(double, double)
   */
  public static double circleArea(double radius) {
    return ellipseArea(radius, radius);
  }

  /**
   * Calculates the circumference of a circle.
   *
   * @param radius a double value indicating the radius of the circle.
   * @return the circumference of a circle given the radius.
   * @see java.lang.Math#PI
   */
  public static double circleCircumference(double radius) {
    return 2.0d * Math.PI * radius;
  }

  /**
   * Calculates the diameter of a circle.
   *
   * @param radius a double value indicating the radius of the circle.
   * @return the diameter of a circle given the radius.
   */
  public static double circleDiameter(double radius) {
    return 2.0d * radius;
  }

  /**
   * Calculates the volume of a cone.
   *
   * @param radius a double value indicating the radius of the cone's base.
   * @param height a double value indicating the height of the cone.
   * @return the volume of a cone given the radius and height.
   * @see java.lang.Math#PI
   * @see java.lang.Math#pow(double, double)
   */
  public static double coneVolume(double radius, double height) {
    return (Math.PI * Math.pow(radius, 2) * height) / 3.0d;
  }

  /**
   * Calculates the surface area of a cube.
   *
   * @param side a double value indicating the side length of the cube.
   * @return the surface area of a cube given a side.
   * @see #squareArea(double)
   */
  public static double cubeSurfaceArea(double side) {
    return 6.0d * squareArea(side);
  }

  /**
   * Calculates the volume of a cube.
   *
   * @param side a double value indicating the length of the cube's side.
   * @return the volume of a cube given the length of a side.
   * @see java.lang.Math#pow(double, double)
   */
  public static double cubeVolume(double side) {
    return Math.pow(side, 3);
  }

  /**
   * Calculates the surface area of a cylinder.
   *
   * @param radius a double value indicating the radius of the cylinder.
   * @param height a double value indicating the height of the cylinder.
   * @return the surface area of a cylinder given the radius and height.
   * @see java.lang.Math#PI
   * @see java.lang.Math#pow(double, double)
   */
  public static double cylinderSurfaceArea(double radius, double height) {
    return (2.0d * Math.PI * Math.pow(radius, 2)) + (2.0d * Math.PI * radius * height);
  }

  /**
   * Calculates the volume of a cylinder.
   *
   * @param radius a double value indicating the radius of the cylinder's end(s).
   * @param height a double value indicating the height of the cylinder.
   * @return the volume of a cylinder given the radius and height.
   * @see java.lang.Math#PI
   * @see java.lang.Math#pow(double, double)
   */
  public static double cylinderVolume(double radius, double height) {
    return Math.PI * Math.pow(radius, 2) * height;
  }

  /**
   * Calculates the area of an ellipse.
   *
   * @param radiusLength a double value indicating the radius length of the ellipse (x axis).
   * @param radiusHeight a double value indicating the radius height of the ellipse (y axis).
   * @return the area of an ellipse given the radius length and height.
   * @see java.lang.Math#PI
   */
  public static double ellipseArea(double radiusLength, double radiusHeight) {
    return Math.PI * radiusLength * radiusHeight;
  }

  /**
   * Calculates the volume of an ellipsoid.
   *
   * @param radiusLength a double value indicating the radius length of the ellipsoid (x axis).
   * @param radiusHeight a double value indicating the radius height of the ellipsoid (y axis).
   * @param radiusWidth a double value indicating the radius width of the ellipsoid (z axis).
   * @return the volume of the ellipsoid given the radius length, height and width.
   * @see java.lang.Math#PI
   */
  public static double ellipsoidVolume(double radiusLength, double radiusHeight, double radiusWidth) {
    return (4.0d * Math.PI * radiusLength * radiusHeight * radiusWidth) / 3.0d;
  }

  /**
   * Calculates the area of a equilateral triangle.
   *
   * @param side a double value indicating the length of the equilateral triangle's side.
   * @return the area of a equilateral triangle given the length of a side.
   * @see java.lang.Math#pow(double, double)
   * @see java.lang.Math#sqrt(double)
   */
  public static double equilateralTriangleArea(double side) {
    return (Math.sqrt(3.0d) / 4.0d) * Math.pow(side, 2);
  }

  /**
   * Calculates the factorial of the given number using an iterative algorithm and BigInteger value type
   * to avoid a StackOverflowException and numeric overflow, respectively.
   *
   * @param value an Integer value used to compute the factorial.
   * @return the factorial of the given number.
   * @throws java.lang.IllegalArgumentException if the number value is null or less than 0.
   * @see java.math.BigInteger
   */
  public static @NotNull BigInteger factorial(@NotNull BigInteger value) {

    Assert.notNull(value, "value must not be null");
    Assert.isTrue(value.compareTo(BigInteger.ZERO) >= 0, NUMBER_LESS_THAN_ZERO_ERROR_MESSAGE, value);

    if (value.compareTo(TWO) <= 0) {
      return value.equals(TWO) ? TWO : BigInteger.ONE;
    }

    BigInteger result = value;
    BigInteger multiplier = value;

    for (multiplier = multiplier.add(NEGATIVE_ONE); multiplier.compareTo(BigInteger.ONE) > 0;
          multiplier = multiplier.add(NEGATIVE_ONE)) {

      result = result.multiply(multiplier);
    }

    return result;
  }

  /**
   * Gets the Fibonacci number at position n in the Fibonacci Sequence.
   *
   * @param n an integer value indicating the nth number in the Fibonacci Sequence.
   * @return the nth number in the Fibonacci Sequence.
   * @throws IllegalArgumentException if the position (n) is less than 1.
   * @see #fibonacciSequence(int)
   */
  public static int fibonacciNumber(int n) {
    return fibonacciSequence(n)[n - 1];
  }

  /**
   * Calculates the Fibonacci Sequence to the nth position.
   *
   * @param n an integer value indicating the position of the nth element in the Fibonacci Sequence.
   * @return an integer array containing n elements of the Fibonacci Sequence.
   * @throws IllegalArgumentException if the position (n) is less than 1.
   */
  public static int[] fibonacciSequence(int n) {

    Assert.argument(n, argument -> argument > 0,
      "The number of elements from the Fibonacci Sequence to calculate must be greater than equal to 0!");

    int[] fibonacciNumbers = new int[n];

    for (int position = 0; position < n; position++) {
      if (position == 0) {
        fibonacciNumbers[position] = 0;
      }
      else if (position < 2) {
        fibonacciNumbers[position] = 1;
      }
      else {
        fibonacciNumbers[position] = (fibonacciNumbers[position - 1] + fibonacciNumbers[position - 2]);
      }
    }

    return fibonacciNumbers;
  }

  /**
   * Calculates the volume of an irregular prism.
   *
   * @param base a double value indicating the prism's base (width).
   * @param height a double value indicating the prism's height.
   * @return the volume of an irregular prism given the base and height.
   */
  public static double irregularPrismVolume(double base, double height) {
    return base * height;
  }

  /**
   * Determines the maximum numerical value in an array of values.
   *
   * @param values an array of numerical values from which to determine the maximum value.
   * @return the maximum numerical value in the array of numerical values.  Returns Double.NaN
   * if the array of values is null.
   * @see java.lang.Double#NaN
   * @see java.lang.Math#max(double, double)
   */
  @NullSafe
  public static double max(double... values) {

    double maxValue = Double.NaN;

    if (values != null) {
      for (double value : values) {
        maxValue = (Double.isNaN(maxValue) ? value : Math.max(maxValue, value));
      }
    }

    return maxValue;
  }

  /**
   * Determines the minimum numerical value in an array of values.
   *
   * @param values an array of numerical values from which to determine the minimum value.
   * @return the minimum numerical value in the array of numerical values.  Returns Double.NaN
   * if the array of values is null.
   * @see java.lang.Double#NaN
   * @see java.lang.Math#min(double, double)
   */
  @NullSafe
  public static double min(double... values) {

    double minValue = Double.NaN;

    if (values != null) {
      for (double value : values) {
        minValue = (Double.isNaN(minValue) ? value : Math.min(minValue, value));
      }
    }

    return minValue;
  }

  /**
   * Multiplies the array of numbers.
   *
   * @param numbers the array of integer values to multiply.
   * @return the array of integer numbers multiplied together.
   */
  @NullSafe
  public static int multiply(int... numbers) {

    int result = 0;

    if (numbers != null) {
      result = (numbers.length > 0 ? 1 : 0);
      for (int number : numbers) {
        result *= number;
      }
    }

    return result;
  }

  /**
   * Calculates the area of a parallelogram.
   *
   * @param base a double value indicating the base length of the parallelogram.
   * @param height a double value indicating the height of the parallelogram.
   * @return the area of a parallelogram given the base length and height.
   * @see #rectangleArea(double, double)
   */
  public static double parallelogramArea(double base, double height) {
    return rectangleArea(base, height);
  }

  /**
   * Calculates the volume of a pyramid.
   *
   * @param base a double value indicating the base length of the pyramid.
   * @param height a double value indicating the height of the pyramid.
   * @return the volume of a pyramid given the base length and height.
   */
  public static double pyramidVolume(double base, double height) {
    return (base * height) / 3.0d;
  }

  /**
   * Calculates the Pythagorean Theorem (c2 = a2 + b2).
   *
   * @param a double value operand.
   * @param b double value operand.
   * @return the value for c using the Pythagorean Theorem
   * @see java.lang.Math#pow(double, double)
   * @see java.lang.Math#sqrt(double)
   */
  public static double pythagoreanTheorem(double a, double b) {
    return Math.sqrt(Math.pow(a, 2) + Math.pow(b, 2));
  }

  /**
   * Calculates the area of a rectangle.
   *
   * @param length a double value indicating the length of the rectangle.
   * @param height a double value indicating the height of the rectangle.
   * @return the area of a rectangle given the length and height.
   */
  public static double rectangleArea(double length, double height) {
    return length * height;
  }

  /**
   * Calculates the surface area of a rectangular prism.
   *
   * @param length a double value indicating the length of the rectangular prism (x axis).
   * @param height a double value indicating the height of the rectangular prism (y axis).
   * @param width a double value indicating the width of the rectangular prism (z axis).
   * @return the surface area of a rectangular prism given the length, height and width of the sides.
   */
  public static double rectangularPrismSurfaceArea(double length, double height, double width) {
    return (2 * length * height) + (2 * length * width) + (2 * height * width);
  }

  /**
   * Calculates the volume of a rectangular prism.
   *
   * @param length a double value indicating the length of the rectangular prism (x axis).
   * @param height a double value indicating the height of the rectangular prism (y axis).
   * @param width a double value indicating the width of the rectangular prism (z axis).
   * @return the volume of a rectangular prism given the length, height and width.
   */
  public static double rectangularPrismVolume(double length, double height, double width) {
    return length * height * width;
  }

  /**
   * Rounds the specified double value to the nearest tenth.
   *
   * @param value the double value to round to the nearest tenth.
   * @return the double value rounded to the nearest tenth.
   * @see #roundToPrecision(double, int)
   */
  public static double roundToNearestTenth(double value) {
    return roundToPrecision(value, 1);
  }

  /**
   * Rounds the {@link Double#TYPE double value} to the nearest, given {@link Integer number of decimal places}.
   *
   * @param value {@link Double#TYPE value} to round.
   * @param numberOfDecimalPlaces {@link Integer value} indicating the number of decimal places;
   * must be greater than {@literal 0}.
   * @return the given {@link Double#TYPE value} rounded to nearest, given {@link Integer number of decimal places}.
   * @see java.lang.Math#round(double)
   */
  public static double roundToPrecision(double value, int numberOfDecimalPlaces) {

    Assert.isTrue(numberOfDecimalPlaces > 0,
      "Number of decimal places [%d] must be greater than 0", numberOfDecimalPlaces);

    double result = value;
    double baseTenExponentialFactor = Math.pow(BASE_TEN, numberOfDecimalPlaces);

    result *= baseTenExponentialFactor;
    result = Math.round(result);
    result /= baseTenExponentialFactor;

    return result;
  }

  /**
   * Calculates the surface area of a sphere.
   *
   * @param radius a double value indicating the radius of the sphere.
   * @return the surface area of a sphere given a radius
   * @see java.lang.Math#PI
   * @see java.lang.Math#pow(double, double)
   */
  public static double sphereSurfaceArea(double radius) {
    return 4.0d * Math.PI * Math.pow(radius, 2);
  }

  /**
   * Calculates the volume of a sphere.
   *
   * @param radius a double value indicating the radius of the sphere.
   * @return the volume of a sphere given the radius.
   * @see java.lang.Math#PI
   * @see java.lang.Math#pow(double, double)
   */
  public static double sphereVolume(double radius) {
    return (4.0d * Math.PI * Math.pow(radius, 3)) / 3.0d;
  }

  /**
   * Calculates the area of a square.
   *
   * @param side a double value indicating the length of the square's side.
   * @return the area of a square given the length of a side.
   * @see #rectangleArea(double, double)
   */
  public static double squareArea(double side) {
    return rectangleArea(side, side);
  }

  /**
   * Calculates the sum of all integer values in the array.
   *
   * @param numbers an array of integer values to sum up.
   * @return the sum of all integer values in the array.
   */
  @NullSafe
  public static int sum(int... numbers) {

    int sum = 0;

    if (numbers != null) {
      for (int number : numbers) {
        sum += number;
      }
    }

    return sum;
  }

  /**
   * Calculates the area of a trapezoid.
   *
   * @param height a double value indicating the height of the trapezoid.
   * @param baseOne a double value indicating the length of one of the trapezoid's bases.
   * @param baseTwo a double value indicating the length of the other trapezoid's bases.
   * @return the area of a trapezoid given the height and length of the 2 bases.
   */
  public static double trapezoidArea(double height, double baseOne, double baseTwo) {
    return (height / 2.0d) * (baseOne + baseTwo);
  }

  /**
   * Calculates the area of a triangle.
   *
   * @param base a double value indicating the length of the triangle's base.
   * @param height a double value indicating the height of one of the triangle's sides.
   * @return the area of a triangle given the base length and height.
   */
  public static double triangleArea(double base, double height) {
    return (base * height) / 2.0d;
  }

  /**
   * Truncates the given {@link Double#TYPE double value} to the given {@link Integer number of decimal places}.
   *
   * @param value {@link Double#TYPE value} to truncate.
   * @param numberOfDecimalPlaces {@link Integer value} indicating the number of decimal places;
   * must be greater than {@literal 0}.
   * @return the given {@link Double#TYPE double value} truncated to the given {@link Integer number of decimal places}.
   * @see java.lang.Math#floor(double)
   */
  public static double truncateToPrecision(double value, int numberOfDecimalPlaces) {

    Assert.isTrue(numberOfDecimalPlaces > 0,
      "Number of decimal places [%d] must be greater than 0", numberOfDecimalPlaces);

    double result = value;
    double baseTenExponentialFactor = Math.pow(BASE_TEN, numberOfDecimalPlaces);

    result *= baseTenExponentialFactor;
    result = Math.floor(result);
    result /= baseTenExponentialFactor;

    return result;
  }
}
