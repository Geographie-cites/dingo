//package dingo
//
//import org.apache.commons.math3.ode.FirstOrderDifferentialEquations
//import org.apache.commons.math3.ode.nonstiff.ClassicalRungeKuttaIntegrator
//import better.files.*
///*
// * Copyright (C) 2023 Romain Reuillon
// *
// * This program is free software: you can redistribute it and/or modify
// * it under the terms of the GNU Affero General Public License as published by
// * the Free Software Foundation, either version 3 of the License, or
// * (at your option) any later version.
// *
// * This program is distributed in the hope that it will be useful,
// * but WITHOUT ANY WARRANTY; without even the implied warranty of
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// * GNU Affero General Public License for more details.
// *
// * You should have received a copy of the GNU Affero General Public License
// * along with this program.  If not, see <http://www.gnu.org/licenses/>.
// */
//
//object stock:
//  object Stock:
//    type Stock = IArray[Double]
//    case class DynamicEquation(equations: (Array[Double], Double) => IArray[Double], dimension: Int)
//
//    def dimension = 3
//
//    def sir(beta: Double, gama: Double): DynamicEquation =
//      val eq = DynamicEquation(
//        equations =
//          (state, t) =>
//            val betaisn = beta * state(1) * state(0) / state.sum
//            val gamai = gama * state(1)
//            IArray(-betaisn, betaisn - gamai, gamai),
//         dimension = dimension)
//      assert(eq.dimension == dimension)
//      eq
//
//
//    def read(population: File, infected: Double): IArray[IArray[Double]] =
//      val populationValue =
//        population.lines.drop(1).map: l =>
//          val c = l.split(",")
//          c(0).toInt -> c(1).toDouble
//        .toMap
//
//      val maxId = populationValue.keys.max
//
//      IArray.tabulate(maxId + 1, Stock.dimension): (i, s) =>
//        val pop = populationValue.getOrElse(i, 0.0)
//        s match
//          case 0 => pop * 1 - infected
//          case 1 => pop * infected
//          case _ => 0.0
//
//  object Integration:
//    def apply(equations: Stock.DynamicEquation): Integration = new Integration(equations)
//
//  class Integration(equations: Stock.DynamicEquation) extends FirstOrderDifferentialEquations:
//
//    def integrate(y0: IArray[Double], integrationStep: Double, step: Double) =
//      val integrator = new ClassicalRungeKuttaIntegrator(integrationStep)
//      val y = new Array[Double](equations.dimension)
//      integrator.integrate(this, 0, y0.asInstanceOf[Array[Double]], step, y)
//      if y.exists(_.isNaN) then throw new RuntimeException(s"""Dynamic from ${y0.toVector} at step ${step} using integration step ${integrationStep} produces NaN: ${y.toVector}"""")
//      y.asInstanceOf[IArray[Double]]
//
//    override def computeDerivatives(t: Double, y: Array[Double], yDot: Array[Double]): Unit =
//      val derivativeValue = equations.equations(y, t)
//      var i = 0
//      while (i < derivativeValue.size)
//        yDot(i) = derivativeValue(i)
//        i += 1
//
//
//    override def getDimension: Int = equations.dimension
//
