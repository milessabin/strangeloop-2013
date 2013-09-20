/*
 * Copyright (c) 2011-13 Miles Sabin 
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

package strangeloop

import scala.language.{ higherKinds, implicitConversions }

import scala.Numeric.Implicits._

import shapeless._, nat._, test.illTyped

object VectDemo extends App { outer =>
  def typed[T](t: => T) {}

  sealed trait Vect[+A, N <: Nat] { xs =>
    def :#:[B >: A](x: B) = outer.:#:(x, xs)
  }

  case class :#:[+A, N <: Nat](head: A, tail: Vect[A, N]) extends Vect[A, Succ[N]]

  type VNil = Vect[Nothing, _0]
  case object VNil extends VNil

  object Vect {
    implicit def narrow[A, N <: Nat](v: Vect[A, Succ[N]]): A :#: N = v match {
      case vc @ (hd :#: tl) => vc
    }

    implicit class VectOps[A, M <: Nat](xs: Vect[A, M]) {
      def ++[N <: Nat](ys: Vect[A, N])(implicit concat: VConcat[A, Vect[A, M], Vect[A, N]]): concat.Out = concat(xs, ys)
      
      def vAdd(ys: Vect[A, M])(implicit add: VAdd[A, M]): Vect[A, M] = add(xs, ys)

      def zipWith[B, C](ys: Vect[B, M])(f: (A, B) => C)(implicit zip: VZipWith[A, B, C, M]): Vect[C, M] = zip(xs, ys, f)
    }
  }

  trait VConcat[A, XS, YS] {
    type MN <: Nat
    type Out = Vect[A, MN]
    def apply(xs: XS, ys: YS): Out
  }

  object VConcat {
    implicit def vnilConcat[A, N <: Nat] = new VConcat[A, Vect[A, _0], Vect[A, N]] {
      type MN = N
      def apply(xs: Vect[A, _0], ys: Vect[A, N]): Out = ys
    }

    implicit def vconsConcat[A, M <: Nat, N <: Nat](implicit ctail: VConcat[A, Vect[A, M], Vect[A, N]]) =
      new VConcat[A, Vect[A, Succ[M]], Vect[A, N]] {
        type MN = Succ[ctail.MN]
        def apply(xs: Vect[A, Succ[M]], ys: Vect[A, N]): Out = xs.head :#: (xs.tail ++ ys)
      }
  }

  trait VAdd[A, N <: Nat] {
    def apply(xs: Vect[A, N], ys: Vect[A, N]): Vect[A, N]
  }

  object VAdd {
    implicit def vnilAdd[A: Numeric] = new VAdd[A, _0] {
      def apply(xs: Vect[A, _0], ys: Vect[A, _0]): Vect[A, _0] = VNil
    }

    implicit def vconsAdd[A: Numeric, N <: Nat](implicit atail: VAdd[A, N]) = new VAdd[A, Succ[N]] {
      def apply(xs: Vect[A, Succ[N]], ys: Vect[A, Succ[N]]): Vect[A, Succ[N]] = (xs.head+ys.head) :#: (xs.tail vAdd ys.tail)
    }
  }

  trait VZipWith[A, B, C, N <: Nat] {
    def apply(xs: Vect[A, N], ys: Vect[B, N], f: (A, B) => C): Vect[C, N]
  }

  object VZipWith {
    implicit def vnilZipWith[A, B, C] = new VZipWith[A, B, C, _0] {
      def apply(xs: Vect[A, _0], ys: Vect[B, _0], f: (A, B) => C): Vect[C, _0] = VNil
    }

    implicit def vconsZipWith[A, B, C, N <: Nat](implicit ztail: VZipWith[A, B, C, N]) = new VZipWith[A, B, C, Succ[N]] {
      def apply(xs: Vect[A, Succ[N]], ys: Vect[B, Succ[N]], f: (A, B) => C): Vect[C, Succ[N]] =
        f(xs.head, ys.head) :#: (xs.tail zipWith ys.tail)(f)
    }
  }

  val l1 = 1 :#: 2 :#: 3 :#: VNil
  typed[Vect[Int, _3]](l1)

  val h = l1.head
  val t = l1.tail

  illTyped("""
    l1.tail.tail.tail.head
  """)  

  val l2 = 4 :#: 5 :#: VNil
  typed[Vect[Int, _2]](l2)

  val l3 = l1 ++ l2
  typed[Vect[Int, _5]](l3)

  val l4 = 3 :#: 5 :#: 7 :#: VNil
  val a1 = l1 vAdd l4
  typed[Vect[Int, _3]](a1)

  illTyped("""
    val a2 = l1 vAdd l2
  """)

  val l5 = "foo" :#: "bar" :#: "baz" :#: VNil
  val z1 = (l1 zipWith l5)((_, _))
  typed[Vect[(Int, String), _3]](z1)

  val l6 = "foo" :#: "bar" :#: VNil
  illTyped("""
    val z2 = (l1 zipWith l6)((_, _))
  """)
}
