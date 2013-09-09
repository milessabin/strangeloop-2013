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

import shapeless._, ops.hlist._, tag._
import scalaz.Applicative
import scalaz.syntax.applicative._

object EffDemo extends App {
  def typed[T](t: => T) {}

  trait Rep[L <: HList, U, V] {
    type Out <: HList
    def apply(l: L, v: V): Out
  }

  object Rep {
    type Aux[L <: HList, U, V, Out0 <: HList] = Rep[L, U, V] { type Out = Out0 }
    implicit def rep[L <: HList, U, V, Out0 <: HList](implicit rep: Replacer.Aux[L, U, V, (U, Out0)]): Aux[L, U, V, Out0] =
      new Rep[L, U, V] {
        type Out = Out0
        def apply(l: L, v: V): Out = rep(l, v)._2
      }
  }

  trait Effect {
    type Res
    type ResO
    type T

    def handle[M[_], A](r: Res)(k: (ResO, T) => M[A]): M[A]
  }

  trait Handler[E <: Effect, M[_]] {
    def handle[A](eff: E)(r: eff.Res)(k: (eff.ResO, eff.T) => M[A]): M[A]
  }

  sealed trait State extends Effect
  case class Get[A]() extends State {
    type Res = A
    type ResO = A
    type T = A
    def handle[M[_], X](r: A)(k: (A, A) => M[X]): M[X] = k(r, r)
  }

  case class Put[A, B](b: B) extends State {
    type Res = A
    type ResO = B
    type T = Unit
    def handle[M[_], X](r: A)(k: (B, Unit) => M[X]): M[X] = k(b, ())
  }

  object State {
    implicit def handleState[M[_]] = new Handler[State, M] {
      def handle[A](eff: State)(r: eff.Res)(k: (eff.ResO, eff.T) => M[A]): M[A] =  eff.handle[M, A](r)(k)
    }
  }

  trait CEffect
  case class MkEff[T, E <: Effect](res: T) extends CEffect

  type CState[T] =  MkEff[T, State]
  def CState[T](t: T) = MkEff[T, State](t)

  trait EffM[M[_], ES <: HList, ESO <: HList, A] { outer =>
    def run(es: ES)(implicit M : Applicative[M]): M[A] =
      effInt(es) { (env, r) => r.pure[M] }

    def runPureEnv(es: ES)(implicit ev: M[A] =:= A, M : Applicative[M]): M[(ESO, A)] =
      effInt(es) { (env, r) => (env, r).pure[M] }

    def effInt[B](es: ES)(k: (ESO, A) => M[B]): M[B]

    def map[B](f: A => B): EffM[M, ES, ESO, B] = new EffM[M, ES, ESO, B] {
      def effInt[C](es: ES)(k: (ESO, B) => M[C]): M[C] =
        outer.effInt(es) { (eso, a) => k(eso, f(a)) }
    }

    def flatMap[ESOO <: HList, B](f: A => EffM[M, ESO, ESOO, B]): EffM[M, ES, ESOO, B] = new EffM[M, ES, ESOO, B] {
      def effInt[C](es: ES)(k: (ESOO, B) => M[C]): M[C] =
        outer.effInt(es) { (eso, a) => f(a).effInt(eso) { (esoo, b) => k(esoo, b) } }
    }
  }

  type Eff[M[_], ES <: HList, A] = EffM[M, ES, ES, A]

  case class EffElem[E <: Effect, Res, ResO, ES <: HList, ESO <: HList](
    sel: Selector[ES, MkEff[Res, E]],
    rep: Rep.Aux[ES, MkEff[Res, E], MkEff[ResO, E], ESO]
  ) 

  object EffElem {
    implicit def mkEffElem[E <: Effect, Res, ResO, ES <: HList, ESO <: HList]
      (implicit
        sel: Selector[ES, MkEff[Res, E]],
        rep: Rep.Aux[ES, MkEff[Res, E], MkEff[ResO, E], ESO]
      ): EffElem[E, Res, ResO, ES, ESO] =
        EffElem[E, Res, ResO, ES, ESO](sel, rep)
  }

  def mkEffect[E <: Effect, M[_], ES <: HList, ESO <: HList](eff: E)
    (implicit
      prf:  EffElem[E, eff.Res, eff.ResO, ES, ESO],
      hdlr: Handler[E, M]
    ): EffM[M, ES, ESO, eff.T] = new EffM[M, ES, ESO, eff.T] {
      def effInt[B](es: ES)(k: (ESO, eff.T) => M[B]): M[B] = {
        val ceff: MkEff[eff.Res, E] = prf.sel(es)
        val res: eff.Res = ceff.res
        hdlr.handle(eff)(res) { (reso, t) =>
          val eso: ESO = prf.rep(es, MkEff(reso))
          k(eso, t)
        }
      }
    }

  def get[M[_], X, ES <: HList](implicit prf: EffElem[State, X, X, ES, ES]): EffM[M, ES, ES, X] =
    mkEffect[State, M, ES, ES](Get[X]())

  def put[M[_], X, ES <: HList](x: X)(implicit prf: EffElem[State, X, X, ES, ES]): EffM[M, ES, ES, Unit] =
    mkEffect[State, M, ES, ES](Put[X, X](x))

  def update[M[_], X, ES <: HList](f: X => X)(implicit prf: EffElem[State, X, X, ES, ES]): EffM[M, ES, ES, Unit] =
    for {
      v <- get
      u <- put(f(v))
    } yield u

  sealed trait Tree[+T]
  case object Leaf extends Tree[Nothing]
  case class Node[T](left: Tree[T], value: T, right: Tree[T]) extends Tree[T]

  trait Tag
  trait Count

  type RES = CState[Int @@ Tag] :: CState[Int @@ Count] :: HNil

  def tagCount[M[_], T](t: Tree[T]): EffM[M, RES, RES, Tree[(Int, T)]] = {
    t match {
      case Leaf =>
        for {
          _ <- update[M, Int @@ Count, RES] { (n : Int) => tag[Count](n+1) }
          _ <- get[M, Int @@ Tag, RES]
        } yield Leaf
      case Node(l, x, r) =>
        for {
          l1  <- tagCount(l)
          lbl <- get[M, Int @@ Tag, RES]
          _   <- put[M, Int @@ Tag, RES](tag[Tag](lbl+1))
          r1  <- tagCount(r)
        } yield Node(l1, (lbl, x), r1)
    }
  }

  val tree = Node(Node(Node(Leaf, "A", Node(Leaf, "B", Leaf)), "C", Leaf), "D", Node(Leaf, "E", Leaf))

  val run = tagCount[Id, String](tree).runPureEnv(CState(tag[Tag](0)) :: CState(tag[Count](0)) :: HNil)
  typed[(RES, Tree[(Int, String)])](run)

  val (_ :: MkEff(leafCount) :: HNil, taggedTree) = run
  println(s"Leaves: $leafCount")
  println(taggedTree)
}
