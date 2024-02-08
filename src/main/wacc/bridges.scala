package wacc

import parsley.generic
import parsley.Parsley
import parsley.ap._
import parsley.position.pos

object bridges {

    trait ParserSingletonBridgePos[+A] extends generic.ErrorBridge {
        protected def con(pos: (Int, Int) = (0, 0)): A
        def from(op: Parsley[_]): Parsley[A] = error(pos.map(this.con(_)) <~ op)
    }

    trait ParserBridge0[+A] extends ParserSingletonBridgePos[A] {
        def apply()(pos: (Int, Int) = (0, 0)): A
        override def from(op: Parsley[_]): Parsley[A] = error(pos.map[A](this.apply()(_)) <~ op)
        override final def con(pos: (Int, Int)): A = this.apply()(pos)
        def default: A = this.apply()()
    }


    trait ParserBridge1[-A, +B] extends ParserSingletonBridgePos[A => B] {
        def apply(x: A)(pos: (Int, Int) = (0, 0)): B
        def apply(x: Parsley[A]): Parsley[B] = error(ap1(pos.map(con), x))
        override final def con(pos: (Int, Int) = (0, 0)): A => B = this.apply(_)(pos)
        def default(x: A): B = this.apply(x)()
    }

    trait ParserBridge2[-A, -B, +C] extends ParserSingletonBridgePos[(A, B) => C] {
        def apply(x: A, y: B)(pos: (Int, Int) = (0, 0)): C
        def apply(x: Parsley[A], y: =>Parsley[B]): Parsley[C] = error(ap2(pos.map(con), x, y))
        override final def con(pos: (Int, Int) = (0, 0)): (A, B) => C = this.apply(_, _)(pos)
        def default(x: A, y: B): C = this.apply(x, y)()
    }

    trait ParserBridge3[-A, -B, -C, +D] extends ParserSingletonBridgePos[(A, B, C) => D] {
        def apply(x: A, y: B, z: C)(pos: (Int, Int) = (0, 0)): D
        def apply(x: Parsley[A], y: => Parsley[B], z: => Parsley[C]): Parsley[D]
            = error(ap3(pos.map(con), x, y, z))
        override final def con(pos: (Int, Int) = (0, 0)): (A, B, C) => D = this.apply(_, _, _)(pos)
        def default(x: A, y: B, z: C): D = this.apply(x, y, z)()
    }

    trait ParserBridge4[-A, -B, -C, -D, +E] extends ParserSingletonBridgePos[(A, B, C, D) => E] {
        def apply(x: A, y: B, z: C, w: D)(pos: (Int, Int) = (0, 0)): E
        def apply(x: Parsley[A], y: => Parsley[B], z: => Parsley[C], w: => Parsley[D]): Parsley[E] 
            = error(ap4(pos.map(con), x, y, z, w))
        override final def con(pos: (Int, Int) = (0, 0)): (A, B, C, D) => E = this.apply(_, _, _, _)(pos)
        def default(x: A, y: B, z: C, w: D): E = this.apply(x, y, z, w)()
    }

}