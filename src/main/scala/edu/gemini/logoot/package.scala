package edu.gemini

import scalaz._

package object logoot {
  type Logoot[A] = Free[LogootOp, A]
}
