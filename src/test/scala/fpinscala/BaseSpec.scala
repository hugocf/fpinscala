package fpinscala

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, OptionValues, WordSpec}

trait BaseSpec extends WordSpec with PropertyChecks with Matchers with OptionValues
