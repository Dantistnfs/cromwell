
package cromwell.backend.impl.aws

import com.typesafe.config.ConfigFactory
import cromwell.core.TestKitSuite
import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import wom.RuntimeAttributesKeys.awsBatchSecretsKey
import wom.values.{WomArray, WomMap, WomString}

import scala.util.Try


class AwsBatchSecretsSpec extends TestKitSuite with AnyFlatSpecLike with Matchers with PrivateMethodTester {

  val testConfig = ConfigFactory.parseString(
    """
      | awsBatchSecrets = [
      |   {"name": "env", "valueFrom": "someArn"},
      |   {"name": "env2", "valueFrom": "someArn2"}
      | ]
      |""".stripMargin
  )

  val expectedVal =  WomArray(Vector(
    WomMap(Map(
      WomString("name") -> WomString("env"),
      WomString("valueFrom") -> WomString("someArn"),
    )),
    WomMap(Map(
      WomString("name") -> WomString("env2"),
      WomString("valueFrom") -> WomString("someArn2"),
    ))
  ))

  behavior of  "AwsBatchSecrets"
  it should "Create new aws batch secrets object" in {
    val secrets = AwsBatchSecrets("envName", "valueFromSomewhere")
    secrets.valueFrom should equal("valueFromSomewhere")
    secrets.name should equal("envName")
  }

  it should "Validate from config" in {
    val value = AwsBatchSecretsValidation.fromConfig(Some(testConfig))
    value.get should equal(expectedVal)
  }

  it should "return empty value if not set in config" in {
    val config = ConfigFactory.parseString("")
    val value = AwsBatchSecretsValidation.fromConfig(Some(config))
    value should equal(None)
  }

  it should "validate if required keys are present" in {
    AwsBatchSecretsValidation.validate(
      Map(awsBatchSecretsKey -> expectedVal )
    ).isValid should be(true)
  }

  it should "fail is there are missing keys" in {
    val validated = Try(AwsBatchSecretsValidation.validate(
      Map(awsBatchSecretsKey -> WomArray(Seq(WomMap(Map(WomString("name") -> WomString("env"))))))
    ))
    validated.isFailure should be(true)
  }

  it should "validate empty" in {
    val results = AwsBatchSecretsValidation.validate(
      Map(awsBatchSecretsKey -> WomArray.empty)
    )
    results.isValid should be (true)
  }
}
