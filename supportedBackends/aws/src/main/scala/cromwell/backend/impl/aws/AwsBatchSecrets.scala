package cromwell.backend.impl.aws

import cats.syntax.validated._
import com.typesafe.config.Config
import common.validation.ErrorOr.ErrorOr
import cromwell.backend.validation.{BadDefaultAttribute, RuntimeAttributesValidation}
import wom.RuntimeAttributesKeys.awsBatchSecretsKey
import wom.types.{WomMapType, WomMaybeEmptyArrayType, WomNothingType, WomStringType, WomType}
import wom.values.{WomArray, WomMap, WomString, WomValue}

import scala.jdk.CollectionConverters._


case class AwsBatchSecrets(
                          name: String,
                          valueFrom: String
                          ) {
}


object AwsBatchSecretsValidation extends RuntimeAttributesValidation[Vector[AwsBatchSecrets]] {

  override def key: String = awsBatchSecretsKey

  override protected def validateValue: PartialFunction[WomValue, ErrorOr[Vector[AwsBatchSecrets]]] = {
    case WomArray(womType, values)  if womType.memberType == WomMapType(WomStringType, WomStringType) =>
      values.toVector.map {
        case WomMap(_, value) =>
          checkKeys(value)
          AwsBatchSecrets(value(WomString("name")).valueString, value(WomString("valueFrom")).valueString)
      }.validNel
    case WomArray(_, values) if values.isEmpty => Vector.empty.valid
  }

  def checkKeys(values: Map[WomValue, WomValue]): ErrorOr[Map[String, String]] = {
    if (values.keySet.equals(Set("name", "valueFrom"))) {
      values.collect { case (WomString(k), WomString(v)) => (k, v) }.validNel
    } else {
      s"incorrect ".invalidNel
    }
  }


  def fromConfig(runtimeConfig: Option[Config]): Option[WomValue] = {
    runtimeConfig.get match {
      case conf if conf.hasPath(awsBatchSecretsKey) =>
        val config = conf.getObjectList(awsBatchSecretsKey).asScala.map {
          _.unwrapped().asScala.toMap}.toList
         val coerced = coercion collectFirst {
          case womType: WomType if womType.coerceRawValue(config).isSuccess => womType.coerceRawValue(config).get
        } getOrElse {
           BadDefaultAttribute(WomString(config.toString))
         }
        Some(coerced)
      case _ => None
    }
  }

  override def coercion: Iterable[WomType] = {
    Set(WomStringType, WomMaybeEmptyArrayType(WomNothingType), WomMaybeEmptyArrayType(WomMapType(WomStringType, WomStringType)))
  }
}