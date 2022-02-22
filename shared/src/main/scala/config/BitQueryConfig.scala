package io.softwarechain.cryptojournal
package config

import scala.util.Random

final case class BitQueryConfig(url: String, rawApiKeys: String) {
  val apiKeys: Set[String] = rawApiKeys.split(",").toSet

  def getKey(exclude: Set[String] = Set.empty): String = {
    val availableKeys = apiKeys.diff(exclude).toList
    if(availableKeys.nonEmpty) {
      availableKeys(Random.nextInt(availableKeys.length))
    } else {
      apiKeys.toList(Random.nextInt(apiKeys.size))
    }
  }
}
