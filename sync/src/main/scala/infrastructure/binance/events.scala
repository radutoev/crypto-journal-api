package io.softwarechain.cryptojournal
package infrastructure.binance

import org.web3j.abi.datatypes.generated.{ Uint112, Uint256 }
import org.web3j.abi.datatypes.{ Address, Event }
import org.web3j.abi.{ EventEncoder, TypeReference }

import java.util

object events {
  //  Approval (index_topic_1 address owner, index_topic_2 address spender, uint256 value)
  val ApprovalEvent: Event = new Event(
    "Approval",
    util.Arrays.asList(
      new TypeReference[Address](true)  {},
      new TypeReference[Address](true)  {},
      new TypeReference[Uint256](false) {}
    )
  )
  val ApprovalEventHash = EventEncoder.encode(ApprovalEvent)

  //Deposit (index_topic_1 address dst, uint256 wad)
  val DepositEvent: Event = new Event(
    "Deposit",
    util.Arrays.asList(new TypeReference[Address](true) {}, new TypeReference[Uint256](false) {})
  )
  val DepositEventHash = EventEncoder.encode(DepositEvent)

  //Mint (index_topic_1 address sender, uint256 amount0, uint256 amount1)
  val MintEvent: Event = new Event(
    "Mint",
    util.Arrays.asList(
      new TypeReference[Address](true)  {},
      new TypeReference[Uint256](false) {},
      new TypeReference[Uint256](false) {}
    )
  )
  val MintEventHash = EventEncoder.encode(MintEvent)

  //Transfer (index_topic_1 address from, index_topic_2 address to, uint256 value)
  val TransferEvent = new Event(
    "Transfer",
    util.Arrays.asList(
      new TypeReference[Address](true)  {},
      new TypeReference[Address](true)  {},
      new TypeReference[Uint256](false) {}
    )
  )
  val TransferEventHash = EventEncoder.encode(TransferEvent)

  //Swap (index_topic_1 address sender, uint256 amount0In, uint256 amount1In, uint256 amount0Out, uint256 amount1Out, index_topic_2 address to)
  val SwapEvent = new Event(
    "Swap",
    util.Arrays.asList(
      new TypeReference[Address](true)  {},
      new TypeReference[Uint256](false) {},
      new TypeReference[Uint256](false) {},
      new TypeReference[Uint256](false) {},
      new TypeReference[Uint256](false) {},
      new TypeReference[Address](true)  {}
    )
  )
  val SwapEventHash = EventEncoder.encode(SwapEvent)

  //  Sync (uint112 reserve0, uint112 reserve1)
  val SyncEvent = new Event(
    "Sync",
    util.Arrays.asList(
      new TypeReference[Uint112](false) {},
      new TypeReference[Uint112](false) {}
    )
  )
  val SyncEventHash = EventEncoder.encode(SyncEvent)
}
