import io.softwarechain.cryptojournal.domain.model.{Currency, FungibleData}

val f1 = FungibleData(
  BigDecimal("432156304.43068674"),
  Currency.unsafeFrom("FOOFIGHT")
)
val f2 = FungibleData(
  BigDecimal("-432156304.4306868"),
  Currency.unsafeFrom("FOOFIGHT")
)

BigDecimal("432156304.43068674") - BigDecimal("432156304.4306868")

f1.add(f2.amount)
f1.subtract(f2.amount)



