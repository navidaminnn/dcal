package distcompiler

class FuzzLib:
  def fillerFunction(flag: Boolean, value: Int): Int =
    if flag then
      if value > 100 then
        value * 2
      else if value > 50 then
        value + 10
      else if value > 0 then
        value
      else if value > -50 then
        value - 5
      else
        value / 2
    else
      throw new RuntimeException("Found a failure case")
